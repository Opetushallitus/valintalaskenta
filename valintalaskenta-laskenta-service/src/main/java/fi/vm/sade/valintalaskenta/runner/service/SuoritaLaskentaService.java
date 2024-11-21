package fi.vm.sade.valintalaskenta.runner.service;

import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintatapajonoJarjestyskriteereillaDTO;
import fi.vm.sade.valinta.sharedutils.ValintaperusteetOperation;
import fi.vm.sade.valintalaskenta.audit.AuditLogUtil;
import fi.vm.sade.valintalaskenta.audit.AuditSession;
import fi.vm.sade.valintalaskenta.domain.dto.LaskeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaDto;
import fi.vm.sade.valintalaskenta.laskenta.resource.ValintalaskentaResourceImpl;
import fi.vm.sade.valintalaskenta.runner.resource.external.koostepalvelu.KoostepalveluAsyncResource;
import fi.vm.sade.valintalaskenta.runner.resource.external.valintaperusteet.ValintaperusteetAsyncResource;
import java.time.Duration;
import java.time.Instant;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executor;
import java.util.function.Function;
import java.util.stream.Collectors;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import software.amazon.awssdk.services.cloudwatch.CloudWatchClient;
import software.amazon.awssdk.services.cloudwatch.model.Dimension;
import software.amazon.awssdk.services.cloudwatch.model.MetricDatum;
import software.amazon.awssdk.services.cloudwatch.model.PutMetricDataRequest;
import software.amazon.awssdk.services.cloudwatch.model.StandardUnit;

@Service
public class SuoritaLaskentaService {
  private static final Logger LOG = LoggerFactory.getLogger(SuoritaLaskentaService.class);

  private final ValintalaskentaResourceImpl valintalaskentaResource;
  private final KoostepalveluAsyncResource koostepalveluAsyncResource;
  private final ValintaperusteetAsyncResource valintaperusteetAsyncResource;
  private final Executor executor;
  private final CloudWatchClient cloudWatchClient;

  private final String environmentName;

  private static final String LAHTOTIEDOT = "lahtotiedot";
  private static final String LASKENTA = "laskenta";

  private enum LaskentaTulos {
    VALMIS,
    OHITETTU,
    VIRHE
  }

  @Autowired
  public SuoritaLaskentaService(
      ValintalaskentaResourceImpl valintalaskentaResource,
      KoostepalveluAsyncResource koostepalveluAsyncResource,
      ValintaperusteetAsyncResource valintaperusteetAsyncResource,
      CloudWatchClient cloudWatchClient,
      @Value("${environment.name}") String environmentName,
      @Qualifier("ValintalaskentaExecutor") Executor executor) {
    this.valintalaskentaResource = valintalaskentaResource;
    this.koostepalveluAsyncResource = koostepalveluAsyncResource;
    this.valintaperusteetAsyncResource = valintaperusteetAsyncResource;
    this.cloudWatchClient = cloudWatchClient;
    this.environmentName = environmentName;
    this.executor = executor;
  }

  private static AuditSession laskentaAuditSession(LaskentaDto laskenta) {
    final String userAgent = "-";
    final String inetAddress = "127.0.0.1";
    AuditSession auditSession =
        new AuditSession(laskenta.getUserOID(), Collections.emptyList(), userAgent, inetAddress);
    auditSession.setPersonOid(laskenta.getUserOID());
    return auditSession;
  }

  private boolean isValintalaskentaKaytossa(
      LaskentaDto laskenta, Collection<String> hakukohdeOids) {
    List<ValintaperusteetDTO> valintaperusteet =
        valintaperusteetAsyncResource
            .haeValintaperusteet(hakukohdeOids.iterator().next(), laskenta.getValinnanvaihe())
            .join();
    boolean jokinValintatapajonoKayttaaValintalaskentaa =
        valintaperusteet.stream()
            .map(ValintaperusteetDTO::getValinnanVaihe)
            .flatMap(v -> v.getValintatapajono().stream())
            .anyMatch(ValintatapajonoJarjestyskriteereillaDTO::getKaytetaanValintalaskentaa);

    return jokinValintatapajonoKayttaaValintalaskentaa;
  }

  private void tallennaJaLokitaMetriikat(
      Collection<String> hakukohdeOids,
      Map<String, Duration> durations,
      LaskentaTulos laskentaTulos) {
    Collection<MetricDatum> datums = new ArrayList<>();
    datums.addAll(
        durations.entrySet().stream()
            .map(
                e ->
                    MetricDatum.builder()
                        .metricName("kesto")
                        .value((double) e.getValue().toMillis())
                        .storageResolution(60)
                        .dimensions(
                            List.of(Dimension.builder().name("vaihe").value(e.getKey()).build()))
                        .timestamp(Instant.now())
                        .unit(StandardUnit.MILLISECONDS)
                        .build())
            .collect(Collectors.toList()));

    datums.add(
        MetricDatum.builder()
            .metricName(
                switch (laskentaTulos) {
                  case VALMIS -> "valmiit";
                  case OHITETTU -> "ohitetut";
                  case VIRHE -> "virheet";
                })
            .value((double) hakukohdeOids.size())
            .storageResolution(60)
            .timestamp(Instant.now())
            .unit(StandardUnit.COUNT)
            .build());

    this.cloudWatchClient.putMetricData(
        PutMetricDataRequest.builder()
            .namespace(this.environmentName + "-valintalaskenta")
            .metricData(datums)
            .build());

    LOG.info(
        "Kesto: Hakukohteet: {}, {}",
        hakukohdeOids.stream().collect(Collectors.joining(",")),
        durations.entrySet().stream()
            .map(e -> e.getKey() + ": " + e.getValue().toMillis() + "ms")
            .collect(Collectors.joining(",")));
  }

  public CompletableFuture<String> suoritaLaskentaHakukohteille(
      LaskentaDto laskenta, Collection<String> hakukohdeOids) {
    if (laskenta
        .getTyyppi()
        .equals(fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaTyyppi.VALINTARYHMA)) {
      return this.suoritaValintaryhmaLaskenta(laskenta, hakukohdeOids)
          .exceptionallyAsync(
              e -> {
                this.tallennaJaLokitaMetriikat(
                    hakukohdeOids, Collections.emptyMap(), LaskentaTulos.VIRHE);
                throw new RuntimeException(e);
              });
    }

    if (hakukohdeOids.size() != 1) {
      throw new RuntimeException(
          "Hakukohteita on "
              + hakukohdeOids.size()
              + ". Muissa kuin valintaryhmälaskennassa hakukohteita täytyy olla tasan yksi!");
    }

    if (!this.isValintalaskentaKaytossa(laskenta, hakukohdeOids)) {
      this.tallennaJaLokitaMetriikat(hakukohdeOids, Collections.emptyMap(), LaskentaTulos.OHITETTU);
      return CompletableFuture.completedFuture(laskenta.getUuid());
    }

    return this.suoritaLaskentaHakukohteelle(laskenta, hakukohdeOids.iterator().next())
        .exceptionallyAsync(
            e -> {
              this.tallennaJaLokitaMetriikat(
                  hakukohdeOids, Collections.emptyMap(), LaskentaTulos.VIRHE);
              throw new RuntimeException(e);
            });
  }

  private CompletableFuture<String> suoritaValintaryhmaLaskenta(
      LaskentaDto laskenta, Collection<String> hakukohdeOids) {
    String hakukohteidenNimi =
        String.format("Valintaryhmälaskenta %s hakukohteella", hakukohdeOids.size());
    LOG.info(
        "Muodostetaan VALINTARYHMALASKENTA (Uuid={}) {}", laskenta.getUuid(), hakukohteidenNimi);
    AuditLogUtil.auditLogLaskenta(
        laskentaAuditSession(laskenta),
        ValintaperusteetOperation.LASKENTATOTEUTUS_KAYNNISTYS,
        laskenta.getUuid(),
        laskenta.getHakuOid(),
        hakukohdeOids,
        Optional.of("VALINTARYHMALASKENTA"));

    return CompletableFuture.supplyAsync(
            () ->
                hakukohdeOids.stream()
                    .map(
                        hakukohdeOid ->
                            this.koostepalveluAsyncResource
                                .haeLahtotiedot(laskenta, hakukohdeOid, true, true)
                                .join())
                    .toList())
        .thenApplyAsync(
            laskeDTOs -> {
              /*
               * Tiksussa b15df0500 Merge remote-tracking branch
               * 'origin/VTKU-181__valintaryhmalaskennan_kutsu_pienempiin_paloihin' tämän kutsun siirto
               * valintalaskentakoostepalvelusta valintalaskentaan oli palasteltu moneen kutsuun. Kun kutsu ei
               * enää mene verkon yli ei (käsittääkseni) ole enää mitää syytä palastella joten palatta takaisin
               * yhteen kutsuun.
               */
              String result =
                  valintalaskentaResource.valintaryhmaLaskenta(laskenta.getUuid(), laskeDTOs);
              this.tallennaJaLokitaMetriikat(
                  hakukohdeOids, Collections.emptyMap(), LaskentaTulos.VALMIS);
              return result;
            },
            this.executor);
  }

  private interface HaeTiedotJaLaske {

    CompletableFuture<String> suorita(
        String tyyppi,
        boolean retryHakemuksetJaOppijat,
        boolean withHakijaRyhmat,
        Function<LaskeDTO, String> laske);
  }

  private CompletableFuture<String> suoritaLaskentaHakukohteelle(
      LaskentaDto laskenta, String hakukohdeOid) {

    HaeTiedotJaLaske haeTiedotJaLaske =
        (tyyppi, retryHakemuksetJaOppijat, withHakijaRymat, laske) -> {
          LOG.info("Muodostetaan {} (Uuid={}) {}", tyyppi, laskenta.getUuid(), hakukohdeOid);
          AuditLogUtil.auditLogLaskenta(
              laskentaAuditSession(laskenta),
              ValintaperusteetOperation.LASKENTATOTEUTUS_KAYNNISTYS,
              laskenta.getUuid(),
              laskenta.getHakuOid(),
              Collections.singleton(hakukohdeOid),
              Optional.of(tyyppi));
          Instant lahtotiedotStart = Instant.now();
          return this.koostepalveluAsyncResource
              .haeLahtotiedot(laskenta, hakukohdeOid, retryHakemuksetJaOppijat, withHakijaRymat)
              .thenApplyAsync(
                  laskeDTO -> {
                    Instant laskeStart = Instant.now();

                    LOG.info(
                        "Haettiin lähtötiedot hakukohteelle "
                            + hakukohdeOid
                            + ", start: "
                            + lahtotiedotStart
                            + ", end: "
                            + laskeStart);

                    String result = laske.apply(laskeDTO);
                    this.tallennaJaLokitaMetriikat(
                        Collections.singleton(hakukohdeOid),
                        Map.of(
                            LAHTOTIEDOT,
                            Duration.between(lahtotiedotStart, laskeStart),
                            LASKENTA,
                            Duration.between(laskeStart, Instant.now())),
                        LaskentaTulos.VALMIS);
                    return result;
                  },
                  this.executor);
        };

    if (laskenta.getValintakoelaskenta()) {
      return haeTiedotJaLaske.suorita(
          "VALINTAKOELASKENTA",
          false,
          false,
          laskeDTO -> valintalaskentaResource.valintakoeLaskenta(laskeDTO));
    } else {
      if (!laskenta.getValinnanvaihe().isPresent()) {
        return haeTiedotJaLaske.suorita(
            "KAIKKI VAIHEET LASKENTA",
            false,
            true,
            laskeDTO -> valintalaskentaResource.laskeKaikki(laskeDTO));
      } else {
        return haeTiedotJaLaske.suorita(
            "VALINTALASKENTA",
            false,
            true,
            laskeDTO -> valintalaskentaResource.valintalaskenta(laskeDTO));
      }
    }
  }
}
