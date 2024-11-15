package fi.vm.sade.valintalaskenta.runner.service;

import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintatapajonoJarjestyskriteereillaDTO;
import fi.vm.sade.valinta.sharedutils.ValintaperusteetOperation;
import fi.vm.sade.valintalaskenta.audit.AuditLogUtil;
import fi.vm.sade.valintalaskenta.audit.AuditSession;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaDto;
import fi.vm.sade.valintalaskenta.laskenta.resource.ValintalaskentaResourceImpl;
import fi.vm.sade.valintalaskenta.runner.resource.external.koostepalvelu.KoostepalveluAsyncResource;
import fi.vm.sade.valintalaskenta.runner.resource.external.valintaperusteet.ValintaperusteetAsyncResource;
import java.time.Duration;
import java.time.Instant;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
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
  private final ExecutorService executor = Executors.newWorkStealingPool();
  private final CloudWatchClient cloudWatchClient;

  private final String environmentName;

  @Autowired
  public SuoritaLaskentaService(
      ValintalaskentaResourceImpl valintalaskentaResource,
      KoostepalveluAsyncResource koostepalveluAsyncResource,
      ValintaperusteetAsyncResource valintaperusteetAsyncResource,
      CloudWatchClient cloudWatchClient,
      @Value("${environment.name}") String environmentName) {
    this.valintalaskentaResource = valintalaskentaResource;
    this.koostepalveluAsyncResource = koostepalveluAsyncResource;
    this.valintaperusteetAsyncResource = valintaperusteetAsyncResource;
    this.cloudWatchClient = cloudWatchClient;
    this.environmentName = environmentName;
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
      String hakukohdeOid, Duration lahtotiedotDuration, Duration laskentaDuration) {
    MetricDatum lahtotiedot =
        MetricDatum.builder()
            .metricName("kesto")
            .value((double) lahtotiedotDuration.toMillis())
            .storageResolution(60)
            .dimensions(List.of(Dimension.builder().name("vaihe").value("lahtotiedot").build()))
            .timestamp(Instant.now())
            .unit(StandardUnit.MILLISECONDS)
            .build();

    MetricDatum laskenta =
        MetricDatum.builder()
            .metricName("kesto")
            .value((double) laskentaDuration.toMillis())
            .storageResolution(60)
            .dimensions(List.of(Dimension.builder().name("vaihe").value("laskenta").build()))
            .timestamp(Instant.now())
            .unit(StandardUnit.MILLISECONDS)
            .build();

    MetricDatum valmiit =
        MetricDatum.builder()
            .metricName("valmiit")
            .value((double) 1)
            .storageResolution(60)
            .timestamp(Instant.now())
            .unit(StandardUnit.COUNT)
            .build();

    this.cloudWatchClient.putMetricData(
        PutMetricDataRequest.builder()
            .namespace(this.environmentName + "-valintalaskenta")
            .metricData(List.of(lahtotiedot, laskenta, valmiit))
            .build());

    LOG.info(
        "Kesto: Hakukohde: {}, lähtotiedot: {} ms, laskenta: {} ms",
        hakukohdeOid,
        lahtotiedotDuration.toMillis(),
        laskentaDuration.toMillis());
  }

  public CompletableFuture<String> suoritaLaskentaHakukohteille(
      LaskentaDto laskenta, Collection<String> hakukohdeOids) {
    AuditSession auditSession = laskentaAuditSession(laskenta);

    if (laskenta
        .getTyyppi()
        .equals(fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaTyyppi.VALINTARYHMA)) {
      String hakukohteidenNimi =
          String.format("Valintaryhmälaskenta %s hakukohteella", hakukohdeOids.size());
      LOG.info(
          "Muodostetaan VALINTARYHMALASKENTA (Uuid={}) {}", laskenta.getUuid(), hakukohteidenNimi);
      AuditLogUtil.auditLogLaskenta(
          auditSession,
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
                return valintalaskentaResource.valintaryhmaLaskenta(laskenta.getUuid(), laskeDTOs);
              },
              this.executor);
    }

    if (hakukohdeOids.size() != 1) {
      throw new RuntimeException(
          "Hakukohteita on "
              + hakukohdeOids.size()
              + ". Muissa kuin valintaryhmälaskennassa hakukohteita täytyy olla tasan yksi!");
    }
    if (!this.isValintalaskentaKaytossa(laskenta, hakukohdeOids)) {
      return CompletableFuture.completedFuture(laskenta.getUuid());
    }
    Instant lahtotiedotStart = Instant.now();

    if (laskenta.getValintakoelaskenta()) {
      String hakukohdeOid = hakukohdeOids.iterator().next();
      LOG.info("Muodostetaan VALINTAKOELASKENTA (Uuid={}) {}", laskenta.getUuid(), hakukohdeOid);
      AuditLogUtil.auditLogLaskenta(
          auditSession,
          ValintaperusteetOperation.LASKENTATOTEUTUS_KAYNNISTYS,
          laskenta.getUuid(),
          laskenta.getHakuOid(),
          hakukohdeOids,
          Optional.of("VALINTAKOELASKENTA"));
      return this.koostepalveluAsyncResource
          .haeLahtotiedot(laskenta, hakukohdeOid, false, false)
          .thenApplyAsync(
              laskeDTO -> {
                Instant laskeStart = Instant.now();
                String result = valintalaskentaResource.valintakoeLaskenta(laskeDTO);
                this.tallennaJaLokitaMetriikat(
                    hakukohdeOid,
                    Duration.between(lahtotiedotStart, laskeStart),
                    Duration.between(laskeStart, Instant.now()));
                return result;
              },
              this.executor);
    } else {
      if (!laskenta.getValinnanvaihe().isPresent()) {
        String hakukohdeOid = hakukohdeOids.iterator().next();
        LOG.info(
            "Muodostetaan KAIKKI VAIHEET LASKENTA (Uuid={}) {}", laskenta.getUuid(), hakukohdeOid);
        AuditLogUtil.auditLogLaskenta(
            auditSession,
            ValintaperusteetOperation.LASKENTATOTEUTUS_KAYNNISTYS,
            laskenta.getUuid(),
            laskenta.getHakuOid(),
            hakukohdeOids,
            Optional.of("KAIKKI VAIHEET LASKENTA"));

        return this.koostepalveluAsyncResource
            .haeLahtotiedot(laskenta, hakukohdeOid, false, true)
            .thenApplyAsync(
                laskeDTO -> {
                  Instant laskeStart = Instant.now();
                  String result = valintalaskentaResource.laskeKaikki(laskeDTO);
                  this.tallennaJaLokitaMetriikat(
                      hakukohdeOid,
                      Duration.between(lahtotiedotStart, laskeStart),
                      Duration.between(laskeStart, Instant.now()));
                  return result;
                },
                this.executor);
      } else {
        String hakukohdeOid = hakukohdeOids.iterator().next();
        LOG.info("Muodostetaan VALINTALASKENTA (Uuid={}) {}", laskenta.getUuid(), hakukohdeOid);
        AuditLogUtil.auditLogLaskenta(
            auditSession,
            ValintaperusteetOperation.LASKENTATOTEUTUS_KAYNNISTYS,
            laskenta.getUuid(),
            laskenta.getHakuOid(),
            hakukohdeOids,
            Optional.of("VALINTALASKENTA"));
        return this.koostepalveluAsyncResource
            .haeLahtotiedot(laskenta, hakukohdeOid, false, true)
            .thenApplyAsync(
                laskeDTO -> {
                  Instant laskeStart = Instant.now();
                  String result = valintalaskentaResource.valintalaskenta(laskeDTO);
                  this.tallennaJaLokitaMetriikat(
                      hakukohdeOid,
                      Duration.between(lahtotiedotStart, laskeStart),
                      Duration.between(laskeStart, Instant.now()));
                  return result;
                },
                this.executor);
      }
    }
  }
}
