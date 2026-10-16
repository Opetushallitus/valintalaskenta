package fi.vm.sade.valintalaskenta.runner.service.impl;

import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintatapajonoJarjestyskriteereillaDTO;
import fi.vm.sade.valinta.sharedutils.ValintaperusteetOperation;
import fi.vm.sade.valintalaskenta.audit.AuditLogUtil;
import fi.vm.sade.valintalaskenta.audit.AuditSession;
import fi.vm.sade.valintalaskenta.domain.dto.AvainArvoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.domain.dto.LaskeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.SuorituspalveluValintadataDTO;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaDto;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaTyyppi;
import fi.vm.sade.valintalaskenta.domain.dto.valintapiste.PistetietoWrapper;
import fi.vm.sade.valintalaskenta.laskenta.dao.SureHakuOidDao;
import fi.vm.sade.valintalaskenta.laskenta.resource.ValintalaskentaResourceImpl;
import fi.vm.sade.valintalaskenta.laskenta.service.valintapiste.ValintapisteService;
import fi.vm.sade.valintalaskenta.runner.resource.external.koostepalvelu.KoostepalveluAsyncResource;
import fi.vm.sade.valintalaskenta.runner.resource.external.koostepalvelu.impl.LahtotiedotException;
import fi.vm.sade.valintalaskenta.runner.resource.external.suorituspalvelu.SuorituspalveluAsyncResource;
import fi.vm.sade.valintalaskenta.runner.resource.external.valintaperusteet.ValintaperusteetAsyncResource;
import fi.vm.sade.valintalaskenta.runner.service.EcsTaskManager;
import fi.vm.sade.valintalaskenta.runner.service.SuoritaLaskentaService;
import java.time.Duration;
import java.time.Instant;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
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
public class SuoritaLaskentaServiceImpl implements SuoritaLaskentaService {
  private static final Logger LOG = LoggerFactory.getLogger(SuoritaLaskentaServiceImpl.class);

  private final ValintalaskentaResourceImpl valintalaskentaResource;
  private final KoostepalveluAsyncResource koostepalveluAsyncResource;
  private final ValintaperusteetAsyncResource valintaperusteetAsyncResource;
  private final SuorituspalveluAsyncResource suorituspalveluAsyncResource;
  private final ValintapisteService valintapisteService;
  private final CloudWatchClient cloudWatchClient;
  private final EcsTaskManager ecsTaskManager;
  private final SureHakuOidDao sureHakuOidDao;

  private final String environmentName;

  private static final String LAHTOTIEDOT = "lahtotiedot";
  private static final String LASKENTA = "laskenta";

  private enum LaskentaTulos {
    VALMIS,
    OHITETTU,
    VIRHE
  }

  @Autowired
  public SuoritaLaskentaServiceImpl(
      ValintalaskentaResourceImpl valintalaskentaResource,
      KoostepalveluAsyncResource koostepalveluAsyncResource,
      ValintaperusteetAsyncResource valintaperusteetAsyncResource,
      SuorituspalveluAsyncResource suorituspalveluAsyncResource,
      ValintapisteService valintapisteService,
      CloudWatchClient cloudWatchClient,
      EcsTaskManager ecsTaskManager,
      SureHakuOidDao sureHakuOidDao,
      @Value("${environment.name}") String environmentName) {
    this.valintalaskentaResource = valintalaskentaResource;
    this.koostepalveluAsyncResource = koostepalveluAsyncResource;
    this.valintaperusteetAsyncResource = valintaperusteetAsyncResource;
    this.suorituspalveluAsyncResource = suorituspalveluAsyncResource;
    this.valintapisteService = valintapisteService;
    this.cloudWatchClient = cloudWatchClient;
    this.ecsTaskManager = ecsTaskManager;
    this.sureHakuOidDao = sureHakuOidDao;
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

    List<ValintaperusteetDTO> valintaperusteet;
    synchronized (this) {
      valintaperusteet =
          valintaperusteetAsyncResource
              .haeValintaperusteet(hakukohdeOids.iterator().next(), laskenta.getValinnanvaihe())
              .join();
    }

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

  @Override
  public void suoritaLaskentaHakukohteille(
      LaskentaDto laskenta, Collection<String> hakukohdeOids, int valintaryhmaRinnakkaisuus) {
    if (laskenta.getTyyppi().equals(LaskentaTyyppi.VALINTARYHMA)) {
      try {
        this.suoritaValintaryhmaLaskenta(laskenta, hakukohdeOids, valintaryhmaRinnakkaisuus);
        return;
      } catch (Exception e) {
        LOG.error(
            "Virhe valintaryhmälaskennan suorittamisessa hakukohteille "
                + hakukohdeOids.stream().collect(Collectors.joining(",")),
            e);
        this.tallennaJaLokitaMetriikat(hakukohdeOids, Collections.emptyMap(), LaskentaTulos.VIRHE);
        throw new RuntimeException(e);
      }
    }

    if (hakukohdeOids.size() != 1) {
      throw new RuntimeException(
          "Hakukohteita on "
              + hakukohdeOids.size()
              + ". Muissa kuin valintaryhmälaskennassa hakukohteita täytyy olla tasan yksi!");
    }

    if (!this.isValintalaskentaKaytossa(laskenta, hakukohdeOids)) {
      LOG.info(
          "Valintalaskenta ei käytössä hakukohteille "
              + hakukohdeOids.stream().collect(Collectors.joining(",")));
      this.tallennaJaLokitaMetriikat(hakukohdeOids, Collections.emptyMap(), LaskentaTulos.OHITETTU);
      return;
    }

    try {
      this.suoritaLaskentaHakukohteelle(laskenta, hakukohdeOids.iterator().next());
    } catch (RuntimeException e) {
      LOG.error(
          "Virhe valintalaskennan suorittamisessa hakukohteille "
              + hakukohdeOids.stream().collect(Collectors.joining(",")),
          e);
      this.tallennaJaLokitaMetriikat(hakukohdeOids, Collections.emptyMap(), LaskentaTulos.VIRHE);
      throw e;
    }
  }

  private void suoritaValintaryhmaLaskenta(
      LaskentaDto laskenta, Collection<String> hakukohdeOids, int rinnakkaisuus) {
    this.ecsTaskManager.withTaskProtection(
        () -> {
          String hakukohteidenNimi =
              String.format("Valintaryhmälaskenta %s hakukohteella", hakukohdeOids.size());
          LOG.info(
              "Muodostetaan VALINTARYHMALASKENTA (Uuid={}) {}",
              laskenta.getUuid(),
              hakukohteidenNimi);
          AuditLogUtil.auditLogLaskenta(
              laskentaAuditSession(laskenta),
              ValintaperusteetOperation.LASKENTATOTEUTUS_KAYNNISTYS,
              laskenta.getUuid(),
              laskenta.getHakuOid(),
              hakukohdeOids,
              Optional.of("VALINTARYHMALASKENTA"));

          AtomicReference<Boolean> stopped = new AtomicReference<>(false);
          ForkJoinPool pool = new ForkJoinPool(rinnakkaisuus);
          List<LaskeDTO> lahtotiedot =
              pool.submit(
                      () ->
                          hakukohdeOids.parallelStream()
                              .map(
                                  hakukohdeOid -> {
                                    if (stopped.get()) {
                                      throw new RuntimeException("Laskenta ei ole enää käynnissä!");
                                    }

                                    try {
                                      return this.haeLahtotiedot(
                                          laskenta, hakukohdeOid, true, true);
                                    } catch (Exception e) {
                                      stopped.set(true);
                                      throw e;
                                    }
                                  })
                              .toList())
                  .join();
          valintalaskentaResource.valintaryhmaLaskenta(laskenta.getUuid(), lahtotiedot);
          this.tallennaJaLokitaMetriikat(
              hakukohdeOids, Collections.emptyMap(), LaskentaTulos.VALMIS);
        });
  }

  private List<AvainArvoDTO> pistetietoToAvainArvot(PistetietoWrapper pistetiedot) {
    final String pistetietoOsallistuminenSuffix = "-OSALLISTUMINEN";
    return pistetiedot.pisteet().stream()
        .flatMap(
            p ->
                Stream.concat(
                    Optional.ofNullable(p.arvo()).stream()
                        .map(a -> new AvainArvoDTO(p.tunniste(), a)),
                    Stream.of(
                        new AvainArvoDTO(
                            p.tunniste() + pistetietoOsallistuminenSuffix,
                            p.osallistuminen() != null ? p.osallistuminen().toString() : ""))))
        .collect(Collectors.toList());
  }

  private void addPistetiedotToHakemus(HakemusDTO hakemus, List<AvainArvoDTO> uudetAvainArvot) {
    Set<String> olemassaolevatAvaimet =
        hakemus.getAvaimet().stream().map(AvainArvoDTO::getAvain).collect(Collectors.toSet());
    for (AvainArvoDTO avainArvo : uudetAvainArvot) {
      if (olemassaolevatAvaimet.contains(avainArvo.getAvain())) {
        LOG.error(
            "Ei voida lisätä pistetietoja, koska hakemukselta {} löytyy jo avain {}.",
            hakemus.getHakemusoid(),
            avainArvo.getAvain());
        throw new RuntimeException(
            "Hakemukselta " + hakemus.getHakemusoid() + " löytyy jo avain " + avainArvo.getAvain());
      } else {
        LOG.debug("Lisätään pistetiedot hakemukselle {}: {}", hakemus.getHakemusoid(), avainArvo);
        hakemus.getAvaimet().add(avainArvo);
      }
    }
  }

  private void combineKoskiJsonFromKoostepalvelutoSupaTiedot(
      List<HakemusDTO> supaTiedot, List<HakemusDTO> koostepalveluTiedot) {
    LOG.info(
        "Yhdistetään Koostepalvelun Koskesta hakemat JSON-tiedot Suorituspalvelun tietoihin. "
            + "Suorituspalvelu: {} hakemusta, Koostepalvelu: {} hakemusta.",
        supaTiedot.size(),
        koostepalveluTiedot.size());

    Map<String, String> hakemusToKoskiJson =
        koostepalveluTiedot.stream()
            .filter(hakemus -> hakemus.getKoskiOpiskeluoikeudetJson() != null)
            .collect(
                Collectors.toMap(
                    HakemusDTO::getHakemusoid,
                    HakemusDTO::getKoskiOpiskeluoikeudetJson,
                    (existing, replacement) -> replacement));

    int updatedCount = 0;
    int missingCount = 0;

    for (HakemusDTO supaHakemus : supaTiedot) {
      String koskiJson = hakemusToKoskiJson.get(supaHakemus.getHakemusoid());

      if (koskiJson != null) {
        supaHakemus.setKoskiOpiskeluoikeudetJson(koskiJson);
        updatedCount++;
        LOG.info("Lisättiin Koski JSON -tiedot hakemukselle {}", supaHakemus.getHakemusoid());
      } else {
        missingCount++;
        // Todo, poistetaan lokitukset testauksen jälkeen, luultavasti käytännössä hyödyttömiä.
        LOG.warn(
            "Koski JSON -tietoja ei löytynyt Koostepalvelusta hakemukselle {}",
            supaHakemus.getHakemusoid());
      }
    }

    LOG.info(
        "Koski JSON-tietojen yhdistämisen yhteenveto: "
            + "päivitettyjä hakemuksia: {}, puuttuvia tietoja: {}",
        updatedCount,
        missingCount);
  }

  private void combinePistetiedotWithHakemusDTOs(
      List<HakemusDTO> hakemukset, List<PistetietoWrapper> valintapisteet) {
    Map<String, PistetietoWrapper> pistetietoMap =
        valintapisteet.stream()
            .collect(Collectors.toMap(PistetietoWrapper::hakemusOID, Function.identity()));

    hakemukset.forEach(
        hakemus -> {
          PistetietoWrapper hakemuksenPistetieto = pistetietoMap.get(hakemus.getHakemusoid());

          if (hakemuksenPistetieto != null && !hakemuksenPistetieto.pisteet().isEmpty()) {
            LOG.debug("Hakemukselle {} löytyi pistetiedot. Käsitellään!", hakemus.getHakemusoid());
            List<AvainArvoDTO> pistetietoAvaimet = pistetietoToAvainArvot(hakemuksenPistetieto);
            addPistetiedotToHakemus(hakemus, pistetietoAvaimet);
          }
        });
  }

  private LaskeDTO haeLahtotiedot(
      LaskentaDto laskenta,
      String hakukohdeOid,
      boolean retryHakemuksetJaOppijat,
      boolean withHakijaRyhmat) {
    boolean kaytetaanUudenSuorituspalvelunTietoja =
        !sureHakuOidDao.haeSureHakuOidit().contains(laskenta.getHakuOid());

    if (kaytetaanUudenSuorituspalvelunTietoja) {
      LOG.info(
          "Käytetään haun {} hakukohteelle {} Suorituspalvelun tietoja!",
          laskenta.getHakuOid(),
          hakukohdeOid);
      // Käynnistetään haut Koostepalvelusta ja Suorituspalvelusta rinnakkain
      CompletableFuture<LaskeDTO> koostepalvelustaHaettavat =
          this.koostepalveluAsyncResource.haeLahtotiedot(
              laskenta, hakukohdeOid, retryHakemuksetJaOppijat, withHakijaRyhmat);
      CompletableFuture<SuorituspalveluValintadataDTO> supastaHaettavat =
          this.suorituspalveluAsyncResource.haeValintaData(laskenta.getHakuOid(), hakukohdeOid);

      SuorituspalveluValintadataDTO supastaHaetut = odotaLahtotietoja(supastaHaettavat);
      List<HakemusDTO> hakemusDTOtSupasta = supastaHaetut.getValintaHakemukset();
      Set<String> supaHakemusOids =
          hakemusDTOtSupasta.stream().map(HakemusDTO::getHakemusoid).collect(Collectors.toSet());
      List<PistetietoWrapper> pistetiedot =
          valintapisteService.findValintapisteetForHakemukset(supaHakemusOids).getRight();
      combinePistetiedotWithHakemusDTOs(hakemusDTOtSupasta, pistetiedot);

      LOG.info(
          "Saatiin Suorituspalvelusta haun {} hakukohteelle {} tiedot, yhteensä {} hakemusta.",
          laskenta.getHakuOid(),
          hakukohdeOid,
          supastaHaetut.getValintaHakemukset().size());
      LaskeDTO tiedotKoostepalvelusta = odotaLahtotietoja(koostepalvelustaHaettavat);
      combineKoskiJsonFromKoostepalvelutoSupaTiedot(
          hakemusDTOtSupasta, tiedotKoostepalvelusta.getHakemus());

      // Yhdistetään Supasta tulevat HakemusDTO:t Koostepalvelusta tuleviin lähtötietoihin.
      return new LaskeDTO(
          tiedotKoostepalvelusta.getUuid(),
          tiedotKoostepalvelusta.isKorkeakouluhaku(),
          tiedotKoostepalvelusta.isErillishaku(),
          tiedotKoostepalvelusta.getHakukohdeOid(),
          hakemusDTOtSupasta,
          tiedotKoostepalvelusta.getValintaperuste(),
          tiedotKoostepalvelusta.getHakijaryhmat());
    } else {
      LOG.info(
          "Käytetään haun {} hakukohteelle {} Suoritusrekisterin tietoja!",
          laskenta.getHakuOid(),
          hakukohdeOid);
      LaskeDTO tiedotKoostepalvelusta =
          odotaLahtotietoja(
              this.koostepalveluAsyncResource.haeLahtotiedot(
                  laskenta, hakukohdeOid, retryHakemuksetJaOppijat, withHakijaRyhmat));
      if (Boolean.FALSE.equals(tiedotKoostepalvelusta.getSuoritusrekisterinTiedotHaettu())) {
        throw new RuntimeException(
            String.format(
                "Haun %s hakukohteen %s laskenta käyttää Suoritusrekisterin tietoja, joita ei ole enää saatavilla. Laskentaa ei voida suorittaa.",
                laskenta.getHakuOid(), hakukohdeOid));
      }
      return tiedotKoostepalvelusta;
    }
  }

  private static <T> T odotaLahtotietoja(CompletableFuture<T> future) {
    try {
      return future.get();
    } catch (Exception e) {
      throw new LahtotiedotException(e);
    }
  }

  private void suoritaLaskentaHakukohteelle(LaskentaDto laskenta, String hakukohdeOid) {

    String tyyppi;
    boolean retryHakemuksetJaOppijat;
    boolean withHakijaRyhmat;
    Function<LaskeDTO, String> laske;
    if (laskenta.getValintakoelaskenta()) {
      tyyppi = "VALINTAKOELASKENTA";
      retryHakemuksetJaOppijat = false;
      withHakijaRyhmat = false;
      laske = laskeDTO -> valintalaskentaResource.valintakoeLaskenta(laskeDTO);
    } else {
      if (!laskenta.getValinnanvaihe().isPresent()) {
        tyyppi = "KAIKKI VAIHEET LASKENTA";
        retryHakemuksetJaOppijat = false;
        withHakijaRyhmat = true;
        laske = laskeDTO -> valintalaskentaResource.laskeKaikki(laskeDTO);
      } else {
        tyyppi = "VALINTALASKENTA";
        retryHakemuksetJaOppijat = false;
        withHakijaRyhmat = true;
        laske = laskeDTO -> valintalaskentaResource.valintalaskenta(laskeDTO);
      }
    }

    LOG.info("Muodostetaan {} (Uuid={}) {}", tyyppi, laskenta.getUuid(), hakukohdeOid);
    AuditLogUtil.auditLogLaskenta(
        laskentaAuditSession(laskenta),
        ValintaperusteetOperation.LASKENTATOTEUTUS_KAYNNISTYS,
        laskenta.getUuid(),
        laskenta.getHakuOid(),
        Collections.singleton(hakukohdeOid),
        Optional.of(tyyppi));
    Instant lahtotiedotStart = Instant.now();

    LaskeDTO kaytettavaLaskeDTO =
        haeLahtotiedot(laskenta, hakukohdeOid, retryHakemuksetJaOppijat, withHakijaRyhmat);

    Instant laskeStart = Instant.now();
    LOG.info(
        "Haettiin lähtötiedot hakukohteelle "
            + hakukohdeOid
            + ", start: "
            + lahtotiedotStart
            + ", end: "
            + laskeStart);
    laske.apply(kaytettavaLaskeDTO);
    this.tallennaJaLokitaMetriikat(
        Collections.singleton(hakukohdeOid),
        Map.of(
            LAHTOTIEDOT,
            Duration.between(lahtotiedotStart, laskeStart),
            LASKENTA,
            Duration.between(laskeStart, Instant.now())),
        LaskentaTulos.VALMIS);
  }
}
