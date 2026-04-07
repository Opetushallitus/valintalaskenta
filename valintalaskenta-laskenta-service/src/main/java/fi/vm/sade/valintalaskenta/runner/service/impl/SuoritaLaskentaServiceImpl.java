package fi.vm.sade.valintalaskenta.runner.service.impl;

import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintatapajonoJarjestyskriteereillaDTO;
import fi.vm.sade.valinta.sharedutils.ValintaperusteetOperation;
import fi.vm.sade.valintalaskenta.audit.AuditLogUtil;
import fi.vm.sade.valintalaskenta.audit.AuditSession;
import fi.vm.sade.valintalaskenta.domain.dto.AvainArvoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.AvainMetatiedotDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakukohdeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.LaskeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.SuorituspalveluValintadataDTO;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaDto;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaTyyppi;
import fi.vm.sade.valintalaskenta.domain.dto.valintapiste.PistetietoWrapper;
import fi.vm.sade.valintalaskenta.laskenta.resource.ValintalaskentaResourceImpl;
import fi.vm.sade.valintalaskenta.laskenta.service.valintapiste.ValintapisteService;
import fi.vm.sade.valintalaskenta.runner.resource.external.koostepalvelu.KoostepalveluAsyncResource;
import fi.vm.sade.valintalaskenta.runner.resource.external.suorituspalvelu.impl.SuorituspalveluAsyncResourceImpl;
import fi.vm.sade.valintalaskenta.runner.resource.external.valintaperusteet.ValintaperusteetAsyncResource;
import fi.vm.sade.valintalaskenta.runner.service.EcsTaskManager;
import fi.vm.sade.valintalaskenta.runner.service.SuoritaLaskentaService;
import java.time.Duration;
import java.time.Instant;
import java.util.*;
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
  private final SuorituspalveluAsyncResourceImpl suorituspalveluAsyncResource;
  private final ValintapisteService valintapisteService;
  private final CloudWatchClient cloudWatchClient;
  private final EcsTaskManager ecsTaskManager;
  // Korkeakoulujen kevään 2025 ensimmäinen yhteishaku 1.2.246.562.29.00000000000000054530
  // Korkeakoulujen kevään 2025 toinen yhteishaku 1.2.246.562.29.00000000000000054531
  // Korkeakoulujen kevään 2026 ensimmäinen yhteishaku 1.2.246.562.29.00000000000000072413
  // Korkeakoulujen yhteishaku syksy 2025 1.2.246.562.29.00000000000000069585
  // Default
  // "1.2.246.562.29.00000000000000054530,1.2.246.562.29.00000000000000054531,1.2.246.562.29.00000000000000072413,1.2.246.562.29.00000000000000069585"
  final Set<String> hautJoilleKaytetaanSuoritusrekisterinTietoja;

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
      SuorituspalveluAsyncResourceImpl suorituspalveluAsyncResource,
      ValintapisteService valintapisteService,
      CloudWatchClient cloudWatchClient,
      EcsTaskManager ecsTaskManager,
      @Value("${environment.name}") String environmentName,
      @Value("${valintalaskenta-laskenta-service.suoritusrekisteri-haku-oids}")
          String suoritusrekisteriHakuOids) {
    this.valintalaskentaResource = valintalaskentaResource;
    this.koostepalveluAsyncResource = koostepalveluAsyncResource;
    this.valintaperusteetAsyncResource = valintaperusteetAsyncResource;
    this.suorituspalveluAsyncResource = suorituspalveluAsyncResource;
    this.valintapisteService = valintapisteService;
    this.cloudWatchClient = cloudWatchClient;
    this.ecsTaskManager = ecsTaskManager;
    this.environmentName = environmentName;
    if (suoritusrekisteriHakuOids == null || suoritusrekisteriHakuOids.isEmpty()) {
      this.hautJoilleKaytetaanSuoritusrekisterinTietoja = Collections.emptySet();
    } else {
      Set<String> sureOids =
          Arrays.stream(suoritusrekisteriHakuOids.split(","))
              .filter(s -> !s.isEmpty())
              .collect(Collectors.toSet());
      this.hautJoilleKaytetaanSuoritusrekisterinTietoja = sureOids;
    }
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
                                      return this.koostepalveluAsyncResource.haeLahtotiedot(
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

  public void vertaileHarkinnanvaraisuus(
      List<HakemusDTO> koostepalvelusta, List<HakemusDTO> suorituspalvelusta) {
    LOG.info(
        "Vertaillaan harkinnanvaraisuus-tietoja! Koostepalvelusta {} hakemusta, Suorituspalvelusta {} hakemusta.",
        koostepalvelusta.size(),
        suorituspalvelusta.size());

    int totalHakemukset = koostepalvelusta.size();
    int matchingHakemukset = 0;
    int missingHakemukset = 0;
    int totalSameHarkinnanvaraisuus = 0;
    int totalDifferentHarkinnanvaraisuus = 0;
    int totalMissingHakukohteet = 0;

    Map<String, Integer> hakukohdeetWithDifferentValues = new HashMap<>();
    Map<String, Integer> hakukohdeetWithSameValues = new HashMap<>();

    for (HakemusDTO koostepalveluHakemus : koostepalvelusta) {
      Optional<HakemusDTO> supaHakemus =
          suorituspalvelusta.stream()
              .filter(
                  suorituspalveluHakemus ->
                      suorituspalveluHakemus
                          .getHakemusoid()
                          .equals(koostepalveluHakemus.getHakemusoid()))
              .findFirst();

      if (supaHakemus.isPresent()) {
        matchingHakemukset++;
        LOG.info(
            "Löytyi vastaava hakemus sekä Koostepalvelusta että Suorituspalvelusta! {}",
            supaHakemus.get().getHakemusoid());

        List<HakukohdeDTO> supaHakukohteet = supaHakemus.get().getHakukohteet();
        List<HakukohdeDTO> koosteHakukohteet = koostepalveluHakemus.getHakukohteet();

        for (HakukohdeDTO koosteHakukohde : koosteHakukohteet) {
          Optional<HakukohdeDTO> supaHakukohde =
              supaHakukohteet.stream()
                  .filter(h -> h.getOid().equals(koosteHakukohde.getOid()))
                  .findFirst();

          if (supaHakukohde.isPresent()) {
            boolean koosteHarkinnanvaraisuus = koosteHakukohde.isHarkinnanvaraisuus();
            boolean supaHarkinnanvaraisuus = supaHakukohde.get().isHarkinnanvaraisuus();

            if (koosteHarkinnanvaraisuus == supaHarkinnanvaraisuus) {
              totalSameHarkinnanvaraisuus++;
              hakukohdeetWithSameValues.merge(koosteHakukohde.getOid(), 1, Integer::sum);
              LOG.trace(
                  "Harkinnanvaraisuus täsmää hakemuksen {} hakukohteelle {}: {}",
                  koostepalveluHakemus.getHakemusoid(),
                  koosteHakukohde.getOid(),
                  koosteHarkinnanvaraisuus);
            } else {
              totalDifferentHarkinnanvaraisuus++;
              hakukohdeetWithDifferentValues.merge(koosteHakukohde.getOid(), 1, Integer::sum);
              LOG.warn(
                  "Harkinnanvaraisuus eroaa hakemuksen {} hakukohteelle {}. Koostepalvelu: {}, Suorituspalvelu: {}",
                  koostepalveluHakemus.getHakemusoid(),
                  koosteHakukohde.getOid(),
                  koosteHarkinnanvaraisuus,
                  supaHarkinnanvaraisuus);
            }
          } else {
            totalMissingHakukohteet++;
            LOG.warn(
                "Hakukohde {} löytyi Koostepalvelusta hakemukselle {}, mutta ei Suorituspalvelusta!",
                koosteHakukohde.getOid(),
                koostepalveluHakemus.getHakemusoid());
          }
        }

        LOG.info(
            "Hakemuksen {} harkinnanvaraisuus-vertailu: täsmäävät: {}, erilaiset: {}, puuttuvat hakukohteet: {}, hakukohteet yhteensä: {}",
            koostepalveluHakemus.getHakemusoid(),
            totalSameHarkinnanvaraisuus,
            totalDifferentHarkinnanvaraisuus,
            totalMissingHakukohteet,
            koosteHakukohteet.size());

      } else {
        missingHakemukset++;
        LOG.warn(
            "Hakemus {} löytyi Koostepalvelusta, mutta ei Suorituspalvelusta!",
            koostepalveluHakemus.getHakemusoid());
      }
    }

    LOG.info(
        "Harkinnanvaraisuus-vertailun yhteenveto: yhteensä hakemuksia: {}, vastaavia hakemuksia: {}, puuttuvia hakemuksia: {}",
        totalHakemukset,
        matchingHakemukset,
        missingHakemukset);

    LOG.info(
        "Harkinnanvaraisuus-arvojen vertailun yhteenveto: täsmäävät: {}, erilaiset: {}, puuttuvat hakukohteet: {}",
        totalSameHarkinnanvaraisuus,
        totalDifferentHarkinnanvaraisuus,
        totalMissingHakukohteet);

    // Erilaiset harkinnanvaraisuus-arvot kaikille hakemuksille yhteensä
    if (!hakukohdeetWithDifferentValues.isEmpty()) {
      LOG.warn("Harkinnanvaraisuus-eroavaisuuksien yhteenveto:");
      hakukohdeetWithDifferentValues.entrySet().stream()
          .sorted(Map.Entry.<String, Integer>comparingByValue().reversed())
          .forEach(
              entry ->
                  LOG.warn(
                      "Hakukohde: {}, eroaa {} hakemuksessa", entry.getKey(), entry.getValue()));
    }

    // Täsmäävät harkinnanvaraisuus-arvot kaikille hakemuksille yhteensä
    if (!hakukohdeetWithSameValues.isEmpty()) {
      LOG.info("Harkinnanvaraisuus-täsmäävyyksien yhteenveto:");
      hakukohdeetWithSameValues.entrySet().stream()
          .sorted(Map.Entry.<String, Integer>comparingByValue().reversed())
          .forEach(
              entry ->
                  LOG.info(
                      "Hakukohde: {}, täsmää {} hakemuksessa", entry.getKey(), entry.getValue()));
    }
  }

  public void vertaileAvainMetatiedot(
      List<HakemusDTO> koostepalvelusta, List<HakemusDTO> suorituspalvelusta) {

    LOG.info(
        "Vertaillaan avainmetatietoja! Koostepalvelusta {} hakemusta, Suorituspalvelusta {} hakemusta.",
        koostepalvelusta.size(),
        suorituspalvelusta.size());

    int totalHakemukset = koostepalvelusta.size();
    int matchingHakemukset = 0;
    int missingHakemukset = 0;
    int totalSameMetatiedot = 0;
    int totalDifferentMetatiedot = 0;
    int totalMissingMetatiedot = 0;
    int totalIgnoredMetatiedot = 0;

    Map<String, Integer> missingKeysCounts = new HashMap<>();
    Map<String, Integer> matchingKeysCounts = new HashMap<>();
    Map<String, Integer> differentKeysCounts = new HashMap<>();

    for (HakemusDTO koostepalveluHakemus : koostepalvelusta) {
      // Counters for this specific hakemus
      int sameMetatiedot = 0;
      int differentMetatiedot = 0;
      int missingMetatiedot = 0;
      int ignoredMetatiedot = 0;

      Optional<HakemusDTO> supaHakemus =
          suorituspalvelusta.stream()
              .filter(
                  suorituspalveluHakemus ->
                      suorituspalveluHakemus
                          .getHakemusoid()
                          .equals(koostepalveluHakemus.getHakemusoid()))
              .findFirst();

      if (supaHakemus.isPresent()) {
        matchingHakemukset++;
        LOG.info(
            "Löytyi vastaava hakemus sekä Koostepalvelusta että Suorituspalvelusta! {}",
            supaHakemus.get().getHakemusoid());

        List<AvainMetatiedotDTO> supaMetatiedot = supaHakemus.get().getAvainMetatiedotDTO();
        List<AvainMetatiedotDTO> koosteMetatiedot = koostepalveluHakemus.getAvainMetatiedotDTO();

        for (AvainMetatiedotDTO koosteMetatieto : koosteMetatiedot) {
          String avain = koosteMetatieto.getAvain();

          Optional<AvainMetatiedotDTO> supaMetatieto =
              supaMetatiedot.stream().filter(am -> am.getAvain().equals(avain)).findFirst();

          if (supaMetatieto.isPresent()) {
            if (metatiedotMatch(
                koosteMetatieto.getMetatiedot(), supaMetatieto.get().getMetatiedot())) {
              sameMetatiedot++;
              matchingKeysCounts.merge(avain, 1, Integer::sum);
              LOG.info(
                  "Supasta ja Koostepalvelusta löytyi hakemukselle {} samat metatiedot avaimelle {}. Jee.",
                  koostepalveluHakemus.getHakemusoid(),
                  avain);
            } else {
              differentMetatiedot++;
              differentKeysCounts.merge(avain, 1, Integer::sum);
              LOG.warn(
                  "Supasta ja Koostepalvelusta löytyi eri metatiedot hakemuksen {} avaimelle {}. Kooste: {}, Supa: {}",
                  koostepalveluHakemus.getHakemusoid(),
                  avain,
                  koosteMetatieto.getMetatiedot(),
                  supaMetatieto.get().getMetatiedot());
              logMetatiedotDetails(
                  koostepalveluHakemus.getHakemusoid(),
                  avain,
                  koosteMetatieto.getMetatiedot(),
                  supaMetatieto.get().getMetatiedot());
            }
          } else {
            missingMetatiedot++;
            missingKeysCounts.merge(avain, 1, Integer::sum);
            LOG.warn(
                "Koostepalvelusta löytyi hakemuksen {} avaimelle {} metatietorivä ({} riveä), mutta Suorituspalvelusta ei palautunut vastaavaa!",
                koostepalveluHakemus.getHakemusoid(),
                avain,
                koosteMetatieto.getMetatiedot().size());
          }
        }

        LOG.info(
            "Hakemuksen {} avainmetatietojen vertailu: samoja: {}, eriäviä: {}, puuttuvia: {}, ohitettuja: {}, metatietoja yhteensä: {}",
            koostepalveluHakemus.getHakemusoid(),
            sameMetatiedot,
            differentMetatiedot,
            missingMetatiedot,
            ignoredMetatiedot,
            koosteMetatiedot.size());

        totalSameMetatiedot += sameMetatiedot;
        totalDifferentMetatiedot += differentMetatiedot;
        totalMissingMetatiedot += missingMetatiedot;
        totalIgnoredMetatiedot += ignoredMetatiedot;

      } else {
        missingHakemukset++;
        LOG.info(
            "Koostepalvelusta palautui hakemus {}, mutta Suorituspalvelusta ei löytynyt vastaavaa.",
            koostepalveluHakemus.getHakemusoid());
      }
    }

    LOG.info(
        "Avainmetatietojen vertailun yhteenveto: yhteensä hakemuksia: {}, vastaavia hakemuksia: {}, puuttuvia hakemuksia: {}",
        totalHakemukset,
        matchingHakemukset,
        missingHakemukset);

    LOG.info(
        "Metatietojen vertailun yhteenveto: samoja: {}, eriäviä: {}, puuttuvia: {}, ohitettuja: {}",
        totalSameMetatiedot,
        totalDifferentMetatiedot,
        totalMissingMetatiedot,
        totalIgnoredMetatiedot);

    // Puuttuvat metatiedot kaikille hakemuksille yhteensä
    if (!missingKeysCounts.isEmpty()) {
      LOG.warn("Puuttuvia metatietoja yhteenveto:");
      missingKeysCounts.entrySet().stream()
          .sorted(Map.Entry.<String, Integer>comparingByValue().reversed())
          .forEach(
              entry ->
                  LOG.warn("Avain: {}, puuttuu {} hakemuksessa", entry.getKey(), entry.getValue()));
    }

    // Erilaiset metatiedot kaikille hakemuksille yhteensä
    if (!differentKeysCounts.isEmpty()) {
      LOG.warn("Eriäviä metatietoja yhteenveto:");
      differentKeysCounts.entrySet().stream()
          .sorted(Map.Entry.<String, Integer>comparingByValue().reversed())
          .forEach(
              entry ->
                  LOG.warn("Avain: {}, eroaa {} hakemuksessa", entry.getKey(), entry.getValue()));
    }

    // Täsmäävät metatiedot kaikille hakemuksille yhteensä
    if (!matchingKeysCounts.isEmpty()) {
      LOG.info("Täsmäävien metatietojen yhteenveto:");
      matchingKeysCounts.entrySet().stream()
          .sorted(Map.Entry.<String, Integer>comparingByValue().reversed())
          .forEach(
              entry ->
                  LOG.info("Avain: {}, täsmää {} hakemuksessa", entry.getKey(), entry.getValue()));
    }
  }

  /**
   * Compares two lists of metatiedot maps for equality, ignoring key order
   *
   * @param koosteMetatiedot Metatiedot from koostepalvelu
   * @param supaMetatiedot Metatiedot from suorituspalvelu
   * @return true if both lists contain the same data (order-independent), false otherwise
   */
  private boolean metatiedotMatch(
      List<Map<String, String>> koosteMetatiedot, List<Map<String, String>> supaMetatiedot) {
    if (koosteMetatiedot == null && supaMetatiedot == null) {
      return true;
    }
    if (koosteMetatiedot == null || supaMetatiedot == null) {
      return false;
    }
    if (koosteMetatiedot.size() != supaMetatiedot.size()) {
      return false;
    }

    // Compare each row in the metatiedot, ignoring order
    for (Map<String, String> koosteMap : koosteMetatiedot) {
      Boolean anyMatch =
          supaMetatiedot.stream().anyMatch(supaMap -> mapsAreEqual(koosteMap, supaMap));
      if (!anyMatch) {
        LOG.info(
            "Ei löydetty vastaavaa arvoa Supasta! kooste {}, supaTiedot: {}",
            koosteMap,
            supaMetatiedot);
        return false;
      }
    }

    return true;
  }

  /**
   * Compares two maps for content equality, ignoring key order
   *
   * @param map1 First map to compare
   * @param map2 Second map to compare
   * @return true if maps contain the same key-value pairs, false otherwise
   */
  private boolean mapsAreEqual(Map<String, String> map1, Map<String, String> map2) {
    if (map1 == null && map2 == null) {
      return true;
    }
    if (map1 == null || map2 == null) {
      return false;
    }
    if (map1.size() != map2.size()) {
      return false;
    }

    // Check if all key-value pairs in map1 exist in map2
    return map1.entrySet().stream()
        .allMatch(entry -> entry.getValue().equals(map2.get(entry.getKey())));
  }

  /**
   * Logs detailed differences between metatiedot from two sources
   *
   * @param hakemusOid Hakemus OID for logging
   * @param avain The key being compared
   * @param koosteMetatiedot Metatiedot from koostepalvelu
   * @param supaMetatiedot Metatiedot from suorituspalvelu
   */
  private void logMetatiedotDetails(
      String hakemusOid,
      String avain,
      List<Map<String, String>> koosteMetatiedot,
      List<Map<String, String>> supaMetatiedot) {
    LOG.debug(
        "Koostepalvelun metatiedot hakemuksen {} avaimelle {}: {} kpl",
        hakemusOid,
        avain,
        koosteMetatiedot.size());
    for (int i = 0; i < koosteMetatiedot.size(); i++) {
      Map<String, String> koosteRow = koosteMetatiedot.get(i);
      koosteRow.forEach(
          (k, v) -> {
            LOG.info("Kooste avain {}: {} = {} ", avain, k, v);
          });
      LOG.info("  Rivi {}: {}", i + 1, koosteMetatiedot.get(i));
    }

    LOG.info("Suorituspalvelun metatiedot avaimelle {}: {} kpl", avain, supaMetatiedot.size());
    for (int i = 0; i < supaMetatiedot.size(); i++) {
      Map<String, String> supaRow = supaMetatiedot.get(i);
      supaRow.forEach(
          (k, v) -> {
            LOG.info("Supa avain {}: {} = {} ", avain, k, v);
          });
      LOG.info("  Rivi {}: {}", i + 1, supaMetatiedot.get(i));
    }
  }

  public void logAvainMetatiedot(List<HakemusDTO> hakemukset, String lahde) {
    LOG.info("Lokitetaan avainmetatiedot {} hakemuksesta (lähde {})", hakemukset.size(), lahde);

    int totalHakemukset = hakemukset.size();
    int hakemusetWithMetatiedot = 0;
    int totalMetatiedot = 0;

    Map<String, Integer> metatiedotCounts = new HashMap<>();

    for (HakemusDTO hakemus : hakemukset) {
      List<AvainMetatiedotDTO> avainMetatiedot = hakemus.getAvainMetatiedotDTO();

      if (avainMetatiedot == null || avainMetatiedot.isEmpty()) {
        LOG.warn("{} Hakemuksella {} ei ole avainmetatietoja", lahde, hakemus.getHakemusoid());
        continue;
      }

      hakemusetWithMetatiedot++;
      LOG.info(
          "{} Hakemuksella {} on {} avainmetatietoa",
          lahde,
          hakemus.getHakemusoid(),
          avainMetatiedot.size());

      for (AvainMetatiedotDTO metatiedot : avainMetatiedot) {
        String avain = metatiedot.getAvain();
        List<Map<String, String>> metatiedotList = metatiedot.getMetatiedot();

        totalMetatiedot++;
        metatiedotCounts.merge(avain, 1, Integer::sum);

        if (metatiedotList == null || metatiedotList.isEmpty()) {
          LOG.warn(
              "{} Hakemuksella {} avaimella {} ei ole metatietoja",
              lahde,
              hakemus.getHakemusoid(),
              avain);
          continue;
        }

        LOG.info(
            "{} Hakemuksella {} avaimella {} on {} metatietoriviä",
            lahde,
            hakemus.getHakemusoid(),
            avain,
            metatiedotList.size());

        for (int i = 0; i < metatiedotList.size(); i++) {
          Map<String, String> metatietoMap = metatiedotList.get(i);
          LOG.info(
              "{} Hakemuksella {} avaimella {} metatietoriviä {}: {}",
              lahde,
              hakemus.getHakemusoid(),
              avain,
              i + 1,
              metatietoMap);

          int finalI = i;
          metatietoMap.forEach(
              (key, value) ->
                  LOG.info(
                      "{} Hakemuksella {} avaimella {} metatietoriviä {} avain: {}, arvo: {}",
                      lahde,
                      hakemus.getHakemusoid(),
                      avain,
                      finalI + 1,
                      key,
                      value));
        }
      }
    }

    LOG.info(
        "Avainmetatietojen lokituksen yhteenveto: yhteensä hakemuksia: {}, hakemuksia joilla metatietoja: {}, metatietoja yhteensä: {}",
        totalHakemukset,
        hakemusetWithMetatiedot,
        totalMetatiedot);

    // Avainmetatietojen esiintymistiheys
    if (!metatiedotCounts.isEmpty()) {
      LOG.info("Avainmetatietojen esiintymistiheys:");
      metatiedotCounts.entrySet().stream()
          .sorted(Map.Entry.<String, Integer>comparingByValue().reversed())
          .forEach(
              entry ->
                  LOG.info(
                      "Avain: {}, esiintyy {} hakemuksessa", entry.getKey(), entry.getValue()));
    }
  }

  public void vertaile(List<HakemusDTO> koostepalvelusta, List<HakemusDTO> suorituspalvelusta) {
    // Lisätään tähän listaan avaimia, joiden vertailu ei jostain syystä ole kiinnostavaa.
    Set<String> keysToIgnore =
        new HashSet<>(
            Arrays.asList(
                "LISAKOULUTUS_VAMMAISTEN",
                "LISAKOULUTUS_MAAHANMUUTTO",
                "LISAKOULUTUS_MAAHANMUUTTO_LUKIO",
                "LISAKOULUTUS_AMMATTISTARTTI",
                "LISAKOULUTUS_KYMPPI",
                "LISAKOULUTUS_TALOUS",
                "LISAKOULUTUS_VALMA",
                "LISAKOULUTUS_TALOUS",
                "preference1-discretionary-follow-up",
                "preference2-discretionary-follow-up",
                "preference3-discretionary-follow-up",
                "preference4-discretionary-follow-up",
                "preference5-discretionary-follow-up",
                "preference6-discretionary-follow-up",
                "preference1-discretionary",
                "preference2-discretionary",
                "preference3-discretionary",
                "preference4-discretionary",
                "preference5-discretionary",
                "preference6-discretionary"));

    LOG.info(
        "Vertaillaan avain-arvoja! Koostepalvelusta {} hakemusta, Supasta {} hakemusta. Ohitetaan {} avainta: {}.",
        koostepalvelusta.size(),
        suorituspalvelusta.size(),
        keysToIgnore.size(),
        String.join(", ", keysToIgnore));

    int totalHakemukset = koostepalvelusta.size();
    int matchingHakemukset = 0;
    int missingHakemukset = 0;
    int totalSameValues = 0;
    int totalDifferentValues = 0;
    int totalMissingValues = 0;
    int totalIgnoredValues = 0;

    Map<String, Integer> missingKeysCounts = new HashMap<>();

    Map<String, Integer> matchingKeysCounts = new HashMap<>();

    for (HakemusDTO koostepalveluHakemus : koostepalvelusta) {
      // Counters for this specific hakemus
      int sameValues = 0;
      int differentValues = 0;
      int missingValues = 0;
      int ignoredValues = 0;

      Optional<HakemusDTO> supaHakemus =
          suorituspalvelusta.stream()
              .filter(
                  suorituspalveluHakemus ->
                      suorituspalveluHakemus
                          .getHakemusoid()
                          .equals(koostepalveluHakemus.getHakemusoid()))
              .findFirst();

      if (supaHakemus.isPresent()) {
        matchingHakemukset++;
        LOG.info(
            "Löytyi vastaava hakemus sekä Koostepalvelusta että Suorituspalvelusta! {}",
            supaHakemus.get().getHakemusoid());

        List<AvainArvoDTO> supaArvot = supaHakemus.get().getAvaimet();
        List<AvainArvoDTO> koosteArvot = koostepalveluHakemus.getAvaimet();

        for (AvainArvoDTO koosteArvo : koosteArvot) {
          if (keysToIgnore.contains(koosteArvo.getAvain())) {
            ignoredValues++;
            continue;
          }

          Optional<AvainArvoDTO> supaArvo =
              supaArvot.stream()
                  .filter(aa -> aa.getAvain().equals(koosteArvo.getAvain()))
                  .findFirst();

          if (supaArvo.isPresent()) {
            if (koosteArvo.getArvo().equals(supaArvo.get().getArvo())) {
              sameValues++;
              matchingKeysCounts.merge(koosteArvo.getAvain(), 1, Integer::sum);
              LOG.trace(
                  "Supasta ja Koostepalvelusta löytyi hakemukselle {} sama arvo ({}) avaimelle {}. Jee.",
                  koostepalveluHakemus.getHakemusoid(),
                  koosteArvo.getArvo(),
                  koosteArvo.getAvain());
            } else {
              differentValues++;
              LOG.warn(
                  "Supasta ja Koostepalvelusta löytyi eri arvot hakemuksen {} avaimelle {}. kooste {} , supa {}",
                  koostepalveluHakemus.getHakemusoid(),
                  koosteArvo.getAvain(),
                  koosteArvo.getArvo(),
                  supaArvo.get().getArvo());
            }
          } else {
            missingValues++;
            missingKeysCounts.merge(koosteArvo.getAvain(), 1, Integer::sum);
            LOG.warn(
                "Koostepalvelusta löytyi hakemuksen {} avaimelle {} arvo {}, mutta Supasta ei palautunut vastaavaa!",
                koostepalveluHakemus.getHakemusoid(),
                koosteArvo.getAvain(),
                koosteArvo.getArvo());
          }
        }

        LOG.info(
            "Hakemuksen {} vertailu: samoja arvoja: {}, eriäviä arvoja: {}, puuttuvia arvoja: {}, ohitettuja arvoja: {}, arvoja yhteensä: {}",
            koostepalveluHakemus.getHakemusoid(),
            sameValues,
            differentValues,
            missingValues,
            ignoredValues,
            koosteArvot.size());

        totalSameValues += sameValues;
        totalDifferentValues += differentValues;
        totalMissingValues += missingValues;
        totalIgnoredValues += ignoredValues;

      } else {
        missingHakemukset++;
        LOG.info(
            "Koostepalvelusta palautui hakemus {}, mutta Supasta ei löytynyt vastaavaa.",
            koostepalveluHakemus.getHakemusoid());
      }
    }

    LOG.info(
        "Vertailun yhteenveto: yhteensä hakemuksia: {}, vastaavia hakemuksia: {}, puuttuvia hakemuksia: {}",
        totalHakemukset,
        matchingHakemukset,
        missingHakemukset);

    LOG.info(
        "Arvojen vertailun yhteenveto: samoja arvoja: {}, eriäviä arvoja: {}, puuttuvia arvoja: {}, ohitettuja arvoja: {}",
        totalSameValues,
        totalDifferentValues,
        totalMissingValues,
        totalIgnoredValues);

    // Puuttuvat avaimet kaikille hakemuksille yhteensä
    if (!missingKeysCounts.isEmpty()) {
      LOG.info(
          "Ohitettiin seuraavat avaimet: {}. Alla yhteenveto:", String.join(", ", keysToIgnore));
      missingKeysCounts.entrySet().stream()
          .sorted(Map.Entry.<String, Integer>comparingByValue().reversed())
          .forEach(
              entry ->
                  LOG.info("Avain: {}, puuttuu {} hakemuksessa", entry.getKey(), entry.getValue()));
    }

    // Täsmäävät avaimet kaikille hakemuksille yhteensä
    if (!matchingKeysCounts.isEmpty()) {
      LOG.info("Täsmäävien avainten yhteenveto:");
      matchingKeysCounts.entrySet().stream()
          .sorted(Map.Entry.<String, Integer>comparingByValue().reversed())
          .forEach(
              entry ->
                  LOG.info("Avain: {}, täsmää {} hakemuksessa", entry.getKey(), entry.getValue()));
    }
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

  private void suoritaLaskentaHakukohteelle(LaskentaDto laskenta, String hakukohdeOid) {

    // OPHSUPA-325 Väliaikainen ohitus koekutsulaskentojen ajaksi: käytetään Suorituspalvelua vain
    // 2026 perusopetuksen jälkeisen koulutuksen yhteishaulle.
    // boolean kaytetaanUudenSuorituspalvelunTietoja =
    //    laskenta.getHakuOid().equals("1.2.246.562.29.00000000000000075761");
    boolean kaytetaanUudenSuorituspalvelunTietoja =
        !hautJoilleKaytetaanSuoritusrekisterinTietoja.contains(laskenta.getHakuOid());

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

    LaskeDTO kaytettavaLaskeDTO;

    if (kaytetaanUudenSuorituspalvelunTietoja) {
      LOG.info("Käytetään haulle {} Suorituspalvelun tietoja!", laskenta.getHakuOid());
      SuorituspalveluValintadataDTO supastaHaetut =
          this.suorituspalveluAsyncResource.haeValintaData(laskenta.getHakuOid(), hakukohdeOid);
      List<HakemusDTO> hakemusDTOtSupasta = supastaHaetut.getValintaHakemukset();
      Set<String> supaHakemusOids =
          hakemusDTOtSupasta.stream().map(HakemusDTO::getHakemusoid).collect(Collectors.toSet());
      List<PistetietoWrapper> pistetiedot =
          valintapisteService.findValintapisteetForHakemukset(supaHakemusOids).getRight();
      combinePistetiedotWithHakemusDTOs(hakemusDTOtSupasta, pistetiedot);

      LOG.info(
          "Saatiin Suorituspalvelusta tiedot, yhteensä {} hakemusta.",
          supastaHaetut.getValintaHakemukset().size());
      LaskeDTO tiedotKoostepalvelusta =
          this.koostepalveluAsyncResource.haeLahtotiedot(
              laskenta, hakukohdeOid, retryHakemuksetJaOppijat, withHakijaRyhmat);
      combineKoskiJsonFromKoostepalvelutoSupaTiedot(
          hakemusDTOtSupasta, tiedotKoostepalvelusta.getHakemus());

      // Vertailulokituksia Koostepalvelun ja Suorituspalvelun välillä, nämä voidaan poistaa
      // testausvaiheen jälkeen
      vertaile(tiedotKoostepalvelusta.getHakemus(), supastaHaetut.getValintaHakemukset());
      // vertaileHarkinnanvaraisuus(
      //    tiedotKoostepalvelusta.getHakemus(), supastaHaetut.getValintaHakemukset());
      vertaileAvainMetatiedot(
          tiedotKoostepalvelusta.getHakemus(), supastaHaetut.getValintaHakemukset());
      // logAvainMetatiedot(tiedotKoostepalvelusta.getHakemus(), "Koostepalvelu");
      // logAvainMetatiedot(supastaHaetut.getValintaHakemukset(), "Supasta");

      // Yhdistetään Supasta tulevat HakemusDTO:t Koostepalvelusta tuleviin lähtötietoihin.
      kaytettavaLaskeDTO =
          new LaskeDTO(
              tiedotKoostepalvelusta.getUuid(),
              tiedotKoostepalvelusta.isKorkeakouluhaku(),
              tiedotKoostepalvelusta.isErillishaku(),
              tiedotKoostepalvelusta.getHakukohdeOid(),
              hakemusDTOtSupasta,
              tiedotKoostepalvelusta.getValintaperuste(),
              tiedotKoostepalvelusta.getHakijaryhmat());
    } else {
      LOG.info("Käytetään haulle {} Suoritusrekisterin tietoja!", laskenta.getHakuOid());
      LaskeDTO tiedotKoostepalvelusta =
          this.koostepalveluAsyncResource.haeLahtotiedot(
              laskenta, hakukohdeOid, retryHakemuksetJaOppijat, withHakijaRyhmat);
      kaytettavaLaskeDTO = tiedotKoostepalvelusta;
    }

    Instant laskeStart = Instant.now();
    LOG.info(
        "Haettiin lähtötiedot hakukohteelle "
            + hakukohdeOid
            + ", start: "
            + lahtotiedotStart
            + ", end: "
            + laskeStart
            + ", kaytetaanSupanTietoja: "
            + kaytetaanUudenSuorituspalvelunTietoja);
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
