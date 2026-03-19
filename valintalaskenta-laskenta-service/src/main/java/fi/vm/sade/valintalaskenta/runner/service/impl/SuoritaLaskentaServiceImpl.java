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
import fi.vm.sade.valintalaskenta.laskenta.resource.ValintalaskentaResourceImpl;
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
  private final CloudWatchClient cloudWatchClient;
  private final EcsTaskManager ecsTaskManager;

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
      CloudWatchClient cloudWatchClient,
      EcsTaskManager ecsTaskManager,
      @Value("${environment.name}") String environmentName) {
    this.valintalaskentaResource = valintalaskentaResource;
    this.koostepalveluAsyncResource = koostepalveluAsyncResource;
    this.valintaperusteetAsyncResource = valintaperusteetAsyncResource;
    this.suorituspalveluAsyncResource = suorituspalveluAsyncResource;
    this.cloudWatchClient = cloudWatchClient;
    this.ecsTaskManager = ecsTaskManager;
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
    if (laskenta
        .getTyyppi()
        .equals(fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaTyyppi.VALINTARYHMA)) {
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

  public void logAvainMetatiedot(List<HakemusDTO> hakemukset, String lahde) {
    LOG.info("Lokitetaan avainmetatiedot {} hakemuksesta (lähde {})", hakemukset.size(), lahde);

    int totalHakemukset = hakemukset.size();
    int hakemusetWithMetatiedot = 0;
    int totalMetatiedot = 0;

    Map<String, Integer> metatiedotCounts = new HashMap<>();

    for (HakemusDTO hakemus : hakemukset) {
      List<AvainMetatiedotDTO> avainMetatiedot = hakemus.getAvainMetatiedotDTO();

      if (avainMetatiedot == null || avainMetatiedot.isEmpty()) {
        LOG.trace("Hakemuksella {} ei ole avainmetatietoja", hakemus.getHakemusoid());
        continue;
      }

      hakemusetWithMetatiedot++;
      LOG.info(
          "Hakemuksella {} on {} avainmetatietoa", hakemus.getHakemusoid(), avainMetatiedot.size());

      for (AvainMetatiedotDTO metatiedot : avainMetatiedot) {
        String avain = metatiedot.getAvain();
        List<Map<String, String>> metatiedotList = metatiedot.getMetatiedot();

        totalMetatiedot++;
        metatiedotCounts.merge(avain, 1, Integer::sum);

        if (metatiedotList == null || metatiedotList.isEmpty()) {
          LOG.warn(
              "Hakemuksella {} avaimella {} ei ole metatietoja", hakemus.getHakemusoid(), avain);
          continue;
        }

        LOG.info(
            "Hakemuksella {} avaimella {} on {} metatietoriviä",
            hakemus.getHakemusoid(),
            avain,
            metatiedotList.size());

        for (int i = 0; i < metatiedotList.size(); i++) {
          Map<String, String> metatietoMap = metatiedotList.get(i);
          LOG.info(
              "Hakemuksella {} avaimella {} metatietoriviä {}: {}",
              hakemus.getHakemusoid(),
              avain,
              i + 1,
              metatietoMap);

          int finalI = i;
          metatietoMap.forEach(
              (key, value) ->
                  LOG.info(
                      "Hakemuksella {} avaimella {} metatietoriviä {} avain: {}, arvo: {}",
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
                "LISAKOULUTUS_TALOUS"));

    LOG.info(
        "Vertaillaan! Koostepalvelusta {} hakemusta, Supasta {} hakemusta. Ohitetaan {} avainta: {}.",
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
      // Todo, lisätään vertailu myös avainmetatiedoille siinä vaiheessa, kun ne on lisätty Supan
      // päähän.
      koostepalveluHakemus.getAvainMetatiedotDTO().stream()
          .filter(am -> !keysToIgnore.contains(am.getAvain()))
          .forEach(
              am -> {
                LOG.info(
                    "Hakemus {} avainmetatieto {} - {}",
                    koostepalveluHakemus.getHakemusoid(),
                    am.getAvain(),
                    am.getMetatiedot());
              });

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
    SuorituspalveluValintadataDTO supastaHaetut =
        this.suorituspalveluAsyncResource.haeValintaData(laskenta.getHakuOid(), hakukohdeOid);
    LOG.info(
        "Saatiin Suorituspalvelusta tiedot, yhteensä {} hakemusta.",
        supastaHaetut.getValintaHakemukset().size());
    LaskeDTO lahtotiedot =
        this.koostepalveluAsyncResource.haeLahtotiedot(
            laskenta, hakukohdeOid, retryHakemuksetJaOppijat, withHakijaRyhmat);
    vertaile(lahtotiedot.getHakemus(), supastaHaetut.getValintaHakemukset());
    vertaileHarkinnanvaraisuus(lahtotiedot.getHakemus(), supastaHaetut.getValintaHakemukset());
    logAvainMetatiedot(lahtotiedot.getHakemus(), "Koostepalvelu");
    logAvainMetatiedot(supastaHaetut.getValintaHakemukset(), "Supasta");
    // Todo, tehdään vertailua, mutta laskenta käynnistetään Supan arvoilla.
    LaskeDTO laskeDtoSupanTiedoilla =
        new LaskeDTO(
            lahtotiedot.getUuid(),
            lahtotiedot.isKorkeakouluhaku(),
            lahtotiedot.isErillishaku(),
            lahtotiedot.getHakukohdeOid(),
            supastaHaetut.getValintaHakemukset(),
            lahtotiedot.getValintaperuste(),
            lahtotiedot.getHakijaryhmat());
    Instant laskeStart = Instant.now();

    LOG.info(
        "Haettiin lähtötiedot hakukohteelle "
            + hakukohdeOid
            + ", start: "
            + lahtotiedotStart
            + ", end: "
            + laskeStart);
    laske.apply(laskeDtoSupanTiedoilla);
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
