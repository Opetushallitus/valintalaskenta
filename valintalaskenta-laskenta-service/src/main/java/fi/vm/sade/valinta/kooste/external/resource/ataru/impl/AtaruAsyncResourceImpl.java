package fi.vm.sade.valinta.kooste.external.resource.ataru.impl;

import com.google.common.collect.Lists;
import com.google.gson.reflect.TypeToken;
import fi.vm.sade.valinta.kooste.external.resource.ataru.AtaruAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.ataru.dto.AtaruHakemus;
import fi.vm.sade.valinta.kooste.external.resource.ataru.dto.AtaruHakemusPrototyyppi;
import fi.vm.sade.valinta.kooste.external.resource.ataru.dto.AtaruSyntheticApplicationResponse;
import fi.vm.sade.valinta.kooste.external.resource.koodisto.KoodistoCachedAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.koodisto.dto.Koodi;
import fi.vm.sade.valinta.kooste.external.resource.oppijanumerorekisteri.OppijanumerorekisteriAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.oppijanumerorekisteri.dto.HenkiloPerustietoDto;
import fi.vm.sade.valinta.kooste.external.resource.oppijanumerorekisteri.dto.KansalaisuusDto;
import fi.vm.sade.valinta.kooste.external.resource.viestintapalvelu.RestCasClient;
import fi.vm.sade.valinta.kooste.url.UrlConfiguration;
import fi.vm.sade.valinta.kooste.util.AtaruHakemusWrapper;
import fi.vm.sade.valinta.kooste.util.CompletableFutureUtil;
import fi.vm.sade.valinta.kooste.util.HakemusWrapper;
import io.reactivex.Observable;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

@Service
public class AtaruAsyncResourceImpl implements AtaruAsyncResource {
  private static final int SUITABLE_ATARU_HAKEMUS_CHUNK_SIZE = 1000;
  private final Logger LOG = LoggerFactory.getLogger(getClass());
  private final RestCasClient casClient;
  private final OppijanumerorekisteriAsyncResource oppijanumerorekisteriAsyncResource;
  private final KoodistoCachedAsyncResource koodistoCachedAsyncResource;
  private final UrlConfiguration urlConfiguration;

  @Autowired
  public AtaruAsyncResourceImpl(
      @Qualifier("AtaruCasClient") RestCasClient casClient,
      OppijanumerorekisteriAsyncResource oppijanumerorekisteriAsyncResource,
      KoodistoCachedAsyncResource koodistoCachedAsyncResource) {
    this.casClient = casClient;
    this.urlConfiguration = UrlConfiguration.getInstance();
    this.oppijanumerorekisteriAsyncResource = oppijanumerorekisteriAsyncResource;
    this.koodistoCachedAsyncResource = koodistoCachedAsyncResource;
  }

  private CompletableFuture<List<HakemusWrapper>> getApplications(
      String hakukohdeOid, List<String> hakemusOids, Boolean withHarkinnanvaraisuustieto) {
    if (hakukohdeOid == null && hakemusOids.isEmpty()) {
      return CompletableFuture.completedFuture(Collections.emptyList());
    }
    return getApplicationsInChunks(hakukohdeOid, hakemusOids, withHarkinnanvaraisuustieto)
        .thenComposeAsync(
            hakemukset -> {
              if (hakemukset.isEmpty()) {
                return CompletableFuture.completedFuture(Collections.emptyList());
              } else {
                if (hakukohdeOid != null) {
                  LOG.info(
                      "Saatiin {} hakemusta hakukohteelle {}, haetaan henkilöt",
                      hakemukset.size(),
                      hakukohdeOid);
                }
                return getHenkilot(hakemukset)
                    .thenComposeAsync(
                        henkilot -> {
                          long puuttuvatHenkilotCount =
                              hakemukset.stream()
                                  .map(AtaruHakemus::getPersonOid)
                                  .filter(personOid -> !henkilot.containsKey(personOid))
                                  .distinct()
                                  .count();
                          if (puuttuvatHenkilotCount > 0) {
                            hakemukset.forEach(
                                hak -> {
                                  if (!henkilot.keySet().contains(hak.getPersonOid())) {
                                    LOG.error(
                                        "Hakemuksen {} henkilöä {} ei löytynyt oppijanumerorekisteristä.",
                                        hak.getHakemusOid(),
                                        hak.getPersonOid());
                                  }
                                });
                            throw new RuntimeException(
                                String.format(
                                    "Hakukohteelle %s löytyi %s puuttuvaa henkilöä.",
                                    hakukohdeOid, puuttuvatHenkilotCount));
                          }
                          ensureKansalaisuus(henkilot);
                          Stream<String> asuinmaaKoodit =
                              hakemukset.stream()
                                  .map(h -> h.getKeyValues().get("country-of-residence"));
                          Stream<String> toisenasteensuoritusmaaKoodit =
                              hakemukset.stream()
                                  .map(
                                      h ->
                                          h.getKeyValues()
                                              .get("secondary-completed-base-education–country"));
                          Stream<String> kansalaisuusKoodit =
                              henkilot.values().stream()
                                  .flatMap(
                                      h ->
                                          h.getKansalaisuus().stream()
                                              .map(KansalaisuusDto::getKansalaisuusKoodi));
                          return getMaakoodit(
                                  asuinmaaKoodit, kansalaisuusKoodit, toisenasteensuoritusmaaKoodit)
                              .thenApplyAsync(
                                  maakoodit ->
                                      hakemukset.stream()
                                          .map(hakemusToHakemusWrapper(henkilot, maakoodit))
                                          .collect(Collectors.toList()));
                        });
              }
            });
  }

  private CompletableFuture<List<AtaruHakemus>> getApplicationChunk(
      String hakukohdeOid, List<String> hakemusOids, Boolean withHarkinnanvaraisuustieto) {
    Map<String, String> query = new HashMap<>();
    if (hakukohdeOid != null) {
      query.put("hakukohdeOid", hakukohdeOid);
    }
    if (withHarkinnanvaraisuustieto) {
      query.put("harkinnanvaraisuustiedotHakutoiveille", "true");
    }

    return this.casClient.post(
        this.urlConfiguration.url("ataru.applications.by-hakukohde", query),
        new TypeToken<>() {},
        hakemusOids,
        Collections.emptyMap(),
        120000);
  }

  private CompletableFuture<List<AtaruHakemus>> getApplicationsInChunks(
      String hakukohdeOid, List<String> hakemusOids, Boolean withHarkinnanvaraisuustieto) {
    if (hakemusOids.isEmpty()) {
      return getApplicationChunk(hakukohdeOid, hakemusOids, withHarkinnanvaraisuustieto);
    } else {
      return CompletableFutureUtil.sequence(
              Lists.partition(hakemusOids, SUITABLE_ATARU_HAKEMUS_CHUNK_SIZE).stream()
                  .map(
                      chunk ->
                          getApplicationChunk(hakukohdeOid, chunk, withHarkinnanvaraisuustieto))
                  .collect(Collectors.toList()))
          .thenApplyAsync(
              chunks -> chunks.stream().flatMap(List::stream).collect(Collectors.toList()));
    }
  }

  private void ensureKansalaisuus(Map<String, HenkiloPerustietoDto> henkilot) {
    List<String> missingKansalaisuus =
        henkilot.entrySet().stream()
            .filter(e -> e.getValue().getKansalaisuus().isEmpty())
            .map(Map.Entry::getKey)
            .collect(Collectors.toList());
    if (!missingKansalaisuus.isEmpty()) {
      LOG.warn(
          String.format(
              "Kansalaisuus missing from henkilöt: %s", String.join(", ", missingKansalaisuus)));
    }
  }

  private CompletableFuture<Map<String, HenkiloPerustietoDto>> getHenkilot(
      List<AtaruHakemus> hakemukset) {
    return oppijanumerorekisteriAsyncResource.haeHenkilot(
        hakemukset.stream()
            .map(AtaruHakemus::getPersonOid)
            .distinct()
            .collect(Collectors.toList()));
  }

  private Function<AtaruHakemus, AtaruHakemusWrapper> hakemusToHakemusWrapper(
      Map<String, HenkiloPerustietoDto> henkilot, Map<String, Koodi> maakoodit) {
    return hakemus -> {
      HenkiloPerustietoDto henkilo = henkilot.get(hakemus.getPersonOid());
      List<String> kansalaisuudet =
          henkilo.getKansalaisuus().stream()
              .map(k -> maakoodit.get(k.getKansalaisuusKoodi()).getKoodiArvo())
              .collect(Collectors.toList());
      Map<String, String> newKeyValues = new HashMap<>(hakemus.getKeyValues());

      String asuinmaaISO =
          maakoodit.get(hakemus.getKeyValues().get("country-of-residence")).getKoodiArvo();
      newKeyValues.replace("country-of-residence", asuinmaaISO);

      String toisenasteensuoritusmaa =
          hakemus.getKeyValues().get("secondary-completed-base-education–country");

      try {
        if (StringUtils.isNotBlank(toisenasteensuoritusmaa)) {
          String toisenasteensuoritusmaaISO =
              maakoodit
                  .get(hakemus.getKeyValues().get("secondary-completed-base-education–country"))
                  .getKoodiArvo();
          newKeyValues.replace(
              "secondary-completed-base-education–country", toisenasteensuoritusmaaISO);
        }
      } catch (Exception e) {
        LOG.error(
            "Toisen asteen suoritusmaata {} ei löytynyt koodistosta, hakemus {}",
            toisenasteensuoritusmaa,
            hakemus);
      }

      AtaruHakemus h =
          new AtaruHakemus(
              hakemus.getHakemusOid(),
              hakemus.getPersonOid(),
              hakemus.getHakuOid(),
              hakemus.getHakutoiveet(),
              hakemus.getMaksuvelvollisuus(),
              hakemus.getAsiointikieli(),
              newKeyValues);
      AtaruHakemusWrapper wrapper = new AtaruHakemusWrapper(h, henkilo);
      wrapper.setKansalaisuus(kansalaisuudet);
      return wrapper;
    };
  }

  private CompletableFuture<Map<String, Koodi>> getMaakoodit(
      Stream<String> asuinmaaKoodit,
      Stream<String> kansalaisuusKoodit,
      Stream<String> toisenasteensuoritusmaaKoodit) {
    Stream<String> toisenasteensuoritusmaaKooditOrEmpty =
        toisenasteensuoritusmaaKoodit == null ? Stream.empty() : toisenasteensuoritusmaaKoodit;
    return CompletableFutureUtil.sequence(
        Stream.concat(
                Stream.concat(toisenasteensuoritusmaaKooditOrEmpty, asuinmaaKoodit),
                kansalaisuusKoodit)
            .filter(Objects::nonNull)
            .distinct()
            .collect(
                Collectors.toMap(
                    koodiArvo -> koodiArvo,
                    koodiArvo ->
                        koodistoCachedAsyncResource.maatjavaltiot2ToMaatjavaltiot1(
                            "maatjavaltiot2_" + koodiArvo))));
  }

  @Override
  public CompletableFuture<List<HakemusWrapper>> getApplicationsByHakukohde(String hakukohdeOid) {
    return getApplications(hakukohdeOid, Lists.newArrayList(), false);
  }

  @Override
  public CompletableFuture<List<HakemusWrapper>> getApplicationsByHakukohde(
      String hakukohdeOid, boolean withHarkinnanvaraisuustieto) {
    return getApplications(hakukohdeOid, Lists.newArrayList(), withHarkinnanvaraisuustieto);
  }

  @Override
  public CompletableFuture<List<HakemusWrapper>> getApplicationsByOids(List<String> oids) {
    return getApplications(null, oids, false);
  }

  @Override
  public CompletableFuture<List<HakemusWrapper>> getApplicationsByOidsWithHarkinnanvaraisuustieto(
      List<String> oids) {
    return getApplications(null, oids, true);
  }

  @Override
  public Observable<List<AtaruSyntheticApplicationResponse>> putApplicationPrototypes(
      Collection<AtaruHakemusPrototyyppi> hakemusPrototyypit) {
    String url = this.urlConfiguration.url("ataru.post-synthetic-applications");

    return Observable.fromFuture(
        this.casClient.post(
            url, new TypeToken<>() {}, hakemusPrototyypit, Collections.emptyMap(), 60 * 60 * 1000));
  }
}
