package fi.vm.sade.valinta.kooste.valintalaskenta.actor;

import com.google.gson.Gson;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetFunktiokutsuDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintatapajonoJarjestyskriteereillaDTO;
import fi.vm.sade.service.valintaperusteet.dto.model.Funktionimi;
import fi.vm.sade.valinta.kooste.external.resource.koski.KoskiAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.koski.KoskiOppija;
import fi.vm.sade.valinta.kooste.util.HakemusWrapper;
import fi.vm.sade.valintalaskenta.domain.dto.SuoritustiedotDTO;
import java.time.LocalDate;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

@Service
public class KoskiService {
  private static final Logger LOG = LoggerFactory.getLogger(KoskiService.class);

  private static final Predicate<String> INCLUDE_ALL = s -> true;
  private static final Predicate<String> EXCLUDE_ALL = s -> false;
  private static final Gson GSON = new Gson();
  private final Predicate<String> koskiHakukohdeOidFilter;
  private final KoskiAsyncResource koskiAsyncResource;
  private final KoskiOpiskeluoikeusHistoryService koskiOpiskeluoikeusHistoryService;
  private final Set<Funktionimi> koskenFunktionimet;
  private final Set<String> koskenOpiskeluoikeusTyypit;

  @Autowired
  public KoskiService(
      @Value("${valintalaskentakoostepalvelu.laskenta.koskesta.haettavat.hakukohdeoidit:none}")
          String koskiHakukohdeOiditString,
      @Value("${valintalaskentakoostepalvelu.laskenta.funktionimet.joille.haetaan.tiedot.koskesta}")
          String koskenFunktionimetString,
      @Value(
              "${valintalaskentakoostepalvelu.laskenta.opiskeluoikeustyypit.joille.haetaan.tiedot.koskesta}")
          String koskenOpiskeluoikeusTyypitString,
      @Value("${valintalaskentakoostepalvelu.laskenta.kosken.historiapyyntojen.rinnakkaisuus}")
          Integer koskenHistoriapyyntojenRinnakkaisuus,
      KoskiAsyncResource koskiAsyncResource) {
    this.koskiAsyncResource = koskiAsyncResource;
    this.koskiHakukohdeOidFilter = resolveKoskiHakukohdeOidFilter(koskiHakukohdeOiditString);
    this.koskenFunktionimet = resolveKoskenFunktionimet(koskenFunktionimetString);
    this.koskenOpiskeluoikeusTyypit =
        resolveKoskenOpiskeluoikeudet(koskenOpiskeluoikeusTyypitString);
    this.koskiOpiskeluoikeusHistoryService =
        new KoskiOpiskeluoikeusHistoryService(
            koskenHistoriapyyntojenRinnakkaisuus, koskiAsyncResource);
  }

  /** Populoi tiedot löytyneet tiedot <code>suoritustiedotDTO</code> :hon. */
  public CompletableFuture<Map<String, KoskiOppija>> haeKoskiOppijat(
      String hakukohdeOid,
      CompletableFuture<List<ValintaperusteetDTO>> valintaperusteet,
      CompletableFuture<List<HakemusWrapper>> hakemukset,
      SuoritustiedotDTO suoritustiedotDTO,
      Date nyt) {
    if (koskiHakukohdeOidFilter.test(hakukohdeOid)) {
      return CompletableFuture.allOf(valintaperusteet, hakemukset)
          .thenComposeAsync(
              x -> {
                Collection<HakemusWrapper> hakemusWrappers = hakemukset.join();
                if (sisaltaaKoskiFunktioitaLaskennassaMukanaOlevissaJonoissa(
                    valintaperusteet.join(), nyt)) {
                  LocalDate paivaJonkaMukaisiaTietojaKoskiDatastaKaytetaan =
                      koskiOpiskeluoikeusHistoryService.etsiKoskiDatanLeikkuriPvm(
                          valintaperusteet.join(), hakukohdeOid);
                  return haeKoskiOppijat(
                      hakukohdeOid,
                      hakemusWrappers,
                      suoritustiedotDTO,
                      paivaJonkaMukaisiaTietojaKoskiDatastaKaytetaan);
                } else {
                  LOG.info(
                      "Ei haeta tietoja Koskesta hakukohteelle "
                          + hakukohdeOid
                          + " , koska siltä ei löydy Koski-tietoja käyttäviä valintaperusteita laskennassa mukana olevissa jonoissa.");
                  return CompletableFuture.completedFuture(Collections.emptyMap());
                }
              });
    } else {
      LOG.info(
          "Ei haeta tietoja Koskesta hakukohteelle "
              + hakukohdeOid
              + " , koska sitä ei ole listattu Koski-hakua varten.");
      return CompletableFuture.completedFuture(Collections.emptyMap());
    }
  }

  private CompletionStage<Map<String, KoskiOppija>> haeKoskiOppijat(
      String hakukohdeOid,
      Collection<HakemusWrapper> hakemusWrappers,
      SuoritustiedotDTO suoritustiedotDTO,
      LocalDate paivaJonkaMukaisiaTietojaKoskiDatastaKaytetaan) {
    LOG.info(
        String.format(
            "Haetaan Koskesta tiedot %d oppijalle hakukohteen %s laskemista varten.",
            hakemusWrappers.size(), hakukohdeOid));
    List<String> oppijanumerotJoiltaKoskiOpiskeluoikeudetPuuttuvat =
        hakemusWrappers.stream()
            .map(HakemusWrapper::getPersonOid)
            .filter(oppijanumero -> !suoritustiedotDTO.onKoskiopiskeluoikeudet(oppijanumero))
            .collect(Collectors.toList());
    int maaraJoilleTiedotJoLoytyvat =
        hakemusWrappers.size() - oppijanumerotJoiltaKoskiOpiskeluoikeudetPuuttuvat.size();
    return koskiAsyncResource
        .findKoskiOppijat(oppijanumerotJoiltaKoskiOpiskeluoikeudetPuuttuvat)
        .thenApplyAsync(
            koskioppijat -> {
              LOG.info(
                  String.format(
                      "Saatiin Koskesta %s uuden oppijan tiedot, kun haettiin %d/%d oppijalle (%s:lle oli jo haettu tiedot) hakukohteen %s laskemista varten.",
                      koskioppijat.size(),
                      oppijanumerotJoiltaKoskiOpiskeluoikeudetPuuttuvat.size(),
                      hakemusWrappers.size(),
                      maaraJoilleTiedotJoLoytyvat,
                      hakukohdeOid));
              koskioppijat.forEach(
                  o -> o.poistaMuuntyyppisetOpiskeluoikeudetKuin(koskenOpiskeluoikeusTyypit));
              koskiOpiskeluoikeusHistoryService.haeVanhemmatOpiskeluoikeudetTarvittaessa(
                  koskioppijat, paivaJonkaMukaisiaTietojaKoskiDatastaKaytetaan);
              Map<String, KoskiOppija> koskiOppijatOppijanumeroittain =
                  koskioppijat.stream()
                      .collect(Collectors.toMap(KoskiOppija::getOppijanumero, Function.identity()));
              oppijanumerotJoiltaKoskiOpiskeluoikeudetPuuttuvat.forEach(
                  oppijanumero -> {
                    KoskiOppija loytynytOppija = koskiOppijatOppijanumeroittain.get(oppijanumero);
                    if (loytynytOppija != null) {
                      suoritustiedotDTO.asetaKoskiopiskeluoikeudet(
                          oppijanumero, GSON.toJson(loytynytOppija.getOpiskeluoikeudet()));
                    } else {
                      suoritustiedotDTO.asetaKoskiopiskeluoikeudet(oppijanumero, "[]");
                    }
                  });
              return koskiOppijatOppijanumeroittain;
            });
  }

  private boolean sisaltaaKoskiFunktioitaLaskennassaMukanaOlevissaJonoissa(
      List<ValintaperusteetDTO> valintaperusteet, Date nyt) {
    if (koskenFunktionimet.isEmpty()) {
      return false;
    }
    return valintaperusteet.stream()
        .anyMatch(
            valintaperusteetDTO ->
                valintaperusteetDTO.getValinnanVaihe().getValintatapajono().stream()
                    .anyMatch(
                        jono ->
                            mukanaLaskennassa(jono, nyt)
                                && jono.getJarjestyskriteerit().stream()
                                    .anyMatch(
                                        kriteeri ->
                                            sisaltaaKoskiFunktioita(kriteeri.getFunktiokutsu()))));
  }

  private boolean mukanaLaskennassa(ValintatapajonoJarjestyskriteereillaDTO jono, Date nyt) {
    return jono.getEiLasketaPaivamaaranJalkeen() == null
        || jono.getEiLasketaPaivamaaranJalkeen().after(nyt);
  }

  private boolean sisaltaaKoskiFunktioita(ValintaperusteetFunktiokutsuDTO funktiokutsu) {
    if (koskenFunktionimet.contains(funktiokutsu.getFunktionimi())) {
      return true;
    }
    return funktiokutsu.getFunktioargumentit().stream()
        .anyMatch(argumentti -> sisaltaaKoskiFunktioita(argumentti.getFunktiokutsu()));
  }

  private static Predicate<String> resolveKoskiHakukohdeOidFilter(
      String koskiHakukohdeOiditString) {
    if (StringUtils.isBlank(koskiHakukohdeOiditString)) {
      LOG.info(
          "Saatiin '"
              + koskiHakukohdeOiditString
              + "' Koskesta haettaviksi hakukohdeoideiksi => ei haeta ollenkaan tietoja Koskesta.");
      return EXCLUDE_ALL;
    }
    if ("ALL".equals(koskiHakukohdeOiditString)) {
      LOG.info(
          "Saatiin '"
              + koskiHakukohdeOiditString
              + "' Koskesta haettaviksi hakukohdeoideiksi => haetaan tiedot Koskesta kaikille hakukohteille, "
              + "joille löytyy Koski-dataa käyttäviä valintaperusteita.");
      return INCLUDE_ALL;
    }
    List<String> hakukohdeOids = Arrays.asList(koskiHakukohdeOiditString.split(","));
    LOG.info(
        "Saatiin '"
            + koskiHakukohdeOiditString
            + "' Koskesta haettaviksi hakukohdeoideiksi => haetaan Koskesta tiedot seuraaville hakukohteille: "
            + hakukohdeOids
            + " , jos niistä löytyy Koski-dataa käyttäviä valintaperusteita.");
    return hakukohdeOids::contains;
  }

  private static Set<Funktionimi> resolveKoskenFunktionimet(String koskenFunktionimetString) {
    if (StringUtils.isBlank(koskenFunktionimetString)) {
      LOG.info(
          "Saatiin '"
              + koskenFunktionimetString
              + "' funktionimiksi, joille haetaan dataa Koskesta => ei haeta ollenkaan tietoja Koskesta.");
      return Collections.emptySet();
    }
    Set<Funktionimi> funktionimet =
        Arrays.stream(koskenFunktionimetString.split(","))
            .map(Funktionimi::valueOf)
            .collect(Collectors.toSet());
    LOG.info(
        "Saatiin '"
            + koskenFunktionimetString
            + "' funktionimiksi, joille haetaan dataa Koskesta => haetaan Koskesta tietoja vain hakukohteille, "
            + "joiden valintaperusteissa käytetään seuraavannimisiä funktioita: "
            + funktionimet);
    return funktionimet;
  }

  private static Set<String> resolveKoskenOpiskeluoikeudet(
      String koskenOpiskeluoikeusTyypitString) {
    if (StringUtils.isBlank(koskenOpiskeluoikeusTyypitString)) {
      LOG.info(
          "Saatiin '"
              + koskenOpiskeluoikeusTyypitString
              + "' opiskeluoikeuksiksi, joille haetaan dataa Koskesta => ei haeta ollenkaan tietoja Koskesta.");
      return Collections.emptySet();
    }
    Set<String> opiskeluoikeusTyypit =
        Arrays.stream(koskenOpiskeluoikeusTyypitString.split(",")).collect(Collectors.toSet());
    LOG.info(
        "Saatiin '"
            + koskenOpiskeluoikeusTyypitString
            + "' opiskeluoikeuksien tyypeiksi, joille haetaan dataa Koskesta => haetaan Koskesta tietoja vain opiskeluoikeuksista, "
            + "tyypin koodiarvo on jokin seuraavista: "
            + opiskeluoikeusTyypit);
    return opiskeluoikeusTyypit;
  }
}
