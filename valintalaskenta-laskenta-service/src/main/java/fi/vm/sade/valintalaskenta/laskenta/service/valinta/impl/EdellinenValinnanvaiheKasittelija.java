package fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl;

import static fi.vm.sade.service.valintaperusteet.dto.model.ValinnanVaiheTyyppi.VALINTAKOE;
import static fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila.HYLATTY;
import static fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA;
import static fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila.MAARITTELEMATON;
import static fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila.VIRHE;
import static fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen.OSALLISTUU;

import com.google.common.collect.Collections2;
import com.google.common.collect.Sets;
import fi.vm.sade.service.valintaperusteet.dto.ValintakoeCreateDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Hylattytila;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Tila;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Virhetila;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import fi.vm.sade.valintalaskenta.domain.valinta.MuokattuJonosija;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Valintakoe;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.tulos.dao.MuokattuJonosijaDAO;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class EdellinenValinnanvaiheKasittelija {
  private static final Logger LOG =
      LoggerFactory.getLogger(EdellinenValinnanvaiheKasittelija.class);

  private final MuokattuJonosijaDAO muokattuJonosijaDAO;

  @Autowired
  public EdellinenValinnanvaiheKasittelija(MuokattuJonosijaDAO muokattuJonosijaDAO) {
    this.muokattuJonosijaDAO = muokattuJonosijaDAO;
  }

  /**
   * Määrittää, onko hakemus hyväksyttävissä edellisen valinnan vaiheen mukaan. Hakemus on
   * hyväksyttävissä, jos se on ollut hyväksyttävissä ainakin yhdessä edellisen valinnan vaiheen
   * valintatapajonossa.
   */
  public TilaJaSelite hakemusHyvaksyttavissaEdellisenValinnanvaiheenMukaan(
      final String hakemusOid, Valinnanvaihe edellinenValinnanvaihe) {
    return hakemusHyvaksyttavissaEdellisenValinnanvaiheenMukaan(
        hakemusOid, edellinenValinnanvaihe, Optional.empty(), Optional.empty());
  }

  private TilaJaSelite hakemusHyvaksyttavissaEdellisenValinnanvaiheenMukaan(
      final String hakemusOid,
      Valinnanvaihe edellinenValinnanvaihe,
      Optional<ValintaperusteetDTO> valintaperusteetDTO,
      Optional<ValintakoeOsallistuminen> vanhatOsallistumiset) {
    // Jos edellisessä valinnan vaiheessa ei ole yhtään valintatapajonoa, voidaan olettaa, että
    // hakemus
    // on hyväksyttävissä
    if (edellinenValinnanvaihe == null || edellinenValinnanvaihe.getValintatapajono().isEmpty()) {
      return new TilaJaSelite(HYVAKSYTTAVISSA, new HashMap<>());
    }

    Optional<TilaJaSelite> hyvaksyttavaKohdekohtaiseenKokeenVuoksi =
        kutsuttavaKohdekohtaiseenKokeeseenVaikkaHylattyValisijoittelussa(
            hakemusOid, edellinenValinnanvaihe, valintaperusteetDTO, vanhatOsallistumiset);
    if (hyvaksyttavaKohdekohtaiseenKokeenVuoksi.isPresent()) {
      return hyvaksyttavaKohdekohtaiseenKokeenVuoksi.get();
    }

    List<TilaJaSelite> tilat = new ArrayList<>();
    for (final Valintatapajono jono : edellinenValinnanvaihe.getValintatapajono()) {
      Jonosija jonosija = getJonosijaForHakemus(hakemusOid, jono.getJonosijatAsList());

      TilaJaSelite tilaJonossa;
      if (jonosija == null) {
        // Jos hakemus ei ole ollut mukana edellisessä valinnan vaiheessa, hakemus ei voi tulla
        // hyväksyttäväksi tässä valinnan vaiheessa.
        return new TilaJaSelite(
            VIRHE,
            suomenkielinenMap(
                "Hakemus ei ole ollut mukana laskennassa edellisessä valinnan vaiheessa"));
      } else if (jonosija.getJarjestyskriteeritulokset().jarjestyskriteeritulokset.isEmpty()) {
        // Mitä tehdään, jos hakemukselle ei ole laskentatulosta? Kai se on hyväksyttävissä
        tilaJonossa =
            new TilaJaSelite(
                HYVAKSYTTAVISSA, suomenkielinenMap("Hakemukselle ei ole laskentatulosta jonossa"));
      } else {
        Jarjestyskriteeritulos tulos =
            jonosija.getJarjestyskriteeritulokset().jarjestyskriteeritulokset.get(0);

        Optional<MuokattuJonosija> muokattuJonosija =
            Optional.ofNullable(
                muokattuJonosijaDAO.readByValintatapajonoOid(
                    jono.getValintatapajonoOid(), hakemusOid));
        muokattuJonosija.ifPresent(
            mj -> {
              Optional<Jarjestyskriteeritulos> muokattuTulos =
                  mj.getJarjestyskriteerit().stream()
                      .filter(j -> j.getPrioriteetti() == tulos.getPrioriteetti())
                      .findFirst();
              muokattuTulos.ifPresent(
                  m -> {
                    tulos.setTila(m.getTila());
                    tulos.setArvo(m.getArvo());
                  });
            });

        tilaJonossa = new TilaJaSelite(tulos.getTila(), tulos.getKuvaus());
      }

      tilat.add(tilaJonossa);
    }

    // Filtteroidaan kaikki HYVAKSYTTAVISSA-tilat. Jos yksikin tällainen löytyy, hakemus on
    // hyväksyttävissä myös tässä valinnan vaiheessa.
    Collection<TilaJaSelite> filtteroidutTilat =
        Collections2.filter(
            tilat,
            edellinenValinnanvaiheTila ->
                HYVAKSYTTAVISSA.equals(edellinenValinnanvaiheTila.getTila()));

    if (filtteroidutTilat.isEmpty()) {
      Map<String, String> hylkaysSelitteet = tilat.get(tilat.size() - 1).getSelite();
      String tekninenSelite =
          "Hakemus ei ole hyväksyttävissä yhdessäkään edellisen valinnan vaiheen valintatapajonossa";
      return new TilaJaSelite(HYLATTY, hylkaysSelitteet, tekninenSelite);
    } else {
      return new TilaJaSelite(HYVAKSYTTAVISSA, new HashMap<>());
    }
  }

  private Sets.SetView<String> paatteleKoetunnisteetJotkaOnVainTallaHakukohteella(
      ValintaperusteetDTO valintaperusteetDTO, ValintakoeOsallistuminen vanhatOsallistumiset) {
    Set<String> tamanVaiheenKoetunnisteet =
        valintaperusteetDTO.getValinnanVaihe().getValintakoe().stream()
            .map(ValintakoeCreateDTO::getTunniste)
            .collect(Collectors.toSet());
    Set<String> muidenkohteidenKoetunnisteet =
        vanhatOsallistumiset.getHakutoiveet().stream()
            .filter(ht -> !ht.getHakukohdeOid().equals(valintaperusteetDTO.getHakukohdeOid()))
            .flatMap(ht -> ht.getValintakoeValinnanvaiheet().stream())
            .flatMap(vv -> vv.getValintakokeet().stream())
            .map(Valintakoe::getValintakoeTunniste)
            .collect(Collectors.toSet());
    return Sets.difference(tamanVaiheenKoetunnisteet, muidenkohteidenKoetunnisteet);
  }

  private Jonosija getJonosijaForHakemus(final String hakemusOid, final List<Jonosija> jonosijat) {
    Collection<Jonosija> filtteroidutJonosijat =
        Collections2.filter(jonosijat, jonosija -> jonosija.getHakemusOid().equals(hakemusOid));
    return filtteroidutJonosijat.isEmpty() ? null : filtteroidutJonosijat.iterator().next();
  }

  private Map<String, String> suomenkielinenMap(String teksti) {
    Map<String, String> vastaus = new HashMap<>();
    vastaus.put("FI", teksti);
    return vastaus;
  }

  /** Selvittää laskennan tilan edellisen vaiheen tilan sekä varsinaisen lasketun tilan mukaan */
  public TilaJaSelite tilaEdellisenValinnanvaiheenTilanMukaan(
      Tila laskettuTila, TilaJaSelite edellisenVaiheentila) {
    TilaJaSelite palautettavaTila = null;

    // Jos edellinen vaiheen mukaan hakija ei ole hyväksyttävissä, asetetaan tilaksi hylätty.
    // Jos taas edellisen vaiheen mukaan hakija on hyväksyttävissä, käytetään laskettua tilaa.
    if (!HYVAKSYTTAVISSA.equals(edellisenVaiheentila.getTila())) {
      palautettavaTila =
          new TilaJaSelite(edellisenVaiheentila.getTila(), edellisenVaiheentila.getSelite());
    } else if (Tila.Tilatyyppi.HYLATTY.equals(laskettuTila.getTilatyyppi())) {
      if (laskettuTila instanceof Hylattytila) {
        palautettavaTila =
            new TilaJaSelite(
                HYLATTY,
                ((Hylattytila) laskettuTila).getKuvaus(),
                ((Hylattytila) laskettuTila).getTekninenKuvaus());
      }
    } else if (Tila.Tilatyyppi.VIRHE.equals(laskettuTila.getTilatyyppi())) {
      if (laskettuTila instanceof Virhetila) {
        palautettavaTila = new TilaJaSelite(VIRHE, ((Virhetila) laskettuTila).getKuvaus());
      }
    } else if (Tila.Tilatyyppi.HYVAKSYTTAVISSA.equals(laskettuTila.getTilatyyppi())) {
      palautettavaTila = new TilaJaSelite(HYVAKSYTTAVISSA, new HashMap<>());
    } else {

      // Jos jostakin syystä tilaa ei pystytä selvittämään, palautetaan määrittelemätön-tila
      palautettavaTila = new TilaJaSelite(MAARITTELEMATON, new HashMap<>());
    }

    return palautettavaTila;
  }

  /**
   * Laskee järjestyskriteerin tilan varsinaisen lasketun tilan sekä sen mukaan, onko hakemus
   * hyväksyttävissä edellisen valinnan vaiheen tilan mukaan
   *
   * @param hakemusOid hakemus OID
   * @param laskettuTila järjestyskriteerille laskennasta saatu tila
   * @param edellinenValinnanvaihe edellinen valinnan vaihe
   * @param valintaperusteetDTO laskettavan hakukohteen valintaperusteet, jos tiedossa
   * @param vanhatOsallistumiset @return järjestyskriteerin tila
   */
  public TilaJaSelite tilaEdellisenValinnanvaiheenMukaan(
      String hakemusOid,
      Tila laskettuTila,
      Valinnanvaihe edellinenValinnanvaihe,
      Optional<ValintaperusteetDTO> valintaperusteetDTO,
      Optional<ValintakoeOsallistuminen> vanhatOsallistumiset) {
    return tilaEdellisenValinnanvaiheenTilanMukaan(
        laskettuTila,
        hakemusHyvaksyttavissaEdellisenValinnanvaiheenMukaan(
            hakemusOid, edellinenValinnanvaihe, valintaperusteetDTO, vanhatOsallistumiset));
  }

  public TilaJaSelite tilaEdellisenValinnanvaiheenMukaan(
      String hakemusOid, Tila laskettuTila, Valinnanvaihe edellinenValinnanvaihe) {
    return tilaEdellisenValinnanvaiheenTilanMukaan(
        laskettuTila,
        hakemusHyvaksyttavissaEdellisenValinnanvaiheenMukaan(hakemusOid, edellinenValinnanvaihe));
  }

  public boolean koeOsallistuminenToisessaKohteessa(
      String hakukohdeOid, ValintakoeOsallistuminen hakijanOsallistumiset) {
    List<String> kohteenValintakokeet =
        hakijanOsallistumiset.getHakutoiveet().stream()
            .filter(h -> h.getHakukohdeOid().equals(hakukohdeOid))
            .flatMap(h -> h.getValintakoeValinnanvaiheet().stream())
            .flatMap(v -> v.getValintakokeet().stream())
            .map(Valintakoe::getValintakoeTunniste)
            .collect(Collectors.toList());

    return hakijanOsallistumiset.getHakutoiveet().stream()
        .filter(h -> !h.getHakukohdeOid().equals(hakukohdeOid))
        .flatMap(h -> h.getValintakoeValinnanvaiheet().stream())
        .flatMap(v -> v.getValintakokeet().stream())
        .anyMatch(
            k ->
                kohteenValintakokeet.contains(k.getValintakoeTunniste())
                    && k.getOsallistuminenTulos().getOsallistuminen().equals(OSALLISTUU));
  }

  private Optional<TilaJaSelite> kutsuttavaKohdekohtaiseenKokeeseenVaikkaHylattyValisijoittelussa(
      String hakemusOid,
      Valinnanvaihe edellinenValinnanvaihe,
      Optional<ValintaperusteetDTO> valintaperusteetDTO,
      Optional<ValintakoeOsallistuminen> vanhatOsallistumiset) {
    if (edellinenValinnanvaihe.hylattyValisijoittelussa(hakemusOid)
        && valintaperusteetDTO.isPresent()
        && vanhatOsallistumiset.isPresent()
        && valintaperusteetDTO.get().getValinnanVaihe() != null
        && koeOsallistuminenToisessaKohteessa(
            valintaperusteetDTO.get().getHakukohdeOid(), vanhatOsallistumiset.get())
        && VALINTAKOE.equals(
            valintaperusteetDTO.get().getValinnanVaihe().getValinnanVaiheTyyppi())) {
      Sets.SetView<String> talleKohteelleSpesifienKokeidenTunnisteet =
          paatteleKoetunnisteetJotkaOnVainTallaHakukohteella(
              valintaperusteetDTO.get(), vanhatOsallistumiset.get());
      if (LOG.isDebugEnabled()) {
        LOG.debug(
            "talleKohteelleSpesifienKokeidenTunnisteet == "
                + talleKohteelleSpesifienKokeidenTunnisteet);
      }
      if (!talleKohteelleSpesifienKokeidenTunnisteet.isEmpty()) {
        return Optional.of(
            new TilaJaSelite(
                HYVAKSYTTAVISSA,
                suomenkielinenMap(
                    "Hakemuksella on kohteeseen seuraavat kokeet, joihin ei osallistuta muissa kohteissa: "
                        + talleKohteelleSpesifienKokeidenTunnisteet)));
      }
    }
    return Optional.empty();
  }
}
