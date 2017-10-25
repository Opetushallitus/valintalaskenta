package fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl;

import com.google.common.collect.Collections2;

import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Hylattytila;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Tila;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Virhetila;
import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import fi.vm.sade.valintalaskenta.domain.valinta.MuokattuJonosija;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Valintakoe;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.tulos.dao.MuokattuJonosijaDAO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

@Component
public class EdellinenValinnanvaiheKasittelija {
    @Autowired
    MuokattuJonosijaDAO muokattuJonosijaDAO;

    /**
     * Määrittää, onko hakemus hyväksyttävissä edellisen valinnan vaiheen mukaan.
     * Hakemus on hyväksyttävissä, jos se on ollut hyväksyttävissä ainakin yhdessä edellisen valinnan vaiheen
     * valintatapajonossa.
     */
    public TilaJaSelite hakemusHyvaksyttavissaEdellisenValinnanvaiheenMukaan(final String hakemusOid,
                                                                             Valinnanvaihe edellinenValinnanvaihe) {
        // Jos edellisessä valinnan vaiheessa ei ole yhtään valintatapajonoa, voidaan olettaa, että hakemus
        // on hyväksyttävissä
        if (edellinenValinnanvaihe == null || edellinenValinnanvaihe.getValintatapajonot().isEmpty()) {
            return new TilaJaSelite(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, new HashMap<>());
        }

        List<TilaJaSelite> tilat = new ArrayList<>();
        for (final Valintatapajono jono : edellinenValinnanvaihe.getValintatapajonot()) {
            Jonosija jonosija = getJonosijaForHakemus(hakemusOid, jono.getJonosijat());

            TilaJaSelite tilaJonossa;
            if (jonosija == null) {
                // Jos hakemus ei ole ollut mukana edellisessä valinnan vaiheessa, hakemus ei voi tulla
                // hyväksyttäväksi tässä valinnan vaiheessa.
                return new TilaJaSelite(
                    JarjestyskriteerituloksenTila.VIRHE,
                    suomenkielinenMap("Hakemus ei ole ollut mukana laskennassa edellisessä valinnan vaiheessa"));
            } else if (jonosija.getJarjestyskriteeritulokset().isEmpty()) {
                // Mitä tehdään, jos hakemukselle ei ole laskentatulosta? Kai se on hyväksyttävissä
                tilaJonossa = new TilaJaSelite(
                    JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA,
                    suomenkielinenMap("Hakemukselle ei ole laskentatulosta jonossa"));
            } else {
                Jarjestyskriteeritulos tulos = jonosija.getJarjestyskriteeritulokset().get(0);

                Optional<MuokattuJonosija> muokattuJonosija = Optional.ofNullable(muokattuJonosijaDAO.readByValintatapajonoOid(jono.getValintatapajonoOid(), hakemusOid));
                muokattuJonosija.ifPresent(mj -> {
                    Optional<Jarjestyskriteeritulos> muokattuTulos = mj.getJarjestyskriteerit().stream().filter(j -> j.getPrioriteetti() == tulos.getPrioriteetti()).findFirst();
                    muokattuTulos.ifPresent(m -> {
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
        Collection<TilaJaSelite> filtteroidutTilat = Collections2.filter(tilat, edellinenValinnanvaiheTila ->
            JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA.equals(edellinenValinnanvaiheTila.getTila()));

        if (filtteroidutTilat.isEmpty()) {
            Map<String, String> hylkaysSelitteet = tilat.get(tilat.size() - 1).getSelite();
            String tekninenSelite = "Hakemus ei ole hyväksyttävissä yhdessäkään edellisen valinnan vaiheen valintatapajonossa";
            return new TilaJaSelite(JarjestyskriteerituloksenTila.HYLATTY, hylkaysSelitteet, tekninenSelite);
        } else {
            return new TilaJaSelite(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, new HashMap<>());
        }
    }

    private Jonosija getJonosijaForHakemus(final String hakemusOid, final List<Jonosija> jonosijat) {
        Collection<Jonosija> filtteroidutJonosijat = Collections2.filter(jonosijat, jonosija -> jonosija.getHakemusOid().equals(hakemusOid));
        return filtteroidutJonosijat.isEmpty() ? null : filtteroidutJonosijat.iterator().next();
    }

    private Map<String, String> suomenkielinenMap(String teksti) {
        Map<String, String> vastaus = new HashMap<String, String>();
        vastaus.put("FI", teksti);
        return vastaus;
    }

    /**
     * Selvittää laskennan tilan edellisen vaiheen tilan sekä varsinaisen lasketun tilan mukaan
     */
    public TilaJaSelite tilaEdellisenValinnanvaiheenTilanMukaan(Tila laskettuTila,
                                                                TilaJaSelite edellisenVaiheentila) {
        TilaJaSelite palautettavaTila = null;

        // Jos edellinen vaiheen mukaan hakija ei ole hyväksyttävissä, asetetaan tilaksi hylätty.
        // Jos taas edellisen vaiheen mukaan hakija on hyväksyttävissä, käytetään laskettua tilaa.
        if (!JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA.equals(edellisenVaiheentila.getTila())) {
            palautettavaTila = new TilaJaSelite(edellisenVaiheentila.getTila(), edellisenVaiheentila.getSelite());
        } else if (Tila.Tilatyyppi.HYLATTY.equals(laskettuTila.getTilatyyppi())) {
            JarjestyskriteerituloksenTila tila = JarjestyskriteerituloksenTila.HYLATTY;
            if (laskettuTila instanceof Hylattytila) {
                palautettavaTila = new TilaJaSelite(tila, ((Hylattytila) laskettuTila).getKuvaus(), ((Hylattytila) laskettuTila).getTekninenKuvaus());
            }
        } else if (Tila.Tilatyyppi.VIRHE.equals(laskettuTila.getTilatyyppi())) {
            JarjestyskriteerituloksenTila tila = JarjestyskriteerituloksenTila.VIRHE;
            if (laskettuTila instanceof Virhetila) {
                palautettavaTila = new TilaJaSelite(tila, ((Virhetila) laskettuTila).getKuvaus());
            }
        } else if (Tila.Tilatyyppi.HYVAKSYTTAVISSA.equals(laskettuTila.getTilatyyppi())) {
            palautettavaTila = new TilaJaSelite(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, new HashMap<>());
        } else {

            // Jos jostakin syystä tilaa ei pystytä selvittämään, palautetaan määrittelemätön-tila
            palautettavaTila = new TilaJaSelite(JarjestyskriteerituloksenTila.MAARITTELEMATON, new HashMap<>());
        }

        return palautettavaTila;
    }

    /**
     * Laskee järjestyskriteerin tilan varsinaisen lasketun tilan sekä sen mukaan, onko hakemus hyväksyttävissä
     * edellisen valinnan vaiheen tilan mukaan
     *
     * @param hakemusOid             hakemus OID
     * @param laskettuTila           järjestyskriteerille laskennasta saatu tila
     * @param edellinenValinnanvaihe edellinen valinnan vaihe
     * @return järjestyskriteerin tila
     */
    public TilaJaSelite tilaEdellisenValinnanvaiheenMukaan(String hakemusOid, Tila laskettuTila, Valinnanvaihe edellinenValinnanvaihe) {
        return tilaEdellisenValinnanvaiheenTilanMukaan(laskettuTila,
            hakemusHyvaksyttavissaEdellisenValinnanvaiheenMukaan(hakemusOid, edellinenValinnanvaihe));
    }

    public boolean koeOsallistuminenToisessaKohteessa(String hakukohdeOid, ValintakoeOsallistuminen hakijanOsallistumiset) {
        List<String> kohteenValintakokeet = hakijanOsallistumiset.getHakutoiveet()
            .stream()
            .filter(h -> h.getHakukohdeOid().equals(hakukohdeOid))
            .flatMap(h -> h.getValinnanVaiheet().stream())
            .flatMap(v -> v.getValintakokeet().stream())
            .map(Valintakoe::getValintakoeTunniste)
            .collect(Collectors.toList());

        return hakijanOsallistumiset.getHakutoiveet()
            .stream()
            .filter(h -> !h.getHakukohdeOid().equals(hakukohdeOid))
            .flatMap(h -> h.getValinnanVaiheet().stream())
            .flatMap(v -> v.getValintakokeet().stream())
            .anyMatch(k -> kohteenValintakokeet.contains(k.getValintakoeTunniste())
                && k.getOsallistuminenTulos().getOsallistuminen().equals(Osallistuminen.OSALLISTUU));
    }
}
