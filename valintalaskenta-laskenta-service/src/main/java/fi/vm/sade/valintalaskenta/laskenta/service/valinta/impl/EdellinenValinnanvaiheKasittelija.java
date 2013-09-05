package fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl;

import com.google.common.base.Predicate;
import com.google.common.collect.Collections2;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Hylattytila;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Tila;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Virhetila;
import fi.vm.sade.valintalaskenta.domain.valinta.*;
import org.springframework.stereotype.Component;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * User: wuoti
 * Date: 4.9.2013
 * Time: 14.40
 */
@Component
public class EdellinenValinnanvaiheKasittelija {

    /**
     * Määrittää, onko hakemus hyväksyttävissä edellisen valinnan vaiheen mukaan.
     * Hakemus on hyväksyttävissä, jos se on ollut hyväksyttävissä ainakin yhdessä edellisen valinnan vaiheen
     * valintatapajonossa.
     */
    public TilaJaSelite hakemusHyvaksyttavissaEdellisenValinnanvaiheenMukaan(final String hakemusOid,
                                                                             Valinnanvaihe edellinenValinnanvaihe) {
        TilaJaSelite palautettavaTila = null;
        List<TilaJaSelite> tilat = new ArrayList<TilaJaSelite>();
        if (edellinenValinnanvaihe == null) {
            palautettavaTila = new TilaJaSelite(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, null);
        } else if (edellinenValinnanvaihe.getValintatapajonot().isEmpty()) {
            // Jos edellisessä valinnan vaiheessa ei ole yhtään valintatapajonoa, voidaan olettaa, että hakemus
            // on hyväksyttävissä
            palautettavaTila = new TilaJaSelite(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, null);
        } else {
            for (final Valintatapajono jono : edellinenValinnanvaihe.getValintatapajonot()) {
                List<Jonosija> jonosijat = jono.getJonosijat();

                Collection<Jonosija> filtteroidutJonosijat = Collections2.filter(jonosijat, new Predicate<Jonosija>() {
                    @Override
                    public boolean apply(@Nullable Jonosija jonosija) {
                        return jonosija.getHakemusOid().equals(hakemusOid);
                    }
                });

                TilaJaSelite tilaJonossa = null;
                if (filtteroidutJonosijat.isEmpty()) {
                    // Jos hakemus ei ole ollut mukana edellisessä valinnan vaiheessa, hakemus ei voi tulla
                    // hyväksyttäväksi tässä valinnan vaiheessa. Breikataan pois.
                    palautettavaTila = new TilaJaSelite(
                            JarjestyskriteerituloksenTila.VIRHE,
                            "Hakemus ei ole ollut mukana laskennassa edellisessä valinnan vaiheessa");
                    break;
                } else if (jonosijat.get(0).getJarjestyskriteeritulokset().isEmpty()) {
                    // Mitä tehdään, jos hakemukselle ei ole laskentatulosta? Kai se on hyväksyttävissä
                    tilaJonossa = new TilaJaSelite(
                            JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA,
                            "Hakemukselle ei ole laskentatulosta jonossa");
                } else {
                    Jarjestyskriteeritulos tulos = jonosijat.get(0).getJarjestyskriteeritulokset().get(0);
                    tilaJonossa = new TilaJaSelite(tulos.getTila(), tulos.getKuvaus());
                }

                tilat.add(tilaJonossa);
            }

            // Tarkistetaan, että palautettavaa tilaa ei ole vielä asetettu
            if (palautettavaTila == null) {

                // Filtteroidaan kaikki HYVAKSYTTAVISSA-tilat. Jos yksikin tällainen löytyy, hakemus on
                // hyväksyttävissä myös tässä valinnan vaiheessa.
                Collection<TilaJaSelite> filtteroidutTilat = Collections2.filter(tilat, new Predicate<TilaJaSelite>() {
                    @Override
                    public boolean apply(@Nullable TilaJaSelite edellinenValinnanvaiheTila) {
                        return JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA.equals(edellinenValinnanvaiheTila.getTila());
                    }
                });

                if (filtteroidutTilat.isEmpty()) {
                    palautettavaTila = new TilaJaSelite(JarjestyskriteerituloksenTila.HYLATTY,
                            "Hakemus ei ole hyväksyttävissä yhdessäkään edellisen valinnan vaiheen valintatapajonossa");
                } else {
                    palautettavaTila = new TilaJaSelite(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, "");
                }
            }
        }

        return palautettavaTila;
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
                palautettavaTila = new TilaJaSelite(tila, ((Hylattytila) laskettuTila).getKuvaus());
            }
        } else if (Tila.Tilatyyppi.VIRHE.equals(laskettuTila.getTilatyyppi())) {
            JarjestyskriteerituloksenTila tila = JarjestyskriteerituloksenTila.VIRHE;
            if (laskettuTila instanceof Virhetila) {
                palautettavaTila = new TilaJaSelite(tila, ((Virhetila) laskettuTila).getKuvaus());
            }
        } else if (Tila.Tilatyyppi.HYVAKSYTTAVISSA.equals(laskettuTila.getTilatyyppi())) {
            palautettavaTila = new TilaJaSelite(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, null);
        } else {

            // Jos jostakin syystä tilaa ei pystytä selvittämään, palautetaan määrittelemätön-tila
            palautettavaTila = new TilaJaSelite(JarjestyskriteerituloksenTila.MAARITTELEMATON, null);
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

}
