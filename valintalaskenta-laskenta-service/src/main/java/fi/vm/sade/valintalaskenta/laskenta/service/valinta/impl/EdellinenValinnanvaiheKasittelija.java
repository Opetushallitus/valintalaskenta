package fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl;

import com.google.common.base.Predicate;
import com.google.common.collect.Collections2;
import fi.vm.sade.valintalaskenta.domain.valinta.*;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * User: wuoti
 * Date: 4.9.2013
 * Time: 14.40
 */
public abstract class EdellinenValinnanvaiheKasittelija {
    public static EdellinenValinnanvaiheTila hakemusHyvaksyttavissa(final String hakemusOid, Valinnanvaihe edellinenValinnanvaihe) {
        EdellinenValinnanvaiheTila palautettavaTila = null;
        List<EdellinenValinnanvaiheTila> tilat = new ArrayList<EdellinenValinnanvaiheTila>();
        if (edellinenValinnanvaihe == null) {
            palautettavaTila = new EdellinenValinnanvaiheTila(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, null);
        } else if (edellinenValinnanvaihe.getValintatapajonot().isEmpty()) {
            // Jos edellisessä valinnan vaiheessa ei ole yhtään valintatapajonoa, voidaan olettaa, että hakemus
            // on hyväksyttävissä
            palautettavaTila = new EdellinenValinnanvaiheTila(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, null);
        } else {
            for (final Valintatapajono jono : edellinenValinnanvaihe.getValintatapajonot()) {
                List<Jonosija> jonosijat = jono.getJonosijat();

                Collection<Jonosija> filtteroidutJonosijat = Collections2.filter(jonosijat, new Predicate<Jonosija>() {
                    @Override
                    public boolean apply(@Nullable Jonosija jonosija) {
                        return jonosija.getHakemusOid().equals(hakemusOid);
                    }
                });

                EdellinenValinnanvaiheTila tilaJonossa = null;
                if (filtteroidutJonosijat.isEmpty()) {
                    System.out.println(2);
                    // Jos hakemus ei ole ollut mukana edellisessä valinnan vaiheessa, hakemus ei voi tulla
                    // hyväksyttäväksi tässä valinnan vaiheessa. Breikataan pois.
                    palautettavaTila = new EdellinenValinnanvaiheTila(
                            JarjestyskriteerituloksenTila.VIRHE,
                            "Hakemus ei ole ollut mukana laskennassa edellisessä valinnan vaiheessa");
                    break;
                } else if (jonosijat.get(0).getJarjestyskriteeritulokset().isEmpty()) {
                    // Mitä tehdään, jos hakemukselle ei ole laskentatulosta? Kai se on hyväksyttävissä
                    tilaJonossa = new EdellinenValinnanvaiheTila(
                            JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA,
                            "Hakemukselle ei ole laskentatulosta jonossa");
                } else {
                    Jarjestyskriteeritulos tulos = jonosijat.get(0).getJarjestyskriteeritulokset().get(0);
                    tilaJonossa = new EdellinenValinnanvaiheTila(tulos.getTila(), tulos.getKuvaus());
                }

                tilat.add(tilaJonossa);
            }

            // Tarkistetaan, että palautettavaa tilaa ei ole vielä asetettu
            if (palautettavaTila == null) {

                // Filtteroidaan kaikki HYVAKSYTTAVISSA-tilat. Jos yksikin tällainen löytyy, hakemus on
                // hyväksyttävissä myös tässä valinnan vaiheessa.
                Collection<EdellinenValinnanvaiheTila> filtteroidutTilat = Collections2.filter(tilat, new Predicate<EdellinenValinnanvaiheTila>() {
                    @Override
                    public boolean apply(@Nullable EdellinenValinnanvaiheTila edellinenValinnanvaiheTila) {
                        return JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA.equals(edellinenValinnanvaiheTila.getTila());
                    }
                });

                if (filtteroidutTilat.isEmpty()) {
                    palautettavaTila = new EdellinenValinnanvaiheTila(JarjestyskriteerituloksenTila.HYLATTY,
                            "Hakemus ei ole hyväksyttävissä yhdessäkään edellisen valinnan vaiheen valintatapajonossa");
                } else {
                    palautettavaTila = new EdellinenValinnanvaiheTila(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, "");
                }
            }
        }
        return palautettavaTila;
    }
}
