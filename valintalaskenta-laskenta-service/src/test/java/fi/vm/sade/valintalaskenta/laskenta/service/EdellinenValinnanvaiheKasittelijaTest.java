package fi.vm.sade.valintalaskenta.laskenta.service;

import fi.vm.sade.valintalaskenta.domain.valinta.*;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.EdellinenValinnanvaiheKasittelija;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.EdellinenValinnanvaiheTila;
import org.junit.Test;
import scala.actors.threadpool.Arrays;

import static org.junit.Assert.assertEquals;

/**
 * User: wuoti
 * Date: 4.9.2013
 * Time: 14.44
 */
public class EdellinenValinnanvaiheKasittelijaTest {

    @Test
    public void testEiValintatapajonoja() {

        Valinnanvaihe vaihe = new Valinnanvaihe();

        EdellinenValinnanvaiheTila tila = EdellinenValinnanvaiheKasittelija.hakemusHyvaksyttavissa("hakesmusOid", vaihe);
        assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, tila.getTila());
    }

    @Test
    public void testHakemusEiMukanaValinnanvaiheessa() {
        Valintatapajono jono = new Valintatapajono();

        for (int i = 2; i <= 10; ++i) {
            Jonosija jonosija = new Jonosija();
            jonosija.setHakemusOid("hakemusOid" + i);
            jono.getJonosijat().add(jonosija);
        }

        Valinnanvaihe vaihe = new Valinnanvaihe();
        vaihe.getValintatapajonot().add(jono);

        final String hakemusOid = "hakemusOid1";
        EdellinenValinnanvaiheTila tila = EdellinenValinnanvaiheKasittelija.hakemusHyvaksyttavissa(hakemusOid, vaihe);
        assertEquals(JarjestyskriteerituloksenTila.VIRHE, tila.getTila());
    }

    private static Jonosija luoJonosija(String hakemusOid, JarjestyskriteerituloksenTila tila) {
        Jonosija jonosija = new Jonosija();
        jonosija.setHakemusOid(hakemusOid);
        Jarjestyskriteeritulos tulos = new Jarjestyskriteeritulos();
        tulos.setTila(tila);
        jonosija.getJarjestyskriteeritulokset().add(tulos);
        return jonosija;
    }

    @Test
    public void testHakemusHyvaksyttavissaAinakinYhdessaJonossa() {
        final String hakemusOid = "hakemusOid1";

        Valintatapajono jono1 = new Valintatapajono();
        jono1.getJonosijat().add(luoJonosija(hakemusOid, JarjestyskriteerituloksenTila.HYLATTY));

        Valintatapajono jono2 = new Valintatapajono();
        jono2.getJonosijat().add(luoJonosija(hakemusOid, JarjestyskriteerituloksenTila.VIRHE));

        Valintatapajono jono3 = new Valintatapajono();
        jono3.getJonosijat().add(luoJonosija(hakemusOid, JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA));

        Valinnanvaihe vaihe = new Valinnanvaihe();
        vaihe.getValintatapajonot().addAll(Arrays.asList(new Valintatapajono[]{jono1, jono2, jono3}));

        EdellinenValinnanvaiheTila tila = EdellinenValinnanvaiheKasittelija.hakemusHyvaksyttavissa(hakemusOid, vaihe);
        assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, tila.getTila());
    }

    @Test
    public void testHakemusEiHyvaksyttavissaYhdessakaanJonossa() {
        final String hakemusOid = "hakemusOid1";

        Valintatapajono jono1 = new Valintatapajono();
        jono1.getJonosijat().add(luoJonosija(hakemusOid, JarjestyskriteerituloksenTila.HYLATTY));

        Valintatapajono jono2 = new Valintatapajono();
        jono2.getJonosijat().add(luoJonosija(hakemusOid, JarjestyskriteerituloksenTila.VIRHE));

        Valintatapajono jono3 = new Valintatapajono();
        jono3.getJonosijat().add(luoJonosija(hakemusOid, JarjestyskriteerituloksenTila.MAARITTELEMATON));

        Valinnanvaihe vaihe = new Valinnanvaihe();
        vaihe.getValintatapajonot().addAll(Arrays.asList(new Valintatapajono[]{jono1, jono2, jono3}));

        EdellinenValinnanvaiheTila tila = EdellinenValinnanvaiheKasittelija.hakemusHyvaksyttavissa(hakemusOid, vaihe);
        assertEquals(JarjestyskriteerituloksenTila.HYLATTY, tila.getTila());
    }

    @Test
    public void testEdellinenVaiheNull() {
        EdellinenValinnanvaiheTila tila = EdellinenValinnanvaiheKasittelija.hakemusHyvaksyttavissa("hakemusOid", null);
        assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, tila.getTila());
    }
}
