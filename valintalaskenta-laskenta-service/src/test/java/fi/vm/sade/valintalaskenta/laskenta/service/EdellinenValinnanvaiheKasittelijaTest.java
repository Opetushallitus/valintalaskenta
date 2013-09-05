package fi.vm.sade.valintalaskenta.laskenta.service;

import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.*;
import fi.vm.sade.valintalaskenta.domain.valinta.*;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.EdellinenValinnanvaiheKasittelija;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.TilaJaSelite;
import org.junit.Before;
import org.junit.Test;
import scala.actors.threadpool.Arrays;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

/**
 * User: wuoti
 * Date: 4.9.2013
 * Time: 14.44
 */
public class EdellinenValinnanvaiheKasittelijaTest {

    private EdellinenValinnanvaiheKasittelija edellinenValinnanvaiheKasittelija;

    @Before
    public void setUp() {
        edellinenValinnanvaiheKasittelija = new EdellinenValinnanvaiheKasittelija();
    }

    @Test
    public void testEiValintatapajonoja() {

        Valinnanvaihe vaihe = new Valinnanvaihe();

        TilaJaSelite tila = edellinenValinnanvaiheKasittelija.hakemusHyvaksyttavissaEdellisenValinnanvaiheenMukaan("hakesmusOid", vaihe);
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
        TilaJaSelite tila = edellinenValinnanvaiheKasittelija.hakemusHyvaksyttavissaEdellisenValinnanvaiheenMukaan(hakemusOid, vaihe);
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

        TilaJaSelite tila = edellinenValinnanvaiheKasittelija.hakemusHyvaksyttavissaEdellisenValinnanvaiheenMukaan(hakemusOid, vaihe);
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

        TilaJaSelite tila = edellinenValinnanvaiheKasittelija.hakemusHyvaksyttavissaEdellisenValinnanvaiheenMukaan(hakemusOid, vaihe);
        assertEquals(JarjestyskriteerituloksenTila.HYLATTY, tila.getTila());
    }

    @Test
    public void testEdellinenVaiheNull() {
        TilaJaSelite tila = edellinenValinnanvaiheKasittelija.hakemusHyvaksyttavissaEdellisenValinnanvaiheenMukaan("hakemusOid", null);
        assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, tila.getTila());
    }

    @Test
    public void testHylattyEdellisenVaiheenMukaan() {
        final String selite = "selite";
        final JarjestyskriteerituloksenTila tila = JarjestyskriteerituloksenTila.HYLATTY;

        TilaJaSelite edellisenVaiheenTila = new TilaJaSelite(tila, selite);
        Tila laskettuTila = new Hyvaksyttavissatila();

        TilaJaSelite tilaJaSelite =
                edellinenValinnanvaiheKasittelija.tilaEdellisenValinnanvaiheenTilanMukaan(laskettuTila, edellisenVaiheenTila);
        assertEquals(tila, tilaJaSelite.getTila());
        assertEquals(selite, tilaJaSelite.getSelite());
    }

    @Test
    public void testVirheEdellisenVaiheenMukaan() {
        final String selite = "selite";
        final JarjestyskriteerituloksenTila tila = JarjestyskriteerituloksenTila.VIRHE;

        TilaJaSelite edellisenVaiheenTila = new TilaJaSelite(tila, selite);
        Tila laskettuTila = new Hyvaksyttavissatila();

        TilaJaSelite tilaJaSelite =
                edellinenValinnanvaiheKasittelija.tilaEdellisenValinnanvaiheenTilanMukaan(laskettuTila, edellisenVaiheenTila);
        assertEquals(tila, tilaJaSelite.getTila());
        assertEquals(selite, tilaJaSelite.getSelite());
    }

    @Test
    public void testHyvaksyttavissaEdellisenVaiheenMukaanLaskettuTilaHyvaksyttavissa() {
        final String selite = "selite";
        final JarjestyskriteerituloksenTila tila = JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA;

        TilaJaSelite edellisenVaiheenTila = new TilaJaSelite(tila, selite);
        Tila laskettuTila = new Hyvaksyttavissatila();

        TilaJaSelite tilaJaSelite =
                edellinenValinnanvaiheKasittelija.tilaEdellisenValinnanvaiheenTilanMukaan(laskettuTila, edellisenVaiheenTila);
        assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, tilaJaSelite.getTila());
        assertNull(tilaJaSelite.getSelite());
    }

    @Test
    public void testHyvaksyttavissaEdellisenVaiheenMukaanLaskettuTilaHylatty() {
        final String selite = "selite";
        final JarjestyskriteerituloksenTila tila = JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA;

        TilaJaSelite edellisenVaiheenTila = new TilaJaSelite(tila, selite);
        final String lasketuntilanSelite = "toinenSelite";
        Tila laskettuTila = new Hylattytila(lasketuntilanSelite, new Arvokonvertterihylkays(""));

        TilaJaSelite tilaJaSelite =
                edellinenValinnanvaiheKasittelija.tilaEdellisenValinnanvaiheenTilanMukaan(laskettuTila, edellisenVaiheenTila);
        assertEquals(JarjestyskriteerituloksenTila.HYLATTY, tilaJaSelite.getTila());
        assertEquals(lasketuntilanSelite, tilaJaSelite.getSelite());
    }

    @Test
    public void testHyvaksyttavissaEdellisenVaiheenMukaanLaskettuTilaVirhe() {
        final String selite = "selite";
        final JarjestyskriteerituloksenTila tila = JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA;

        TilaJaSelite edellisenVaiheenTila = new TilaJaSelite(tila, selite);
        final String lasketuntilanSelite = "toinenSelite";
        Tila laskettuTila = new Virhetila(lasketuntilanSelite, new ArvokonvertointiVirhe(""));

        TilaJaSelite tilaJaSelite =
                edellinenValinnanvaiheKasittelija.tilaEdellisenValinnanvaiheenTilanMukaan(laskettuTila, edellisenVaiheenTila);
        assertEquals(JarjestyskriteerituloksenTila.VIRHE, tilaJaSelite.getTila());
        assertEquals(lasketuntilanSelite, tilaJaSelite.getSelite());
    }

    @Test
    public void test() {
        Valinnanvaihe edellinenVaihe = new Valinnanvaihe();
        final String hakemusOid = "hakemusOid1";
        TilaJaSelite tilaJaSelite = edellinenValinnanvaiheKasittelija.tilaEdellisenValinnanvaiheenMukaan(hakemusOid, new Hyvaksyttavissatila(), edellinenVaihe);
        assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, tilaJaSelite.getTila());
        assertNull(tilaJaSelite.getSelite());
    }
}
