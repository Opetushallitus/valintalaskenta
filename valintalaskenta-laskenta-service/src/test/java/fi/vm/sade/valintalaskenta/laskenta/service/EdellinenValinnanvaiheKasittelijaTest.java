package fi.vm.sade.valintalaskenta.laskenta.service;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;

import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.ArvokonvertointiVirhe;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Arvokonvertterihylkays;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Hylattytila;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Hyvaksyttavissatila;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Tila;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Virhetila;
import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.EdellinenValinnanvaiheKasittelija;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.TilaJaSelite;
import fi.vm.sade.valintalaskenta.tulos.dao.MuokattuJonosijaDAO;
import org.junit.Test;

import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

public class EdellinenValinnanvaiheKasittelijaTest {
    private final EdellinenValinnanvaiheKasittelija edellinenValinnanvaiheKasittelija = new EdellinenValinnanvaiheKasittelija(mock(MuokattuJonosijaDAO.class));

    private Map<String, String> suomenkielinenMap(String teksti) {
        Map<String, String> vastaus = new HashMap<>();
        vastaus.put("FI", teksti);
        return vastaus;
    }

    @Test
    public void testEiValintatapajonoja() {
        Valinnanvaihe vaihe = new Valinnanvaihe();

        TilaJaSelite tila = edellinenValinnanvaiheKasittelija.hakemusHyvaksyttavissaEdellisenValinnanvaiheenMukaan("hakesmusOid", vaihe, null, new ValintakoeOsallistuminen());
        assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, tila.getTila());
    }

    @Test
    public void testHakemusEiMukanaValinnanvaiheessa() {
        Valintatapajono jono = new Valintatapajono();

        List<Jonosija> jonosijat = new LinkedList<>();
        for (int i = 2; i <= 10; ++i) {
            Jonosija jonosija = new Jonosija();
            jonosija.setHakemusOid("hakemusOid" + i);
            jonosijat.add(jonosija);
        }
        jono.setJonosijat(jonosijat);

        Valinnanvaihe vaihe = new Valinnanvaihe();
        vaihe.getValintatapajonot().add(jono);

        final String hakemusOid = "hakemusOid1";
        TilaJaSelite tila = edellinenValinnanvaiheKasittelija.hakemusHyvaksyttavissaEdellisenValinnanvaiheenMukaan(hakemusOid, vaihe, null, new ValintakoeOsallistuminen());
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
        jono1.setJonosijat(new LinkedList<>());
        jono1.getJonosijat().add(luoJonosija(hakemusOid, JarjestyskriteerituloksenTila.HYLATTY));

        Valintatapajono jono2 = new Valintatapajono();
        jono2.setJonosijat(new LinkedList<>());
        jono2.getJonosijat().add(luoJonosija(hakemusOid, JarjestyskriteerituloksenTila.VIRHE));

        Valintatapajono jono3 = new Valintatapajono();
        jono3.setJonosijat(new LinkedList<>());
        jono3.getJonosijat().add(luoJonosija(hakemusOid, JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA));

        Valinnanvaihe vaihe = new Valinnanvaihe();
        vaihe.getValintatapajonot().addAll(Arrays.asList(jono1, jono2, jono3));

        TilaJaSelite tila = edellinenValinnanvaiheKasittelija.hakemusHyvaksyttavissaEdellisenValinnanvaiheenMukaan(hakemusOid, vaihe, null, new ValintakoeOsallistuminen());
        assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, tila.getTila());
    }

    @Test
    public void testHakemusEiHyvaksyttavissaYhdessakaanJonossa() {
        final String hakemusOid = "hakemusOid1";

        Valintatapajono jono1 = new Valintatapajono();
        jono1.setJonosijat(new LinkedList<>());
        jono1.getJonosijat().add(luoJonosija(hakemusOid, JarjestyskriteerituloksenTila.HYLATTY));

        Valintatapajono jono2 = new Valintatapajono();
        jono2.setJonosijat(new LinkedList<>());
        jono2.getJonosijat().add(luoJonosija(hakemusOid, JarjestyskriteerituloksenTila.VIRHE));

        Valintatapajono jono3 = new Valintatapajono();
        jono3.setJonosijat(new LinkedList<>());
        jono3.getJonosijat().add(luoJonosija(hakemusOid, JarjestyskriteerituloksenTila.MAARITTELEMATON));

        Valinnanvaihe vaihe = new Valinnanvaihe();
        vaihe.getValintatapajonot().addAll(Arrays.asList(jono1, jono2, jono3));

        TilaJaSelite tila = edellinenValinnanvaiheKasittelija.hakemusHyvaksyttavissaEdellisenValinnanvaiheenMukaan(hakemusOid, vaihe, null, new ValintakoeOsallistuminen());
        assertEquals(JarjestyskriteerituloksenTila.HYLATTY, tila.getTila());
    }

    @Test
    public void testEdellinenVaiheNull() {
        TilaJaSelite tila = edellinenValinnanvaiheKasittelija.hakemusHyvaksyttavissaEdellisenValinnanvaiheenMukaan("hakemusOid", null, null, new ValintakoeOsallistuminen());
        assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, tila.getTila());
    }

    @Test
    public void testHylattyEdellisenVaiheenMukaan() {
        final String selite = "selite";
        final JarjestyskriteerituloksenTila tila = JarjestyskriteerituloksenTila.HYLATTY;

        TilaJaSelite edellisenVaiheenTila = new TilaJaSelite(tila, suomenkielinenMap(selite));
        Tila laskettuTila = new Hyvaksyttavissatila();

        TilaJaSelite tilaJaSelite =
                edellinenValinnanvaiheKasittelija.tilaEdellisenValinnanvaiheenTilanMukaan(laskettuTila, edellisenVaiheenTila);
        assertEquals(tila, tilaJaSelite.getTila());
        assertEquals(selite, tilaJaSelite.getSelite().get("FI"));
    }

    @Test
    public void testVirheEdellisenVaiheenMukaan() {
        final String selite = "selite";
        final JarjestyskriteerituloksenTila tila = JarjestyskriteerituloksenTila.VIRHE;

        TilaJaSelite edellisenVaiheenTila = new TilaJaSelite(tila, suomenkielinenMap(selite));
        Tila laskettuTila = new Hyvaksyttavissatila();

        TilaJaSelite tilaJaSelite =
                edellinenValinnanvaiheKasittelija.tilaEdellisenValinnanvaiheenTilanMukaan(laskettuTila, edellisenVaiheenTila);
        assertEquals(tila, tilaJaSelite.getTila());
        assertEquals(selite, tilaJaSelite.getSelite().get("FI"));
    }

    @Test
    public void testHyvaksyttavissaEdellisenVaiheenMukaanLaskettuTilaHyvaksyttavissa() {
        final String selite = "selite";
        final JarjestyskriteerituloksenTila tila = JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA;

        TilaJaSelite edellisenVaiheenTila = new TilaJaSelite(tila, suomenkielinenMap(selite));
        Tila laskettuTila = new Hyvaksyttavissatila();

        TilaJaSelite tilaJaSelite =
                edellinenValinnanvaiheKasittelija.tilaEdellisenValinnanvaiheenTilanMukaan(laskettuTila, edellisenVaiheenTila);
        assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, tilaJaSelite.getTila());
        assertTrue(tilaJaSelite.getSelite().isEmpty());
    }

    @Test
    public void testHyvaksyttavissaEdellisenVaiheenMukaanLaskettuTilaHylatty() {
        final String selite = "selite";
        final JarjestyskriteerituloksenTila tila = JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA;

        TilaJaSelite edellisenVaiheenTila = new TilaJaSelite(tila, suomenkielinenMap(selite));
        final String lasketuntilanSelite = "toinenSelite";
        Tila laskettuTila = new Hylattytila(suomenkielinenMap(lasketuntilanSelite), new Arvokonvertterihylkays(""));

        TilaJaSelite tilaJaSelite =
                edellinenValinnanvaiheKasittelija.tilaEdellisenValinnanvaiheenTilanMukaan(laskettuTila, edellisenVaiheenTila);
        assertEquals(JarjestyskriteerituloksenTila.HYLATTY, tilaJaSelite.getTila());
        assertEquals(lasketuntilanSelite, tilaJaSelite.getSelite().get("FI"));
    }

    @Test
    public void testHyvaksyttavissaEdellisenVaiheenMukaanLaskettuTilaVirhe() {
        final String selite = "selite";
        final JarjestyskriteerituloksenTila tila = JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA;

        TilaJaSelite edellisenVaiheenTila = new TilaJaSelite(tila, suomenkielinenMap(selite));
        final String lasketuntilanSelite = "toinenSelite";
        Tila laskettuTila = new Virhetila(suomenkielinenMap(lasketuntilanSelite), new ArvokonvertointiVirhe(""));

        TilaJaSelite tilaJaSelite =
                edellinenValinnanvaiheKasittelija.tilaEdellisenValinnanvaiheenTilanMukaan(laskettuTila, edellisenVaiheenTila);
        assertEquals(JarjestyskriteerituloksenTila.VIRHE, tilaJaSelite.getTila());
        assertEquals(lasketuntilanSelite, tilaJaSelite.getSelite().get("FI"));
    }

    @Test
    public void test() {
        Valinnanvaihe edellinenVaihe = new Valinnanvaihe();
        final String hakemusOid = "hakemusOid1";
        TilaJaSelite tilaJaSelite = edellinenValinnanvaiheKasittelija.tilaEdellisenValinnanvaiheenMukaan(hakemusOid, new Hyvaksyttavissatila(), edellinenVaihe, Optional.empty(), new ValintakoeOsallistuminen());
        assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, tilaJaSelite.getTila());
        assertTrue(tilaJaSelite.getSelite().isEmpty());
    }
}
