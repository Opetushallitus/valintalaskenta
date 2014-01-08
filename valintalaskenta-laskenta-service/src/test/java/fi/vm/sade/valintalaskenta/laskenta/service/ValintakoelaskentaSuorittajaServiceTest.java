package fi.vm.sade.valintalaskenta.laskenta.service;

import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.hakemus.schema.HakukohdeTyyppi;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakukohde;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Tila;
import fi.vm.sade.service.valintaperusteet.schema.FunktiokutsuTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.HakukohteenValintaperusteTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteetTyyppi;
import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.valintakoe.*;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValinnanvaiheDAO;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValintakoeOsallistuminenDAO;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.EdellinenValinnanvaiheKasittelija;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.TilaJaSelite;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.ValintakoelaskentaSuorittajaService;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.Valintakoeosallistumislaskin;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.impl.ValintakoelaskentaSuorittajaServiceImpl;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.impl.util.HakukohdeValintakoeData;
import org.hamcrest.BaseMatcher;
import org.hamcrest.Description;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Matchers;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.*;

import static fi.vm.sade.valintalaskenta.laskenta.testdata.TestDataUtil.*;
import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.*;

/**
 * User: wuoti
 * Date: 3.5.2013
 * Time: 12.00
 */
public class ValintakoelaskentaSuorittajaServiceTest {

    private ValintakoelaskentaSuorittajaService valintakoelaskentaSuorittajaService;

    private ValintakoeOsallistuminenDAO valintakoeOsallistuminenDAOMock;
    private Valintakoeosallistumislaskin valintakoeosallistumislaskinMock;

    private ValinnanvaiheDAO valinnanvaiheDAOMock;
    private EdellinenValinnanvaiheKasittelija edellinenValinnanvaiheKasittelijaMock;

    @Before
    public void setUp() {
        valintakoelaskentaSuorittajaService = new ValintakoelaskentaSuorittajaServiceImpl();

        valintakoeOsallistuminenDAOMock = mock(ValintakoeOsallistuminenDAO.class);
        valintakoeosallistumislaskinMock = mock(Valintakoeosallistumislaskin.class);

        valinnanvaiheDAOMock = mock(ValinnanvaiheDAO.class);
        edellinenValinnanvaiheKasittelijaMock = mock(EdellinenValinnanvaiheKasittelija.class);

        ReflectionTestUtils.setField(valintakoelaskentaSuorittajaService, "valintakoeOsallistuminenDAO",
                valintakoeOsallistuminenDAOMock);
        ReflectionTestUtils.setField(valintakoelaskentaSuorittajaService, "valintakoeosallistumislaskin",
                valintakoeosallistumislaskinMock);

        ReflectionTestUtils.setField(valintakoelaskentaSuorittajaService, "valinnanvaiheDAO",
                valinnanvaiheDAOMock);
        ReflectionTestUtils.setField(valintakoelaskentaSuorittajaService, "edellinenValinnanvaiheKasittelija",
                edellinenValinnanvaiheKasittelijaMock);
    }

    @Test
    public void testMustache() {
        final String hakukohdeOid1 = "hakukohdeOid1";

        final String hakukohdeOid2 = "hakukohdeOid2";

        final HakemusTyyppi hakemus = luoHakemus("hakemusOid", "hakijaOid", hakukohdeOid1, hakukohdeOid2);

        final String hakuOid = "hakuOid";
        final String valintakoetunniste = "{{hakukohde.koetunniste}}";

        final String valinnanVaiheOid1 = "valinnanVaiheOid1";
        final int valinnanVaiheJarjestysluku1 = 0;

        ValintaperusteetTyyppi valintaperusteet1 = luoValintaperusteetJaValintakoeValinnanvaihe(hakuOid, hakukohdeOid1, valinnanVaiheOid1,
                valinnanVaiheJarjestysluku1, valintakoetunniste);
        HakukohteenValintaperusteTyyppi hkvp = new HakukohteenValintaperusteTyyppi();
        hkvp.setArvo("movember");
        hkvp.setTunniste("koetunniste");

        valintaperusteet1.getHakukohteenValintaperuste().add(hkvp);


        final OsallistuminenTulos osallistuminenTulos = new OsallistuminenTulos();
        osallistuminenTulos.setOsallistuminen(Osallistuminen.OSALLISTUU);
        when(valintakoeosallistumislaskinMock.laskeOsallistuminenYhdelleHakukohteelle(any(Hakukohde.class), any(HakemusTyyppi.class), any(FunktiokutsuTyyppi.class))).thenReturn(osallistuminenTulos);

        ArgumentCaptor<ValintakoeOsallistuminen> captor = ArgumentCaptor.forClass(ValintakoeOsallistuminen.class);
        valintakoelaskentaSuorittajaService.laske(hakemus, Arrays.asList(valintaperusteet1));
        verify(valintakoeOsallistuminenDAOMock, times(1)).createOrUpdate(captor.capture());

        ValintakoeOsallistuminen osallistuminen = captor.getValue();
        assertEquals("movember", osallistuminen.getHakutoiveet().get(0).getValinnanVaiheet().get(0).getValintakokeet().get(0).getValintakoeTunniste());


    }

    @Test
    public void testMustacheFail() {
        final String hakukohdeOid1 = "hakukohdeOid1";

        final String hakukohdeOid2 = "hakukohdeOid2";

        final HakemusTyyppi hakemus = luoHakemus("hakemusOid", "hakijaOid", hakukohdeOid1, hakukohdeOid2);

        final String hakuOid = "hakuOid";
        final String valintakoetunniste = "{{hakukohde.koetunnistea}}";

        final String valinnanVaiheOid1 = "valinnanVaiheOid1";
        final int valinnanVaiheJarjestysluku1 = 1;

        ValintaperusteetTyyppi valintaperusteet1 = luoValintaperusteetJaValintakoeValinnanvaihe(hakuOid, hakukohdeOid1, valinnanVaiheOid1,
                valinnanVaiheJarjestysluku1, valintakoetunniste);
        HakukohteenValintaperusteTyyppi hkvp = new HakukohteenValintaperusteTyyppi();
        hkvp.setArvo("movember");
        hkvp.setTunniste("koetunniste");

        valintaperusteet1.getHakukohteenValintaperuste().add(hkvp);


        final OsallistuminenTulos osallistuminenTulos = new OsallistuminenTulos();
        osallistuminenTulos.setOsallistuminen(Osallistuminen.OSALLISTUU);
        when(valintakoeosallistumislaskinMock.laskeOsallistuminenYhdelleHakukohteelle(any(Hakukohde.class), any(HakemusTyyppi.class), any(FunktiokutsuTyyppi.class))).thenReturn(osallistuminenTulos);

        valintakoelaskentaSuorittajaService.laske(hakemus, Arrays.asList(valintaperusteet1));
        verify(valintakoeOsallistuminenDAOMock, times(0)).createOrUpdate(any(ValintakoeOsallistuminen.class));

        verify(valintakoeosallistumislaskinMock, times(0)).laskeOsallistuminenYhdelleHakukohteelle(any(Hakukohde.class), any(HakemusTyyppi.class), any(FunktiokutsuTyyppi.class));


    }

    @Test
    public void testEdellistaVaihettaEiLoydy() {
        final String hakukohdeOid1 = "hakukohdeOid1";

        final String hakukohdeOid2 = "hakukohdeOid2";

        final HakemusTyyppi hakemus = luoHakemus("hakemusOid", "hakijaOid", hakukohdeOid1, hakukohdeOid2);

        final String hakuOid = "hakuOid";
        final String valintakoetunniste = "{{hakukohde.movember}}";

        final String valinnanVaiheOid1 = "valinnanVaiheOid1";
        final int valinnanVaiheJarjestysluku1 = 1;

        ValintaperusteetTyyppi valintaperusteet1 = luoValintaperusteetJaValintakoeValinnanvaihe(hakuOid, hakukohdeOid1, valinnanVaiheOid1,
                valinnanVaiheJarjestysluku1, valintakoetunniste);
        HakukohteenValintaperusteTyyppi hkvp = new HakukohteenValintaperusteTyyppi();
        hkvp.setArvo("koetunniste");
        hkvp.setTunniste("movember");

        valintaperusteet1.getHakukohteenValintaperuste().add(hkvp);


        final OsallistuminenTulos osallistuminenTulos = new OsallistuminenTulos();
        osallistuminenTulos.setOsallistuminen(Osallistuminen.OSALLISTUU);
        when(valintakoeosallistumislaskinMock.laskeOsallistuminenYhdelleHakukohteelle(any(Hakukohde.class), any(HakemusTyyppi.class), any(FunktiokutsuTyyppi.class))).thenReturn(osallistuminenTulos);

        valintakoelaskentaSuorittajaService.laske(hakemus, Arrays.asList(valintaperusteet1));
        verify(valintakoeOsallistuminenDAOMock, times(0)).createOrUpdate(any(ValintakoeOsallistuminen.class));


    }


    @Test
    public void testViimeisinValinnanvaihe() {
        final String hakukohdeOid1 = "hakukohdeOid1";
        final String hakukohdeOid2 = "hakukohdeOid2";
        final HakemusTyyppi hakemus = luoHakemus("hakemusOid", "hakijaOid", hakukohdeOid1, hakukohdeOid2);

        final String hakuOid = "hakuOid";
        final String valintakoetunniste = "valintakoetunniste";

        final String valinnanVaiheOid1 = "valinnanVaiheOid1";
        final int valinnanVaiheJarjestysluku1 = 2;

        ValintaperusteetTyyppi valintaperusteet1 = luoValintaperusteetJaValintakoeValinnanvaihe(hakuOid, hakukohdeOid1, valinnanVaiheOid1,
                valinnanVaiheJarjestysluku1, valintakoetunniste);

        final String valinnanVaiheOid2 = "valinnanVaiheOid2";
        final int valinnanVaiheJarjestysluku2 = 2;

        ValintaperusteetTyyppi valintaperusteet2 = luoValintaperusteetJaValintakoeValinnanvaihe(hakuOid, hakukohdeOid2, valinnanVaiheOid2,
                valinnanVaiheJarjestysluku2, valintakoetunniste);

        when(valinnanvaiheDAOMock.haeEdeltavaValinnanvaihe(Matchers.<String>any(), Matchers.eq(hakukohdeOid1), Matchers.eq(valinnanVaiheJarjestysluku1))).thenReturn(null);
        when(valinnanvaiheDAOMock.haeEdeltavaValinnanvaihe(Matchers.<String>any(), Matchers.eq(hakukohdeOid2), Matchers.eq(valinnanVaiheJarjestysluku2))).thenReturn(null);

        when(valintakoeOsallistuminenDAOMock.haeEdeltavaValinnanvaihe(Matchers.eq(hakuOid), Matchers.eq(hakukohdeOid1), Matchers.eq(valinnanVaiheJarjestysluku1))).thenReturn(new ValintakoeOsallistuminen());
        when(valintakoeOsallistuminenDAOMock.haeEdeltavaValinnanvaihe(Matchers.eq(hakuOid), Matchers.eq(hakukohdeOid2), Matchers.eq(valinnanVaiheJarjestysluku2))).thenReturn(new ValintakoeOsallistuminen());

        Valinnanvaihe viimeisin1 = new Valinnanvaihe();
        Valinnanvaihe viimeisin2 = new Valinnanvaihe();
        when(valinnanvaiheDAOMock.haeViimeisinValinnanvaihe(Matchers.<String>any(), Matchers.eq(hakukohdeOid1), Matchers.eq(valinnanVaiheJarjestysluku1))).thenReturn(viimeisin1);
        when(valinnanvaiheDAOMock.haeViimeisinValinnanvaihe(Matchers.<String>any(), Matchers.eq(hakukohdeOid2), Matchers.eq(valinnanVaiheJarjestysluku2))).thenReturn(viimeisin2);

        final TilaJaSelite ts0 = new TilaJaSelite(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, "Testi Selite Hyvaksyttavissa");
        final TilaJaSelite ts1 = new TilaJaSelite(JarjestyskriteerituloksenTila.VIRHE, "Testi Selite Virhe");

        when(edellinenValinnanvaiheKasittelijaMock.tilaEdellisenValinnanvaiheenMukaan(Matchers.<String>any(), Matchers.<Tila>any(), Matchers.eq(viimeisin1))).thenReturn(ts0);
        when(edellinenValinnanvaiheKasittelijaMock.tilaEdellisenValinnanvaiheenMukaan(Matchers.<String>any(), Matchers.<Tila>any(), Matchers.eq(viimeisin2))).thenReturn(ts1);

        final OsallistuminenTulos osallistuu1 = new OsallistuminenTulos();
        osallistuu1.setOsallistuminen(Osallistuminen.OSALLISTUU);

        final OsallistuminenTulos osallistuu2 = new OsallistuminenTulos();
        osallistuu2.setOsallistuminen(Osallistuminen.OSALLISTUU);

        when(valintakoeosallistumislaskinMock.laskeOsallistuminenYhdelleHakukohteelle(argThat(new BaseMatcher<Hakukohde>() {
            @Override
            public boolean matches(Object o) {
                return o != null && ((Hakukohde) o).hakukohdeOid().equals(hakukohdeOid1);
            }

            @Override
            public void describeTo(Description description) {
            }
        }), Matchers.<HakemusTyyppi>any(), Matchers.<FunktiokutsuTyyppi>any())).thenReturn(osallistuu1);

        when(valintakoeosallistumislaskinMock.laskeOsallistuminenYhdelleHakukohteelle(argThat(new BaseMatcher<Hakukohde>() {
            @Override
            public boolean matches(Object o) {
                return o != null && ((Hakukohde) o).hakukohdeOid().equals(hakukohdeOid2);

            }

            @Override
            public void describeTo(Description description) {
            }
        }), Matchers.<HakemusTyyppi>any(), Matchers.<FunktiokutsuTyyppi>any())).thenReturn(osallistuu2);

        when(valintakoeOsallistuminenDAOMock.readByHakuOidAndHakemusOid(anyString(), anyString())).thenReturn(null);

        ArgumentCaptor<ValintakoeOsallistuminen> captor = ArgumentCaptor.forClass(ValintakoeOsallistuminen.class);
        List<ValintaperusteetTyyppi> valintaperusteet = new ArrayList<ValintaperusteetTyyppi>();
        valintaperusteet.add(valintaperusteet1);
        valintaperusteet.add(valintaperusteet2);

        valintakoelaskentaSuorittajaService.laske(hakemus, valintaperusteet);
        verify(valintakoeOsallistuminenDAOMock, times(1)).createOrUpdate(captor.capture());

        ValintakoeOsallistuminen osallistuminen = captor.getValue();

        List<Hakutoive> hakutoiveet = osallistuminen.getHakutoiveet();
        Collections.sort(hakutoiveet, new Comparator<Hakutoive>() {
            @Override
            public int compare(Hakutoive o1, Hakutoive o2) {
                return o1.getHakukohdeOid().compareTo(o2.getHakukohdeOid());
            }
        });
        {
            Hakutoive hakutoive1 = hakutoiveet.get(0);

            ValintakoeValinnanvaihe vaihe1 = hakutoive1.getValinnanVaiheet().get(0);

            Valintakoe vk1 = vaihe1.getValintakokeet().get(0);
            assertEquals(Osallistuminen.OSALLISTUU, vk1.getOsallistuminenTulos().getOsallistuminen());
        }

        {
            Hakutoive hakutoive2 = hakutoiveet.get(1);

            ValintakoeValinnanvaihe vaihe2 = hakutoive2.getValinnanVaiheet().get(0);

            Valintakoe vk1 = vaihe2.getValintakokeet().get(0);
            assertEquals(Osallistuminen.EI_OSALLISTU, vk1.getOsallistuminenTulos().getOsallistuminen());
        }
    }

    @Test
    public void testEdellisetValinnanvaiheet() {

        final String hakukohdeOid1 = "hakukohdeOid1";

        final String hakukohdeOid2 = "hakukohdeOid2";

        final HakemusTyyppi hakemus = luoHakemus("hakemusOid", "hakijaOid", hakukohdeOid1, hakukohdeOid2);

        final String hakuOid = "hakuOid";
        final String valintakoetunniste = "valintakoetunniste";

        final String valinnanVaiheOid1 = "valinnanVaiheOid1";
        final int valinnanVaiheJarjestysluku1 = 2;

        ValintaperusteetTyyppi valintaperusteet1 = luoValintaperusteetJaValintakoeValinnanvaihe(hakuOid, hakukohdeOid1, valinnanVaiheOid1,
                valinnanVaiheJarjestysluku1, valintakoetunniste);

        final String valinnanVaiheOid2 = "valinnanVaiheOid2";
        final int valinnanVaiheJarjestysluku2 = 2;


        ValintaperusteetTyyppi valintaperusteet2 = luoValintaperusteetJaValintakoeValinnanvaihe(hakuOid, hakukohdeOid2, valinnanVaiheOid2,
                valinnanVaiheJarjestysluku2, valintakoetunniste);

        Valinnanvaihe v0 = new Valinnanvaihe();

        Valinnanvaihe v1 = new Valinnanvaihe();

        final TilaJaSelite ts0 = new TilaJaSelite(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, "Testi Selite Hyvaksyttavissa");
        final TilaJaSelite ts1 = new TilaJaSelite(JarjestyskriteerituloksenTila.VIRHE, "Testi Selite Virhe");

        when(valinnanvaiheDAOMock.haeEdeltavaValinnanvaihe(Matchers.<String>any(), Matchers.eq(hakukohdeOid1), Matchers.eq(valinnanVaiheJarjestysluku1))).thenReturn(v0);
        when(valinnanvaiheDAOMock.haeEdeltavaValinnanvaihe(Matchers.<String>any(), Matchers.eq(hakukohdeOid2), Matchers.eq(valinnanVaiheJarjestysluku2))).thenReturn(v1);

        when(edellinenValinnanvaiheKasittelijaMock.tilaEdellisenValinnanvaiheenMukaan(Matchers.<String>any(), Matchers.<Tila>any(), Matchers.eq(v0))).thenReturn(ts0);
        when(edellinenValinnanvaiheKasittelijaMock.tilaEdellisenValinnanvaiheenMukaan(Matchers.<String>any(), Matchers.<Tila>any(), Matchers.eq(v1))).thenReturn(ts1);

        final OsallistuminenTulos osallistuu1 = new OsallistuminenTulos();
        osallistuu1.setOsallistuminen(Osallistuminen.OSALLISTUU);

        final OsallistuminenTulos osallistuu2 = new OsallistuminenTulos();
        osallistuu2.setOsallistuminen(Osallistuminen.OSALLISTUU);

        when(valintakoeosallistumislaskinMock.laskeOsallistuminenYhdelleHakukohteelle(argThat(new BaseMatcher<Hakukohde>() {
            @Override
            public boolean matches(Object o) {
                return o != null && ((Hakukohde) o).hakukohdeOid().equals(hakukohdeOid1);
            }

            @Override
            public void describeTo(Description description) {
            }
        }), Matchers.<HakemusTyyppi>any(), Matchers.<FunktiokutsuTyyppi>any())).thenReturn(osallistuu1);

        when(valintakoeosallistumislaskinMock.laskeOsallistuminenYhdelleHakukohteelle(argThat(new BaseMatcher<Hakukohde>() {
            @Override
            public boolean matches(Object o) {
                return o != null && ((Hakukohde) o).hakukohdeOid().equals(hakukohdeOid2);

            }

            @Override
            public void describeTo(Description description) {
            }
        }), Matchers.<HakemusTyyppi>any(), Matchers.<FunktiokutsuTyyppi>any())).thenReturn(osallistuu2);

        when(valintakoeOsallistuminenDAOMock.readByHakuOidAndHakemusOid(anyString(), anyString())).thenReturn(null);

        ArgumentCaptor<ValintakoeOsallistuminen> captor = ArgumentCaptor.forClass(ValintakoeOsallistuminen.class);
        List<ValintaperusteetTyyppi> valintaperusteet = new ArrayList<ValintaperusteetTyyppi>();
        valintaperusteet.add(valintaperusteet1);
        valintaperusteet.add(valintaperusteet2);

        valintakoelaskentaSuorittajaService.laske(hakemus, valintaperusteet);
        verify(valintakoeOsallistuminenDAOMock, times(1)).createOrUpdate(captor.capture());

        ValintakoeOsallistuminen osallistuminen = captor.getValue();

        List<Hakutoive> hakutoiveet = osallistuminen.getHakutoiveet();
        Collections.sort(hakutoiveet, new Comparator<Hakutoive>() {
            @Override
            public int compare(Hakutoive o1, Hakutoive o2) {
                return o1.getHakukohdeOid().compareTo(o2.getHakukohdeOid());
            }
        });
        {
            Hakutoive hakutoive1 = hakutoiveet.get(0);

            ValintakoeValinnanvaihe vaihe1 = hakutoive1.getValinnanVaiheet().get(0);

            Valintakoe vk1 = vaihe1.getValintakokeet().get(0);
            assertEquals(Osallistuminen.OSALLISTUU, vk1.getOsallistuminenTulos().getOsallistuminen());
        }

        {
            Hakutoive hakutoive2 = hakutoiveet.get(1);

            ValintakoeValinnanvaihe vaihe2 = hakutoive2.getValinnanVaiheet().get(0);

            Valintakoe vk1 = vaihe2.getValintakokeet().get(0);
            assertEquals(Osallistuminen.EI_OSALLISTU, vk1.getOsallistuminenTulos().getOsallistuminen());
        }

    }

    @Test
    public void testBasic() {

        final String hakukohdeOid1 = "hakukohdeOid1";

        final String hakukohdeOid2 = "hakukohdeOid2";

        final HakemusTyyppi hakemus = luoHakemus("hakemusOid", "hakijaOid", hakukohdeOid1, hakukohdeOid2);

        final String hakuOid = "hakuOid";
        final String valintakoetunniste = "valintakoetunniste";

        final String valinnanVaiheOid1 = "valinnanVaiheOid1";
        final int valinnanVaiheJarjestysluku1 = 0;

        ValintaperusteetTyyppi valintaperusteet1 = luoValintaperusteetJaValintakoeValinnanvaihe(hakuOid, hakukohdeOid1, valinnanVaiheOid1,
                valinnanVaiheJarjestysluku1, valintakoetunniste);

        final String valinnanVaiheOid2 = "valinnanVaiheOid2";
        final int valinnanVaiheJarjestysluku2 = 0;


        ValintaperusteetTyyppi valintaperusteet2 = luoValintaperusteetJaValintakoeValinnanvaihe(hakuOid, hakukohdeOid2, valinnanVaiheOid2,
                valinnanVaiheJarjestysluku2, valintakoetunniste);

        final OsallistuminenTulos osallistuu1 = new OsallistuminenTulos();
        osallistuu1.setOsallistuminen(Osallistuminen.OSALLISTUU);

        final OsallistuminenTulos osallistuu2 = new OsallistuminenTulos();
        osallistuu2.setOsallistuminen(Osallistuminen.OSALLISTUU);

        when(valintakoeosallistumislaskinMock.laskeOsallistuminenYhdelleHakukohteelle(argThat(new BaseMatcher<Hakukohde>() {
            @Override
            public boolean matches(Object o) {
                return o != null && ((Hakukohde) o).hakukohdeOid().equals(hakukohdeOid1);
            }

            @Override
            public void describeTo(Description description) {
            }
        }), Matchers.<HakemusTyyppi>any(), Matchers.<FunktiokutsuTyyppi>any())).thenReturn(osallistuu1);

        when(valintakoeosallistumislaskinMock.laskeOsallistuminenYhdelleHakukohteelle(argThat(new BaseMatcher<Hakukohde>() {
            @Override
            public boolean matches(Object o) {
                return o != null && ((Hakukohde) o).hakukohdeOid().equals(hakukohdeOid2);

            }

            @Override
            public void describeTo(Description description) {
            }
        }), Matchers.<HakemusTyyppi>any(), Matchers.<FunktiokutsuTyyppi>any())).thenReturn(osallistuu2);

        when(valintakoeOsallistuminenDAOMock.readByHakuOidAndHakemusOid(anyString(), anyString())).thenReturn(null);

        ArgumentCaptor<ValintakoeOsallistuminen> captor = ArgumentCaptor.forClass(ValintakoeOsallistuminen.class);
        List<ValintaperusteetTyyppi> valintaperusteet = new ArrayList<ValintaperusteetTyyppi>();
        valintaperusteet.add(valintaperusteet1);
        valintaperusteet.add(valintaperusteet2);

        valintakoelaskentaSuorittajaService.laske(hakemus, valintaperusteet);
        verify(valintakoeOsallistuminenDAOMock, times(1)).createOrUpdate(captor.capture());

        ValintakoeOsallistuminen osallistuminen = captor.getValue();

        assertEquals(hakemus.getHakemusOid(), osallistuminen.getHakemusOid());
        assertEquals(hakemus.getHakijaOid(), osallistuminen.getHakijaOid());
        assertEquals(hakuOid, osallistuminen.getHakuOid());
        assertEquals(2, osallistuminen.getHakutoiveet().size());

        List<Hakutoive> hakutoiveet = osallistuminen.getHakutoiveet();
        Collections.sort(hakutoiveet, new Comparator<Hakutoive>() {
            @Override
            public int compare(Hakutoive o1, Hakutoive o2) {
                return o1.getHakukohdeOid().compareTo(o2.getHakukohdeOid());
            }
        });
        {
            Hakutoive hakutoive1 = hakutoiveet.get(0);
            assertEquals(hakutoive1.getHakukohdeOid(), hakukohdeOid1);
            assertEquals(1, hakutoive1.getValinnanVaiheet().size());

            ValintakoeValinnanvaihe vaihe1 = hakutoive1.getValinnanVaiheet().get(0);
            assertEquals(valinnanVaiheOid1, vaihe1.getValinnanVaiheOid());
            assertEquals(valinnanVaiheJarjestysluku1, vaihe1.getValinnanVaiheJarjestysluku().intValue());
            assertEquals(1, vaihe1.getValintakokeet().size());

            Valintakoe vk1 = vaihe1.getValintakokeet().get(0);
            assertEquals(valintakoetunniste, vk1.getValintakoeOid());
            assertEquals(valintakoetunniste, vk1.getValintakoeTunniste());
            assertEquals(Osallistuminen.OSALLISTUU, vk1.getOsallistuminenTulos().getOsallistuminen());
        }

        {
            Hakutoive hakutoive2 = hakutoiveet.get(1);
            assertEquals(hakukohdeOid2, hakutoive2.getHakukohdeOid());
            assertEquals(1, hakutoive2.getValinnanVaiheet().size());

            ValintakoeValinnanvaihe vaihe2 = hakutoive2.getValinnanVaiheet().get(0);
            assertEquals(valinnanVaiheOid2, vaihe2.getValinnanVaiheOid());
            assertEquals(valinnanVaiheJarjestysluku2, vaihe2.getValinnanVaiheJarjestysluku().intValue());
            assertEquals(1, vaihe2.getValintakokeet().size());

            Valintakoe vk2 = vaihe2.getValintakokeet().get(0);
            assertEquals(valintakoetunniste, vk2.getValintakoeOid());
            assertEquals(valintakoetunniste, vk2.getValintakoeTunniste());
            assertEquals(Osallistuminen.EI_OSALLISTU, vk2.getOsallistuminenTulos().getOsallistuminen());
        }
    }

    @Test
    public void testAsetaOsallistumisetKokeisiin() {
        Map<String, HakukohdeTyyppi> hakuToiveetByOid = new HashMap<String, HakukohdeTyyppi>();

        final String hakukohdeOid1 = "hakukohdeOid1";
        final String hakukohdeOid2 = "hakukohdeOid2";
        final String hakukohdeOid3 = "hakukohdeOid3";

        hakuToiveetByOid.put(hakukohdeOid1, luoHakukohdeTyyppi(hakukohdeOid1, 1));
        hakuToiveetByOid.put(hakukohdeOid3, luoHakukohdeTyyppi(hakukohdeOid2, 3));
        hakuToiveetByOid.put(hakukohdeOid2, luoHakukohdeTyyppi(hakukohdeOid3, 2));


        final String valintakoetunniste = "valintakoetunniste";

        {
            List<HakukohdeValintakoeData> kokeet = new ArrayList<HakukohdeValintakoeData>();
            kokeet.add(luoHakukohdeValintakoeData(hakukohdeOid1, Osallistuminen.OSALLISTUU, valintakoetunniste));
            kokeet.add(luoHakukohdeValintakoeData(hakukohdeOid2, Osallistuminen.OSALLISTUU, valintakoetunniste));
            kokeet.add(luoHakukohdeValintakoeData(hakukohdeOid3, Osallistuminen.OSALLISTUU, valintakoetunniste));

            // Testataan protected-metodia. Tää on vähän kyseenalaista.
            ReflectionTestUtils.invokeMethod(valintakoelaskentaSuorittajaService, "asetaOsallistumisetKokeisiin",
                    kokeet, hakuToiveetByOid);

            assertEquals(3, kokeet.size());
            assertEquals(hakukohdeOid1, kokeet.get(0).getHakukohdeOid());
            assertEquals(Osallistuminen.OSALLISTUU, kokeet.get(0).getOsallistuminenTulos().getOsallistuminen());

            assertEquals(hakukohdeOid2, kokeet.get(1).getHakukohdeOid());
            assertEquals(Osallistuminen.EI_OSALLISTU, kokeet.get(1).getOsallistuminenTulos().getOsallistuminen());

            assertEquals(hakukohdeOid3, kokeet.get(2).getHakukohdeOid());
            assertEquals(Osallistuminen.EI_OSALLISTU, kokeet.get(2).getOsallistuminenTulos().getOsallistuminen());
        }

        {
            List<HakukohdeValintakoeData> kokeet = new ArrayList<HakukohdeValintakoeData>();
            kokeet.add(luoHakukohdeValintakoeData(hakukohdeOid1, Osallistuminen.EI_OSALLISTU, valintakoetunniste));
            kokeet.add(luoHakukohdeValintakoeData(hakukohdeOid2, Osallistuminen.OSALLISTUU, valintakoetunniste));
            kokeet.add(luoHakukohdeValintakoeData(hakukohdeOid3, Osallistuminen.OSALLISTUU, valintakoetunniste));

            ReflectionTestUtils.invokeMethod(valintakoelaskentaSuorittajaService, "asetaOsallistumisetKokeisiin",
                    kokeet, hakuToiveetByOid);

            assertEquals(3, kokeet.size());
            assertEquals(hakukohdeOid1, kokeet.get(0).getHakukohdeOid());
            assertEquals(Osallistuminen.EI_OSALLISTU, kokeet.get(0).getOsallistuminenTulos().getOsallistuminen());

            assertEquals(hakukohdeOid2, kokeet.get(1).getHakukohdeOid());
            assertEquals(Osallistuminen.OSALLISTUU, kokeet.get(1).getOsallistuminenTulos().getOsallistuminen());

            assertEquals(hakukohdeOid3, kokeet.get(2).getHakukohdeOid());
            assertEquals(Osallistuminen.EI_OSALLISTU, kokeet.get(2).getOsallistuminenTulos().getOsallistuminen());
        }

        {
            List<HakukohdeValintakoeData> kokeet = new ArrayList<HakukohdeValintakoeData>();
            kokeet.add(luoHakukohdeValintakoeData(hakukohdeOid1, Osallistuminen.EI_OSALLISTU, valintakoetunniste));
            kokeet.add(luoHakukohdeValintakoeData(hakukohdeOid2, Osallistuminen.EI_OSALLISTU, valintakoetunniste));
            kokeet.add(luoHakukohdeValintakoeData(hakukohdeOid3, Osallistuminen.OSALLISTUU, valintakoetunniste));

            ReflectionTestUtils.invokeMethod(valintakoelaskentaSuorittajaService, "asetaOsallistumisetKokeisiin",
                    kokeet, hakuToiveetByOid);

            assertEquals(3, kokeet.size());
            assertEquals(hakukohdeOid1, kokeet.get(0).getHakukohdeOid());
            assertEquals(Osallistuminen.EI_OSALLISTU, kokeet.get(0).getOsallistuminenTulos().getOsallistuminen());

            assertEquals(hakukohdeOid2, kokeet.get(1).getHakukohdeOid());
            assertEquals(Osallistuminen.EI_OSALLISTU, kokeet.get(1).getOsallistuminenTulos().getOsallistuminen());

            assertEquals(hakukohdeOid3, kokeet.get(2).getHakukohdeOid());
            assertEquals(Osallistuminen.OSALLISTUU, kokeet.get(2).getOsallistuminenTulos().getOsallistuminen());
        }

        {
            List<HakukohdeValintakoeData> kokeet = new ArrayList<HakukohdeValintakoeData>();
            kokeet.add(luoHakukohdeValintakoeData(hakukohdeOid1, Osallistuminen.EI_OSALLISTU, valintakoetunniste));
            kokeet.add(luoHakukohdeValintakoeData(hakukohdeOid2, Osallistuminen.EI_OSALLISTU, valintakoetunniste));
            kokeet.add(luoHakukohdeValintakoeData(hakukohdeOid3, Osallistuminen.EI_OSALLISTU, valintakoetunniste));

            ReflectionTestUtils.invokeMethod(valintakoelaskentaSuorittajaService, "asetaOsallistumisetKokeisiin",
                    kokeet, hakuToiveetByOid);

            assertEquals(3, kokeet.size());
            assertEquals(hakukohdeOid1, kokeet.get(0).getHakukohdeOid());
            assertEquals(Osallistuminen.EI_OSALLISTU, kokeet.get(0).getOsallistuminenTulos().getOsallistuminen());

            assertEquals(hakukohdeOid2, kokeet.get(1).getHakukohdeOid());
            assertEquals(Osallistuminen.EI_OSALLISTU, kokeet.get(1).getOsallistuminenTulos().getOsallistuminen());

            assertEquals(hakukohdeOid3, kokeet.get(2).getHakukohdeOid());
            assertEquals(Osallistuminen.EI_OSALLISTU, kokeet.get(2).getOsallistuminenTulos().getOsallistuminen());
        }

        {
            List<HakukohdeValintakoeData> kokeet = new ArrayList<HakukohdeValintakoeData>();
            kokeet.add(luoHakukohdeValintakoeData(hakukohdeOid1, Osallistuminen.OSALLISTUU, valintakoetunniste));
            kokeet.add(luoHakukohdeValintakoeData(hakukohdeOid2, Osallistuminen.EI_OSALLISTU, valintakoetunniste));
            kokeet.add(luoHakukohdeValintakoeData(hakukohdeOid3, Osallistuminen.EI_OSALLISTU, valintakoetunniste));

            ReflectionTestUtils.invokeMethod(valintakoelaskentaSuorittajaService, "asetaOsallistumisetKokeisiin",
                    kokeet, hakuToiveetByOid);

            assertEquals(3, kokeet.size());
            assertEquals(hakukohdeOid1, kokeet.get(0).getHakukohdeOid());
            assertEquals(Osallistuminen.OSALLISTUU, kokeet.get(0).getOsallistuminenTulos().getOsallistuminen());

            assertEquals(hakukohdeOid2, kokeet.get(1).getHakukohdeOid());
            assertEquals(Osallistuminen.EI_OSALLISTU, kokeet.get(1).getOsallistuminenTulos().getOsallistuminen());

            assertEquals(hakukohdeOid3, kokeet.get(2).getHakukohdeOid());
            assertEquals(Osallistuminen.EI_OSALLISTU, kokeet.get(2).getOsallistuminenTulos().getOsallistuminen());
        }
    }
}
