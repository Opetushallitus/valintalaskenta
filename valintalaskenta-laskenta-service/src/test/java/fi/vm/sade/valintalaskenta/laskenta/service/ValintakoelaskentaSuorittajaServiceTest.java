package fi.vm.sade.valintalaskenta.laskenta.service;

import static fi.vm.sade.valintalaskenta.laskenta.testdata.TestDataUtil.luoHakemus;
import static fi.vm.sade.valintalaskenta.laskenta.testdata.TestDataUtil.luoHakukohdeDTO;
import static fi.vm.sade.valintalaskenta.laskenta.testdata.TestDataUtil.luoHakukohdeValintakoeData;
import static fi.vm.sade.valintalaskenta.laskenta.testdata.TestDataUtil.luoValintaperusteetJaValintakoeValinnanvaihe;
import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.argThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import fi.vm.sade.service.valintaperusteet.dto.HakukohteenValintaperusteDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakemus;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakukohde;
import fi.vm.sade.service.valintaperusteet.model.Funktiokutsu;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakukohdeDTO;
import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Hakutoive;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.OsallistuminenTulos;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Valintakoe;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeValinnanvaihe;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValinnanvaiheDAO;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValintakoeOsallistuminenDAO;
import fi.vm.sade.valintalaskenta.laskenta.resource.ValintakoelaskennanKumulatiivisetTulokset;
import fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion.HakemusDTOToHakemusConverter;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.EdellinenValinnanvaiheKasittelija;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.TilaJaSelite;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.ValintakoelaskentaSuorittajaService;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.Valintakoeosallistumislaskin;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.impl.ValintakoelaskentaSuorittajaServiceImpl;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.impl.util.HakukohdeValintakoeData;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import org.hamcrest.BaseMatcher;
import org.hamcrest.Description;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Matchers;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * User: wuoti Date: 3.5.2013 Time: 12.00
 */
public class ValintakoelaskentaSuorittajaServiceTest {
    private final String uuid = null;
    private ValintakoelaskentaSuorittajaService valintakoelaskentaSuorittajaService;

    private ValintakoeOsallistuminenDAO valintakoeOsallistuminenDAOMock;
    private Valintakoeosallistumislaskin valintakoeosallistumislaskinMock;

    private ValinnanvaiheDAO valinnanvaiheDAOMock;
    private EdellinenValinnanvaiheKasittelija edellinenValinnanvaiheKasittelijaMock;

    private ValintalaskentaModelMapper modelMapperMock;

    private HakemusDTOToHakemusConverter hakemusConverterMock;

    private ValintakoelaskennanKumulatiivisetTulokset kumulatiivisetTulokset = new ValintakoelaskennanKumulatiivisetTulokset();

    @Before
    public void setUp() {
        valintakoeOsallistuminenDAOMock = mock(ValintakoeOsallistuminenDAO.class);
        valintakoeosallistumislaskinMock = mock(Valintakoeosallistumislaskin.class);

        valinnanvaiheDAOMock = mock(ValinnanvaiheDAO.class);
        edellinenValinnanvaiheKasittelijaMock = mock(EdellinenValinnanvaiheKasittelija.class);

        modelMapperMock = mock(ValintalaskentaModelMapper.class);
        hakemusConverterMock = mock(HakemusDTOToHakemusConverter.class);

        valintakoelaskentaSuorittajaService = new ValintakoelaskentaSuorittajaServiceImpl(modelMapperMock,
            hakemusConverterMock, edellinenValinnanvaiheKasittelijaMock, valintakoeOsallistuminenDAOMock,
            valintakoeosallistumislaskinMock, valinnanvaiheDAOMock);
    }

    @Test
    public void testMustache() {
        final String hakukohdeOid1 = "hakukohdeOid1";
        final String hakukohdeOid2 = "hakukohdeOid2";

        final HakemusDTO hakemus = luoHakemus("hakuOid", "hakemusOid", "hakijaOid", hakukohdeOid1, hakukohdeOid2);

        final String hakuOid = "hakuOid";
        final String valintakoetunniste = "{{hakukohde.koetunniste}}";

        final String valinnanVaiheOid1 = "valinnanVaiheOid1";
        final int valinnanVaiheJarjestysluku1 = 0;

        ValintaperusteetDTO valintaperusteet1 = luoValintaperusteetJaValintakoeValinnanvaihe(
            hakuOid, hakukohdeOid1, valinnanVaiheOid1,
            valinnanVaiheJarjestysluku1, valintakoetunniste);
        HakukohteenValintaperusteDTO hkvp = new HakukohteenValintaperusteDTO();
        hkvp.setArvo("movember");
        hkvp.setTunniste("koetunniste");

        valintaperusteet1.getHakukohteenValintaperuste().add(hkvp);

        final OsallistuminenTulos osallistuminenTulos = new OsallistuminenTulos();
        osallistuminenTulos.setOsallistuminen(Osallistuminen.OSALLISTUU);
        when(valintakoeosallistumislaskinMock.laskeOsallistuminenYhdelleHakukohteelle(
            any(Hakukohde.class), any(Hakemus.class), any(Funktiokutsu.class)))
            .thenReturn(osallistuminenTulos);

        ArgumentCaptor<ValintakoeOsallistuminen> captor = ArgumentCaptor.forClass(ValintakoeOsallistuminen.class);
        valintakoelaskentaSuorittajaService.laske(hakemus, Collections.singletonList(valintaperusteet1), uuid, kumulatiivisetTulokset);
        verify(valintakoeOsallistuminenDAOMock, times(1)).createOrUpdate(captor.capture());

        ValintakoeOsallistuminen osallistuminen = captor.getValue();
        assertEquals("movember", osallistuminen.getHakutoiveet().get(0)
            .getValinnanVaiheet().get(0).getValintakokeet().get(0)
            .getValintakoeTunniste());
    }

    @Test
    public void testMustacheFail() {
        final String hakukohdeOid1 = "hakukohdeOid1";
        final String hakukohdeOid2 = "hakukohdeOid2";

        final HakemusDTO hakemus = luoHakemus("hakuOid", "hakemusOid", "hakijaOid", hakukohdeOid1, hakukohdeOid2);

        final String hakuOid = "hakuOid";
        final String valintakoetunniste = "{{hakukohde.koetunnistea}}";

        final String valinnanVaiheOid1 = "valinnanVaiheOid1";
        final int valinnanVaiheJarjestysluku1 = 1;

        ValintaperusteetDTO valintaperusteet1 = luoValintaperusteetJaValintakoeValinnanvaihe(
            hakuOid, hakukohdeOid1, valinnanVaiheOid1,
            valinnanVaiheJarjestysluku1, valintakoetunniste);
        HakukohteenValintaperusteDTO hkvp = new HakukohteenValintaperusteDTO();
        hkvp.setArvo("movember");
        hkvp.setTunniste("koetunniste");

        valintaperusteet1.getHakukohteenValintaperuste().add(hkvp);

        final OsallistuminenTulos osallistuminenTulos = new OsallistuminenTulos();
        osallistuminenTulos.setOsallistuminen(Osallistuminen.OSALLISTUU);
        when(valintakoeosallistumislaskinMock.laskeOsallistuminenYhdelleHakukohteelle(
            any(Hakukohde.class), any(Hakemus.class), any(Funktiokutsu.class)))
            .thenReturn(osallistuminenTulos);

        valintakoelaskentaSuorittajaService.laske(hakemus, Collections.singletonList(valintaperusteet1), uuid, kumulatiivisetTulokset);
        verify(valintakoeOsallistuminenDAOMock, times(0)).createOrUpdate(any(ValintakoeOsallistuminen.class));

        verify(valintakoeosallistumislaskinMock, times(0)).laskeOsallistuminenYhdelleHakukohteelle(any(Hakukohde.class),
                any(Hakemus.class), any(Funktiokutsu.class));
    }

    @Test
    public void testEdellistaVaihettaEiLoydy() {
        final String hakukohdeOid1 = "hakukohdeOid1";
        final String hakukohdeOid2 = "hakukohdeOid2";

        final HakemusDTO hakemus = luoHakemus("hakuOid", "hakemusOid", "hakijaOid", hakukohdeOid1, hakukohdeOid2);

        final String hakuOid = "hakuOid";
        final String valintakoetunniste = "{{hakukohde.movember}}";

        final String valinnanVaiheOid1 = "valinnanVaiheOid1";
        final int valinnanVaiheJarjestysluku1 = 1;

        ValintaperusteetDTO valintaperusteet1 = luoValintaperusteetJaValintakoeValinnanvaihe(
            hakuOid, hakukohdeOid1, valinnanVaiheOid1,
            valinnanVaiheJarjestysluku1, valintakoetunniste);
        HakukohteenValintaperusteDTO hkvp = new HakukohteenValintaperusteDTO();
        hkvp.setArvo("koetunniste");
        hkvp.setTunniste("movember");

        valintaperusteet1.getHakukohteenValintaperuste().add(hkvp);

        final OsallistuminenTulos osallistuminenTulos = new OsallistuminenTulos();
        osallistuminenTulos.setOsallistuminen(Osallistuminen.OSALLISTUU);
        when(valintakoeosallistumislaskinMock.laskeOsallistuminenYhdelleHakukohteelle(any(Hakukohde.class), any(Hakemus.class), any(Funktiokutsu.class)))
            .thenReturn(osallistuminenTulos);

        valintakoelaskentaSuorittajaService.laske(hakemus, Collections.singletonList(valintaperusteet1), uuid, kumulatiivisetTulokset);
        verify(valintakoeOsallistuminenDAOMock, times(0)).createOrUpdate(any(ValintakoeOsallistuminen.class));
    }

    @Test
    public void testViimeisinValinnanvaihe() {
        final String hakukohdeOid1 = "hakukohdeOid1";
        final String hakukohdeOid2 = "hakukohdeOid2";
        final HakemusDTO hakemus = luoHakemus("hakuOid", "hakemusOid", "hakijaOid", hakukohdeOid1, hakukohdeOid2);

        final String hakuOid = "hakuOid";
        final String valintakoetunniste = "valintakoetunniste";

        final String valinnanVaiheOid1 = "valinnanVaiheOid1";
        final int valinnanVaiheJarjestysluku1 = 2;

        ValintaperusteetDTO valintaperusteet1 = luoValintaperusteetJaValintakoeValinnanvaihe(
            hakuOid, hakukohdeOid1, valinnanVaiheOid1,
            valinnanVaiheJarjestysluku1, valintakoetunniste);

        final String valinnanVaiheOid2 = "valinnanVaiheOid2";
        final int valinnanVaiheJarjestysluku2 = 2;

        ValintaperusteetDTO valintaperusteet2 = luoValintaperusteetJaValintakoeValinnanvaihe(hakuOid, hakukohdeOid2,
            valinnanVaiheOid2, valinnanVaiheJarjestysluku2, valintakoetunniste);

        when(valinnanvaiheDAOMock.haeEdeltavaValinnanvaihe(Matchers.any(), Matchers.eq(hakukohdeOid1), Matchers.eq(valinnanVaiheJarjestysluku1)))
            .thenReturn(null);
        when(valinnanvaiheDAOMock.haeEdeltavaValinnanvaihe(Matchers.any(), Matchers.eq(hakukohdeOid2), Matchers.eq(valinnanVaiheJarjestysluku2)))
            .thenReturn(null);

        when(valintakoeOsallistuminenDAOMock.haeEdeltavaValinnanvaihe(Matchers.eq(hakuOid), Matchers.eq(hakukohdeOid1), Matchers.eq(valinnanVaiheJarjestysluku1)))
            .thenReturn(new ValintakoeOsallistuminen());
        when(valintakoeOsallistuminenDAOMock.haeEdeltavaValinnanvaihe(Matchers.eq(hakuOid), Matchers.eq(hakukohdeOid2), Matchers.eq(valinnanVaiheJarjestysluku2)))
            .thenReturn(new ValintakoeOsallistuminen());

        Valinnanvaihe viimeisin1 = new Valinnanvaihe();
        Valinnanvaihe viimeisin2 = new Valinnanvaihe();
        when(valinnanvaiheDAOMock.haeViimeisinValinnanvaihe(Matchers.any(), Matchers.eq(hakukohdeOid1), Matchers.eq(valinnanVaiheJarjestysluku1)))
            .thenReturn(viimeisin1);
        when(valinnanvaiheDAOMock.haeViimeisinValinnanvaihe(Matchers.any(), Matchers.eq(hakukohdeOid2), Matchers.eq(valinnanVaiheJarjestysluku2)))
            .thenReturn(viimeisin2);

        final Map<String, String> hyvaksyttavissaSelite = new HashMap<>();
        hyvaksyttavissaSelite.put("FI", "Testi Selite Hyvaksyttavissa");

        final Map<String, String> virheSelite = new HashMap<>();
        virheSelite.put("FI", "Testi Selite Virhe");

        final TilaJaSelite ts0 = new TilaJaSelite(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, hyvaksyttavissaSelite);
        final TilaJaSelite ts1 = new TilaJaSelite(JarjestyskriteerituloksenTila.VIRHE, virheSelite);

        when(edellinenValinnanvaiheKasittelijaMock.tilaEdellisenValinnanvaiheenMukaan(Matchers.any(), Matchers.any(), Matchers.eq(viimeisin1)))
            .thenReturn(ts0);
        when(edellinenValinnanvaiheKasittelijaMock.tilaEdellisenValinnanvaiheenMukaan(Matchers.any(), Matchers.any(), Matchers.eq(viimeisin2)))
            .thenReturn(ts1);

        final OsallistuminenTulos osallistuu1 = new OsallistuminenTulos();
        osallistuu1.setOsallistuminen(Osallistuminen.OSALLISTUU);

        final OsallistuminenTulos osallistuu2 = new OsallistuminenTulos();
        osallistuu2.setOsallistuminen(Osallistuminen.OSALLISTUU);

        when(valintakoeosallistumislaskinMock.laskeOsallistuminenYhdelleHakukohteelle(
            argThat(new BaseMatcher<Hakukohde>() {
                @Override
                public boolean matches(Object o) {
                    return o != null && ((Hakukohde) o).hakukohdeOid().equals(hakukohdeOid1);
                }

                @Override
                public void describeTo(Description description) {
                }
            }), Matchers.any(), Matchers.any()))
            .thenReturn(osallistuu1);

        when(valintakoeosallistumislaskinMock.laskeOsallistuminenYhdelleHakukohteelle(
            argThat(new BaseMatcher<Hakukohde>() {
                @Override
                public boolean matches(Object o) {
                    return o != null && ((Hakukohde) o).hakukohdeOid().equals(hakukohdeOid2);
                }

                @Override
                public void describeTo(Description description) {
                }
            }), Matchers.any(), Matchers.any()))
            .thenReturn(osallistuu2);

        when(valintakoeOsallistuminenDAOMock.readByHakuOidAndHakemusOid(anyString(), anyString())).thenReturn(null);

        ArgumentCaptor<ValintakoeOsallistuminen> captor = ArgumentCaptor
            .forClass(ValintakoeOsallistuminen.class);
        List<ValintaperusteetDTO> valintaperusteet = new ArrayList<>();
        valintaperusteet.add(valintaperusteet1);
        valintaperusteet.add(valintaperusteet2);

        valintakoelaskentaSuorittajaService.laske(hakemus, valintaperusteet, uuid, kumulatiivisetTulokset);
        verify(valintakoeOsallistuminenDAOMock, times(1)).createOrUpdate(captor.capture());

        ValintakoeOsallistuminen osallistuminen = captor.getValue();

        List<Hakutoive> hakutoiveet = osallistuminen.getHakutoiveet();
        Collections.sort(hakutoiveet, (o1, o2) -> o1.getHakukohdeOid().compareTo(o2.getHakukohdeOid()));
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

        final HakemusDTO hakemus = luoHakemus("hakuOid", "hakemusOid", "hakijaOid", hakukohdeOid1, hakukohdeOid2);

        final String hakuOid = "hakuOid";
        final String valintakoetunniste = "valintakoetunniste";

        final String valinnanVaiheOid1 = "valinnanVaiheOid1";
        final int valinnanVaiheJarjestysluku1 = 2;

        ValintaperusteetDTO valintaperusteet1 = luoValintaperusteetJaValintakoeValinnanvaihe(
            hakuOid, hakukohdeOid1, valinnanVaiheOid1,
            valinnanVaiheJarjestysluku1, valintakoetunniste);

        final String valinnanVaiheOid2 = "valinnanVaiheOid2";
        final int valinnanVaiheJarjestysluku2 = 2;

        ValintaperusteetDTO valintaperusteet2 = luoValintaperusteetJaValintakoeValinnanvaihe(
            hakuOid, hakukohdeOid2, valinnanVaiheOid2,
            valinnanVaiheJarjestysluku2, valintakoetunniste);

        Valinnanvaihe v0 = new Valinnanvaihe();

        Valinnanvaihe v1 = new Valinnanvaihe();

        final Map<String, String> hyvaksyttavissaSelite = new HashMap<>();
        hyvaksyttavissaSelite.put("FI", "Testi Selite Hyvaksyttavissa");

        final Map<String, String> virheSelite = new HashMap<>();
        virheSelite.put("FI", "Testi Selite Virhe");

        final TilaJaSelite ts0 = new TilaJaSelite(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA,hyvaksyttavissaSelite);
        final TilaJaSelite ts1 = new TilaJaSelite(JarjestyskriteerituloksenTila.VIRHE, virheSelite);

        when(valinnanvaiheDAOMock.haeEdeltavaValinnanvaihe(Matchers.any(), Matchers.eq(hakukohdeOid1), Matchers.eq(valinnanVaiheJarjestysluku1)))
            .thenReturn(v0);
        when(valinnanvaiheDAOMock.haeEdeltavaValinnanvaihe(Matchers.any(), Matchers.eq(hakukohdeOid2), Matchers.eq(valinnanVaiheJarjestysluku2)))
            .thenReturn(v1);

        when(edellinenValinnanvaiheKasittelijaMock.tilaEdellisenValinnanvaiheenMukaan(Matchers.any(), Matchers.any(), Matchers.eq(v0)))
            .thenReturn(ts0);
        when(edellinenValinnanvaiheKasittelijaMock.tilaEdellisenValinnanvaiheenMukaan(Matchers.any(), Matchers.any(),Matchers.eq(v1)))
            .thenReturn(ts1);

        final OsallistuminenTulos osallistuu1 = new OsallistuminenTulos();
        osallistuu1.setOsallistuminen(Osallistuminen.OSALLISTUU);

        final OsallistuminenTulos osallistuu2 = new OsallistuminenTulos();
        osallistuu2.setOsallistuminen(Osallistuminen.OSALLISTUU);

        when(valintakoeosallistumislaskinMock.laskeOsallistuminenYhdelleHakukohteelle(
            argThat(new BaseMatcher<Hakukohde>() {
                @Override
                public boolean matches(Object o) {
                    return o != null && ((Hakukohde) o).hakukohdeOid().equals(hakukohdeOid1);
                }

                @Override
                public void describeTo(Description description) {
                }
            }), Matchers.any(), Matchers.any()))
            .thenReturn(osallistuu1);

        when(valintakoeosallistumislaskinMock.laskeOsallistuminenYhdelleHakukohteelle(
            argThat(new BaseMatcher<Hakukohde>() {
                @Override
                public boolean matches(Object o) {
                    return o != null && ((Hakukohde) o).hakukohdeOid().equals(hakukohdeOid2);
                }

                @Override
                public void describeTo(Description description) {
                }
            }), Matchers.any(), Matchers.any()))
            .thenReturn(osallistuu2);

        when(valintakoeOsallistuminenDAOMock.readByHakuOidAndHakemusOid(anyString(), anyString())).thenReturn(null);

        ArgumentCaptor<ValintakoeOsallistuminen> captor = ArgumentCaptor.forClass(ValintakoeOsallistuminen.class);
        List<ValintaperusteetDTO> valintaperusteet = new ArrayList<>();
        valintaperusteet.add(valintaperusteet1);
        valintaperusteet.add(valintaperusteet2);

        valintakoelaskentaSuorittajaService.laske(hakemus, valintaperusteet, uuid, kumulatiivisetTulokset);
        verify(valintakoeOsallistuminenDAOMock, times(1)).createOrUpdate(captor.capture());

        ValintakoeOsallistuminen osallistuminen = captor.getValue();

        List<Hakutoive> hakutoiveet = osallistuminen.getHakutoiveet();
        Collections.sort(hakutoiveet, (o1, o2) -> o1.getHakukohdeOid().compareTo(o2.getHakukohdeOid()));
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

        final HakemusDTO hakemus = luoHakemus("hakuOid", "hakemusOid", "hakijaOid", hakukohdeOid1, hakukohdeOid2);

        final String hakuOid = "hakuOid";
        final String valintakoetunniste = "valintakoetunniste";

        final String valinnanVaiheOid1 = "valinnanVaiheOid1";
        final int valinnanVaiheJarjestysluku1 = 0;

        ValintaperusteetDTO valintaperusteet1 = luoValintaperusteetJaValintakoeValinnanvaihe(
            hakuOid, hakukohdeOid1, valinnanVaiheOid1,
            valinnanVaiheJarjestysluku1, valintakoetunniste);

        final String valinnanVaiheOid2 = "valinnanVaiheOid2";
        final int valinnanVaiheJarjestysluku2 = 0;

        ValintaperusteetDTO valintaperusteet2 = luoValintaperusteetJaValintakoeValinnanvaihe(
            hakuOid, hakukohdeOid2, valinnanVaiheOid2,
            valinnanVaiheJarjestysluku2, valintakoetunniste);

        final OsallistuminenTulos osallistuu1 = new OsallistuminenTulos();
        osallistuu1.setOsallistuminen(Osallistuminen.OSALLISTUU);

        final OsallistuminenTulos osallistuu2 = new OsallistuminenTulos();
        osallistuu2.setOsallistuminen(Osallistuminen.OSALLISTUU);

        when(valintakoeosallistumislaskinMock.laskeOsallistuminenYhdelleHakukohteelle(
            argThat(new BaseMatcher<Hakukohde>() {
                @Override
                public boolean matches(Object o) {
                    return o != null && ((Hakukohde) o).hakukohdeOid().equals(hakukohdeOid1);
                }

                @Override
                public void describeTo(Description description) {
                }
            }), Matchers.any(), Matchers.any()))
            .thenReturn(osallistuu1);

        when(valintakoeosallistumislaskinMock.laskeOsallistuminenYhdelleHakukohteelle(
            argThat(new BaseMatcher<Hakukohde>() {
                @Override
                public boolean matches(Object o) {
                    return o != null && ((Hakukohde) o).hakukohdeOid().equals(hakukohdeOid2);
                }

                @Override
                public void describeTo(Description description) {
                }
            }), Matchers.any(), Matchers.any()))
            .thenReturn(osallistuu2);

        when(valintakoeOsallistuminenDAOMock.readByHakuOidAndHakemusOid(anyString(), anyString())).thenReturn(null);

        ArgumentCaptor<ValintakoeOsallistuminen> captor = ArgumentCaptor.forClass(ValintakoeOsallistuminen.class);
        List<ValintaperusteetDTO> valintaperusteet = new ArrayList<>();
        valintaperusteet.add(valintaperusteet1);
        valintaperusteet.add(valintaperusteet2);

        valintakoelaskentaSuorittajaService.laske(hakemus, valintaperusteet, uuid, kumulatiivisetTulokset);
        verify(valintakoeOsallistuminenDAOMock, times(1)).createOrUpdate(captor.capture());

        ValintakoeOsallistuminen osallistuminen = captor.getValue();

        assertEquals(hakemus.getHakemusoid(), osallistuminen.getHakemusOid());
        assertEquals(hakemus.getHakijaOid(), osallistuminen.getHakijaOid());
        assertEquals(hakuOid, osallistuminen.getHakuOid());
        assertEquals(2, osallistuminen.getHakutoiveet().size());

        List<Hakutoive> hakutoiveet = osallistuminen.getHakutoiveet();
        Collections.sort(hakutoiveet, (o1, o2) -> o1.getHakukohdeOid().compareTo(o2.getHakukohdeOid()));
        {
            Hakutoive hakutoive1 = hakutoiveet.get(0);
            assertEquals(hakutoive1.getHakukohdeOid(), hakukohdeOid1);
            assertEquals(1, hakutoive1.getValinnanVaiheet().size());

            ValintakoeValinnanvaihe vaihe1 = hakutoive1.getValinnanVaiheet().get(0);
            assertEquals(valinnanVaiheOid1, vaihe1.getValinnanVaiheOid());
            assertEquals(valinnanVaiheJarjestysluku1, vaihe1.getValinnanVaiheJarjestysluku().intValue());
            assertEquals(1, vaihe1.getValintakokeet().size());

            Valintakoe vk1 = vaihe1.getValintakokeet().get(0);
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
            assertEquals(valintakoetunniste, vk2.getValintakoeTunniste());
            assertEquals(Osallistuminen.EI_OSALLISTU, vk2.getOsallistuminenTulos().getOsallistuminen());
        }
    }

    @Test
    public void testAsetaOsallistumisetKokeisiin() {
        Map<String, HakukohdeDTO> hakuToiveetByOid = new HashMap<>();

        final String hakukohdeOid1 = "hakukohdeOid1";
        final String hakukohdeOid2 = "hakukohdeOid2";
        final String hakukohdeOid3 = "hakukohdeOid3";

        hakuToiveetByOid.put(hakukohdeOid1, luoHakukohdeDTO(hakukohdeOid1, 1));
        hakuToiveetByOid.put(hakukohdeOid3, luoHakukohdeDTO(hakukohdeOid2, 3));
        hakuToiveetByOid.put(hakukohdeOid2, luoHakukohdeDTO(hakukohdeOid3, 2));

        final String valintakoetunniste = "valintakoetunniste";

        {
            List<HakukohdeValintakoeData> kokeet = new ArrayList<>();
            kokeet.add(luoHakukohdeValintakoeData(hakukohdeOid1, Osallistuminen.OSALLISTUU, valintakoetunniste));
            kokeet.add(luoHakukohdeValintakoeData(hakukohdeOid2, Osallistuminen.OSALLISTUU, valintakoetunniste));
            kokeet.add(luoHakukohdeValintakoeData(hakukohdeOid3, Osallistuminen.OSALLISTUU, valintakoetunniste));

            // Testataan protected-metodia. Tää on vähän kyseenalaista.
            ReflectionTestUtils.invokeMethod(valintakoelaskentaSuorittajaService, "asetaOsallistumisetKokeisiin", kokeet, hakuToiveetByOid);

            assertEquals(3, kokeet.size());
            assertEquals(hakukohdeOid1, kokeet.get(0).getHakukohdeOid());
            assertEquals(Osallistuminen.OSALLISTUU, kokeet.get(0).getOsallistuminenTulos().getOsallistuminen());

            assertEquals(hakukohdeOid2, kokeet.get(1).getHakukohdeOid());
            assertEquals(Osallistuminen.EI_OSALLISTU, kokeet.get(1).getOsallistuminenTulos().getOsallistuminen());

            assertEquals(hakukohdeOid3, kokeet.get(2).getHakukohdeOid());
            assertEquals(Osallistuminen.EI_OSALLISTU, kokeet.get(2).getOsallistuminenTulos().getOsallistuminen());
        }

        {
            List<HakukohdeValintakoeData> kokeet = new ArrayList<>();
            kokeet.add(luoHakukohdeValintakoeData(hakukohdeOid1, Osallistuminen.EI_OSALLISTU, valintakoetunniste));
            kokeet.add(luoHakukohdeValintakoeData(hakukohdeOid2, Osallistuminen.OSALLISTUU, valintakoetunniste));
            kokeet.add(luoHakukohdeValintakoeData(hakukohdeOid3, Osallistuminen.OSALLISTUU, valintakoetunniste));

            ReflectionTestUtils.invokeMethod(valintakoelaskentaSuorittajaService, "asetaOsallistumisetKokeisiin", kokeet, hakuToiveetByOid);

            assertEquals(3, kokeet.size());
            assertEquals(hakukohdeOid1, kokeet.get(0).getHakukohdeOid());
            assertEquals(Osallistuminen.EI_OSALLISTU, kokeet.get(0).getOsallistuminenTulos().getOsallistuminen());

            assertEquals(hakukohdeOid2, kokeet.get(1).getHakukohdeOid());
            assertEquals(Osallistuminen.OSALLISTUU, kokeet.get(1).getOsallistuminenTulos().getOsallistuminen());

            assertEquals(hakukohdeOid3, kokeet.get(2).getHakukohdeOid());
            assertEquals(Osallistuminen.EI_OSALLISTU, kokeet.get(2).getOsallistuminenTulos().getOsallistuminen());
        }

        {
            List<HakukohdeValintakoeData> kokeet = new ArrayList<>();
            kokeet.add(luoHakukohdeValintakoeData(hakukohdeOid1, Osallistuminen.EI_OSALLISTU, valintakoetunniste));
            kokeet.add(luoHakukohdeValintakoeData(hakukohdeOid2, Osallistuminen.EI_OSALLISTU, valintakoetunniste));
            kokeet.add(luoHakukohdeValintakoeData(hakukohdeOid3, Osallistuminen.OSALLISTUU, valintakoetunniste));

            ReflectionTestUtils.invokeMethod(valintakoelaskentaSuorittajaService, "asetaOsallistumisetKokeisiin", kokeet, hakuToiveetByOid);

            assertEquals(3, kokeet.size());
            assertEquals(hakukohdeOid1, kokeet.get(0).getHakukohdeOid());
            assertEquals(Osallistuminen.EI_OSALLISTU, kokeet.get(0).getOsallistuminenTulos().getOsallistuminen());

            assertEquals(hakukohdeOid2, kokeet.get(1).getHakukohdeOid());
            assertEquals(Osallistuminen.EI_OSALLISTU, kokeet.get(1).getOsallistuminenTulos().getOsallistuminen());

            assertEquals(hakukohdeOid3, kokeet.get(2).getHakukohdeOid());
            assertEquals(Osallistuminen.OSALLISTUU, kokeet.get(2).getOsallistuminenTulos().getOsallistuminen());
        }

        {
            List<HakukohdeValintakoeData> kokeet = new ArrayList<>();
            kokeet.add(luoHakukohdeValintakoeData(hakukohdeOid1, Osallistuminen.EI_OSALLISTU, valintakoetunniste));
            kokeet.add(luoHakukohdeValintakoeData(hakukohdeOid2, Osallistuminen.EI_OSALLISTU, valintakoetunniste));
            kokeet.add(luoHakukohdeValintakoeData(hakukohdeOid3, Osallistuminen.EI_OSALLISTU, valintakoetunniste));

            ReflectionTestUtils.invokeMethod(valintakoelaskentaSuorittajaService, "asetaOsallistumisetKokeisiin", kokeet, hakuToiveetByOid);

            assertEquals(3, kokeet.size());
            assertEquals(hakukohdeOid1, kokeet.get(0).getHakukohdeOid());
            assertEquals(Osallistuminen.EI_OSALLISTU, kokeet.get(0).getOsallistuminenTulos().getOsallistuminen());

            assertEquals(hakukohdeOid2, kokeet.get(1).getHakukohdeOid());
            assertEquals(Osallistuminen.EI_OSALLISTU, kokeet.get(1).getOsallistuminenTulos().getOsallistuminen());

            assertEquals(hakukohdeOid3, kokeet.get(2).getHakukohdeOid());
            assertEquals(Osallistuminen.EI_OSALLISTU, kokeet.get(2).getOsallistuminenTulos().getOsallistuminen());
        }

        {
            List<HakukohdeValintakoeData> kokeet = new ArrayList<>();
            kokeet.add(luoHakukohdeValintakoeData(hakukohdeOid1, Osallistuminen.OSALLISTUU, valintakoetunniste));
            kokeet.add(luoHakukohdeValintakoeData(hakukohdeOid2, Osallistuminen.EI_OSALLISTU, valintakoetunniste));
            kokeet.add(luoHakukohdeValintakoeData(hakukohdeOid3, Osallistuminen.EI_OSALLISTU, valintakoetunniste));

            ReflectionTestUtils.invokeMethod(valintakoelaskentaSuorittajaService, "asetaOsallistumisetKokeisiin", kokeet, hakuToiveetByOid);

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
