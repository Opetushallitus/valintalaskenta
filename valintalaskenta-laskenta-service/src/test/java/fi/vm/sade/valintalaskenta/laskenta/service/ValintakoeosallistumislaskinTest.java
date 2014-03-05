package fi.vm.sade.valintalaskenta.laskenta.service;

import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.valintaperusteet.laskenta.Totuusarvofunktio;
import fi.vm.sade.service.valintaperusteet.laskenta.api.*;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.*;
import fi.vm.sade.service.valintaperusteet.model.Funktiokutsu;
import fi.vm.sade.service.valintaperusteet.dto.model.Funktionimi;
import fi.vm.sade.service.valintaperusteet.schema.FunktiokutsuTyyppi;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.OsallistuminenTulos;
import fi.vm.sade.valintalaskenta.laskenta.service.exception.LaskentaVaarantyyppisellaFunktiollaException;
import fi.vm.sade.valintalaskenta.laskenta.service.impl.LaskentadomainkonvertteriWrapper;
import fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion.FunktioKutsuTyyppiToFunktioKutsuConverter;
import fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion.HakemusTyyppiToHakemusConverter;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.Valintakoeosallistumislaskin;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.impl.ValintakoeosallistumislaskinImpl;
import org.junit.Before;
import org.junit.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.HashMap;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * User: wuoti Date: 6.5.2013 Time: 9.59
 */
public class ValintakoeosallistumislaskinTest {

    private Valintakoeosallistumislaskin valintakoeosallistumislaskin;

    private FunktioKutsuTyyppiToFunktioKutsuConverter funktiokutsuConverterMock;
    private LaskentaService laskentaServiceMock;
    private HakemusTyyppiToHakemusConverter hakemusConverterMock;
    private LaskentadomainkonvertteriWrapper laskentadomainkonvertteriWrapperMock;

    private Map<String, String> suomenkielinenMap(String teksti) {
        Map<String, String> vastaus = new HashMap<String, String>();
        vastaus.put("FI", teksti);
        return vastaus;
    }

    @Before
    public void setUpt() {
        valintakoeosallistumislaskin = new ValintakoeosallistumislaskinImpl();

        funktiokutsuConverterMock = mock(FunktioKutsuTyyppiToFunktioKutsuConverter.class);
        hakemusConverterMock = mock(HakemusTyyppiToHakemusConverter.class);
        laskentaServiceMock = mock(LaskentaService.class);
        laskentadomainkonvertteriWrapperMock = mock(LaskentadomainkonvertteriWrapper.class);

        ReflectionTestUtils.setField(valintakoeosallistumislaskin, "funktiokutsuConverter", funktiokutsuConverterMock);
        ReflectionTestUtils.setField(valintakoeosallistumislaskin, "hakemusConverter", hakemusConverterMock);
        ReflectionTestUtils.setField(valintakoeosallistumislaskin, "laskentaService", laskentaServiceMock);
        ReflectionTestUtils.setField(valintakoeosallistumislaskin, "laskentadomainkonvertteriWrapper",
                laskentadomainkonvertteriWrapperMock);
    }

    private void valmisteleStubit(final Hakukohde hakukohde, Tila tila, boolean tulos) {
        Funktiokutsu funktiokutsu = new Funktiokutsu();
        funktiokutsu.setFunktionimi(Funktionimi.TOTUUSARVO);

        when(funktiokutsuConverterMock.convert(any(FunktiokutsuTyyppi.class))).thenReturn(funktiokutsu);
        when(hakemusConverterMock.convert(any(HakemusTyyppi.class))).thenReturn(any(Hakemus.class));
        when(laskentadomainkonvertteriWrapperMock.muodostaTotuusarvolasku(funktiokutsu)).thenReturn(
                any(Totuusarvofunktio.class));

        final Laskentatulos<Boolean> tulos1 = new Laskentatulos<Boolean>(tila, tulos, new StringBuffer(), new HashMap<String, SyotettyArvo>(), new HashMap<String, FunktioTulos>());
        when(
                laskentaServiceMock.suoritaValintakoelaskenta(hakukohde,
                        any(Hakemus.class), any(Totuusarvofunktio.class))).thenReturn(tulos1);
    }

    @Test
    public void testTilaHyvaksyttavissaTrue() {
        final String hakukohdeOid = "hakukohdeOid1";
        final Hakukohde hakukohde = new Hakukohde(hakukohdeOid, new HashMap<String, String>());

        valmisteleStubit(hakukohde, new Hyvaksyttavissatila(), true);

        OsallistuminenTulos osallistuminen = valintakoeosallistumislaskin.laskeOsallistuminenYhdelleHakukohteelle(
                hakukohde, new HakemusTyyppi(), new FunktiokutsuTyyppi());
        assertEquals(Osallistuminen.OSALLISTUU, osallistuminen.getOsallistuminen());
    }

    @Test
    public void testTilaHyvaksyttavissaFalse() {
        final String hakukohdeOid = "hakukohdeOid1";
        final Hakukohde hakukohde = new Hakukohde(hakukohdeOid, new HashMap<String, String>());

        valmisteleStubit(hakukohde, new Hyvaksyttavissatila(), false);
        OsallistuminenTulos osallistuminen = valintakoeosallistumislaskin.laskeOsallistuminenYhdelleHakukohteelle(
                hakukohde, new HakemusTyyppi(), new FunktiokutsuTyyppi());
        assertEquals(Osallistuminen.EI_OSALLISTU, osallistuminen.getOsallistuminen());
    }

    @Test
    public void testTilaHylattyFalse() {
        final String hakukohdeOid = "hakukohdeOid1";
        final Hakukohde hakukohde = new Hakukohde(hakukohdeOid, new HashMap<String, String>());

        valmisteleStubit(hakukohde, new Hylattytila(suomenkielinenMap("kuvaus"), new PakollinenValintaperusteHylkays("")), false);
        OsallistuminenTulos osallistuminen = valintakoeosallistumislaskin.laskeOsallistuminenYhdelleHakukohteelle(
                hakukohde, new HakemusTyyppi(), new FunktiokutsuTyyppi());
        assertEquals(Osallistuminen.OSALLISTUU, osallistuminen.getOsallistuminen());
    }

    @Test
    public void testTilaHylattyTrue() {
        final String hakukohdeOid = "hakukohdeOid1";
        final Hakukohde hakukohde = new Hakukohde(hakukohdeOid, new HashMap<String, String>());

        valmisteleStubit(hakukohde, new Hylattytila(suomenkielinenMap("kuvaus"), new PakollinenValintaperusteHylkays("")), true);
        OsallistuminenTulos osallistuminen = valintakoeosallistumislaskin.laskeOsallistuminenYhdelleHakukohteelle(
                hakukohde, new HakemusTyyppi(), new FunktiokutsuTyyppi());
        assertEquals(Osallistuminen.OSALLISTUU, osallistuminen.getOsallistuminen());
    }

    @Test
    public void testTilaVirheTrue() {
        final String hakukohdeOid = "hakukohdeOid1";
        final Hakukohde hakukohde = new Hakukohde(hakukohdeOid, new HashMap<String, String>());

        valmisteleStubit(hakukohde, new Virhetila(suomenkielinenMap("kuvaus"), new ArvokonvertointiVirhe("")), true);
        OsallistuminenTulos osallistuminen = valintakoeosallistumislaskin.laskeOsallistuminenYhdelleHakukohteelle(
                hakukohde, new HakemusTyyppi(), new FunktiokutsuTyyppi());
        assertEquals(Osallistuminen.VIRHE, osallistuminen.getOsallistuminen());
    }

    @Test
    public void testTilaVirheFalse() {
        final String hakukohdeOid = "hakukohdeOid1";
        final Hakukohde hakukohde = new Hakukohde(hakukohdeOid, new HashMap<String, String>());

        valmisteleStubit(hakukohde, new Virhetila(suomenkielinenMap("kuvaus"), new ArvokonvertointiVirhe("")), false);
        OsallistuminenTulos osallistuminen = valintakoeosallistumislaskin.laskeOsallistuminenYhdelleHakukohteelle(
                hakukohde, new HakemusTyyppi(), new FunktiokutsuTyyppi());
        assertEquals(Osallistuminen.VIRHE, osallistuminen.getOsallistuminen());
    }

    @Test(expected = LaskentaVaarantyyppisellaFunktiollaException.class)
    public void testVaaranTyyppinenFunktio() {
        Funktiokutsu funktiokutsu = new Funktiokutsu();
        funktiokutsu.setFunktionimi(Funktionimi.LUKUARVO);

        when(funktiokutsuConverterMock.convert(any(FunktiokutsuTyyppi.class))).thenReturn(funktiokutsu);
        valintakoeosallistumislaskin.laskeOsallistuminenYhdelleHakukohteelle(new Hakukohde("hakukohdeOid", new HashMap<String, String>()), new HakemusTyyppi(),
                new FunktiokutsuTyyppi());
    }
}
