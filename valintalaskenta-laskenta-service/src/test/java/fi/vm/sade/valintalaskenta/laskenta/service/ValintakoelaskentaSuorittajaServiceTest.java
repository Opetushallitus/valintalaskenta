package fi.vm.sade.valintalaskenta.laskenta.service;

import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.hakemus.schema.HakukohdeTyyppi;
import fi.vm.sade.service.valintaperusteet.laskenta.Totuusarvofunktio;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakemus;
import fi.vm.sade.service.valintaperusteet.laskenta.api.LaskentaService;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Laskentatulos;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Hyvaksyttavissatila;
import fi.vm.sade.service.valintaperusteet.model.Funktionimi;
import fi.vm.sade.service.valintaperusteet.schema.*;
import fi.vm.sade.valintalaskenta.dao.ValintakoeOsallistuminenDAO;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Hakutoive;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValinnanVaihe;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Valintakoe;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.laskenta.service.impl.ValintakoelaskentaSuorittajaServiceImpl;
import fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion.FunktioKutsuTyyppiToFunktioKutsuConverter;
import fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion.HakemusTyyppiToHakemusConverter;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Matchers;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.*;

/**
 * User: wuoti
 * Date: 3.5.2013
 * Time: 12.00
 */
public class ValintakoelaskentaSuorittajaServiceTest {

    private ValintakoelaskentaSuorittajaService valintakoelaskentaSuorittajaService;

    private FunktioKutsuTyyppiToFunktioKutsuConverter funktiokutsuConverter;
    private HakemusTyyppiToHakemusConverter hakemusConverter;
    private LaskentaService laskentaServiceMock;
    private ValintakoeOsallistuminenDAO valintakoeOsallistuminenDAOMock;


    private static FunktiokutsuTyyppi totuusarvoFalse;
    private static FunktiokutsuTyyppi totuusarvoTrue;

    static {
        totuusarvoFalse = new FunktiokutsuTyyppi();
        totuusarvoFalse.setFunktionimi(Funktionimi.TOTUUSARVO.name());
        SyoteparametriTyyppi param1 = new SyoteparametriTyyppi();
        param1.setAvain("totuusarvo");
        param1.setArvo(Boolean.FALSE.toString());
        totuusarvoFalse.getSyoteparametrit().add(param1);

        totuusarvoTrue = new FunktiokutsuTyyppi();
        totuusarvoTrue.setFunktionimi(Funktionimi.TOTUUSARVO.name());
        SyoteparametriTyyppi param2 = new SyoteparametriTyyppi();
        param2.setAvain("totuusarvo");
        param2.setArvo(Boolean.TRUE.toString());
        totuusarvoTrue.getSyoteparametrit().add(param2);
    }

    @Before
    public void setUp() {
        valintakoelaskentaSuorittajaService = new ValintakoelaskentaSuorittajaServiceImpl();

        funktiokutsuConverter = new FunktioKutsuTyyppiToFunktioKutsuConverter();
        hakemusConverter = new HakemusTyyppiToHakemusConverter();
        laskentaServiceMock = mock(LaskentaService.class);
        valintakoeOsallistuminenDAOMock = mock(ValintakoeOsallistuminenDAO.class);

        ReflectionTestUtils.setField(valintakoelaskentaSuorittajaService, "funktiokutsuConverter", funktiokutsuConverter);
        ReflectionTestUtils.setField(valintakoelaskentaSuorittajaService, "hakemusConverter", hakemusConverter);
        ReflectionTestUtils.setField(valintakoelaskentaSuorittajaService, "laskentaService", laskentaServiceMock);
        ReflectionTestUtils.setField(valintakoelaskentaSuorittajaService, "valintakoeOsallistuminenDAO", valintakoeOsallistuminenDAOMock);
    }


    @Test
    public void test() {
        final HakemusTyyppi hakemus = new HakemusTyyppi();
        hakemus.setHakemusOid("hakemusOid");
        hakemus.setHakijaOid("hakijaOid");

        HakukohdeTyyppi hakukohde1 = new HakukohdeTyyppi();
        hakukohde1.setHakukohdeOid("hakukohdeOid1");
        hakukohde1.setPrioriteetti(1);

        HakukohdeTyyppi hakukohde2 = new HakukohdeTyyppi();
        hakukohde2.setHakukohdeOid("hakukohdeOid2");
        hakukohde2.setPrioriteetti(2);

        hakemus.getHakutoive().add(hakukohde1);
        hakemus.getHakutoive().add(hakukohde2);

        final String hakuOid = "hakuOid";

        ValintaperusteetTyyppi valintaperusteet1 = new ValintaperusteetTyyppi();
        valintaperusteet1.setHakukohdeOid(hakukohde1.getHakukohdeOid());
        valintaperusteet1.setHakuOid(hakuOid);

        ValintakoeValinnanVaiheTyyppi valinnanVaihe1 = new ValintakoeValinnanVaiheTyyppi();
        valinnanVaihe1.setValinnanVaiheOid("valinnanVaiheOid1");
        valinnanVaihe1.setValinnanVaiheJarjestysluku(1);
        valintaperusteet1.setValinnanVaihe(valinnanVaihe1);

        final String valintakoeTunniste = "valintakoetunniste";

        ValintakoeTyyppi koe1 = new ValintakoeTyyppi();
        koe1.setTunniste(valintakoeTunniste);
        koe1.setOid("valintakoeOid1");
        koe1.setFunktiokutsu(totuusarvoTrue);
        valinnanVaihe1.getValintakoe().add(koe1);

        ValintaperusteetTyyppi valintaperusteet2 = new ValintaperusteetTyyppi();
        valintaperusteet2.setHakukohdeOid(hakukohde2.getHakukohdeOid());
        valintaperusteet2.setHakuOid(hakuOid);

        ValintakoeValinnanVaiheTyyppi valinnanVaihe2 = new ValintakoeValinnanVaiheTyyppi();
        valinnanVaihe2.setValinnanVaiheOid("valinnanVaiheOid2");
        valinnanVaihe2.setValinnanVaiheJarjestysluku(2);
        valintaperusteet2.setValinnanVaihe(valinnanVaihe2);

        ValintakoeTyyppi koe2 = new ValintakoeTyyppi();
        koe2.setTunniste(valintakoeTunniste);
        koe2.setOid("valintakoeOid2");
        koe2.setFunktiokutsu(totuusarvoFalse);
        valinnanVaihe2.getValintakoe().add(koe2);

        final Laskentatulos<Boolean> tulos1 = new Laskentatulos<Boolean>(new Hyvaksyttavissatila(), true);
        when(laskentaServiceMock.suoritaLasku(eq(hakukohde1.getHakukohdeOid()), Matchers.<Hakemus>any(), Matchers.<Totuusarvofunktio>any())).thenReturn(tulos1);

        final Laskentatulos<Boolean> tulos2 = new Laskentatulos<Boolean>(new Hyvaksyttavissatila(), false);
        when(laskentaServiceMock.suoritaLasku(eq(hakukohde2.getHakukohdeOid()), Matchers.<Hakemus>any(), Matchers.<Totuusarvofunktio>any())).thenReturn(tulos2);

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
            assertEquals(hakutoive1.getHakukohdeOid(), hakukohde1.getHakukohdeOid());
            assertEquals(1, hakutoive1.getValinnanVaiheet().size());

            ValinnanVaihe vaihe1 = hakutoive1.getValinnanVaiheet().get(0);
            assertEquals(valinnanVaihe1.getValinnanVaiheOid(), vaihe1.getValinnanVaiheOid());
            assertEquals(valinnanVaihe1.getValinnanVaiheJarjestysluku(), vaihe1.getValinnanVaiheJarjestysluku().intValue());
            assertEquals(1, vaihe1.getValintakokeet().size());

            Valintakoe vk1 = vaihe1.getValintakokeet().get(0);
            assertEquals(koe1.getOid(), vk1.getValintakoeOid());
            assertEquals(koe1.getTunniste(), vk1.getValintakoeTunniste());
        }

        {
            Hakutoive hakutoive2 = hakutoiveet.get(1);
            assertEquals(hakutoive2.getHakukohdeOid(), hakukohde2.getHakukohdeOid());
            assertEquals(1, hakutoive2.getValinnanVaiheet().size());

            ValinnanVaihe vaihe2 = hakutoive2.getValinnanVaiheet().get(0);
            assertEquals(valinnanVaihe2.getValinnanVaiheOid(), vaihe2.getValinnanVaiheOid());
            assertEquals(valinnanVaihe2.getValinnanVaiheJarjestysluku(), vaihe2.getValinnanVaiheJarjestysluku().intValue());
            assertEquals(1, vaihe2.getValintakokeet().size());

            Valintakoe vk2 = vaihe2.getValintakokeet().get(0);
            assertEquals(koe2.getOid(), vk2.getValintakoeOid());
            assertEquals(koe2.getTunniste(), vk2.getValintakoeTunniste());
        }
    }


}
