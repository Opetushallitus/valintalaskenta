package fi.vm.sade.valintalaskenta.laskenta.service;

import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.hakemus.schema.HakukohdeTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.FunktiokutsuTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintakoeTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintakoeValinnanVaiheTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteetTyyppi;
import fi.vm.sade.valintalaskenta.dao.ValintakoeOsallistuminenDAO;
import fi.vm.sade.valintalaskenta.domain.valintakoe.*;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.ValintakoelaskentaSuorittajaService;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.Valintakoeosallistumislaskin;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.impl.ValintakoelaskentaSuorittajaServiceImpl;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Matchers;
import org.springframework.test.util.ReflectionTestUtils;
import scala.actors.threadpool.Arrays;

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

    private ValintakoeOsallistuminenDAO valintakoeOsallistuminenDAOMock;
    private Valintakoeosallistumislaskin valintakoeosallistumislaskinMock;

    @Before
    public void setUp() {
        valintakoelaskentaSuorittajaService = new ValintakoelaskentaSuorittajaServiceImpl();

        valintakoeOsallistuminenDAOMock = mock(ValintakoeOsallistuminenDAO.class);
        valintakoeosallistumislaskinMock = mock(Valintakoeosallistumislaskin.class);

        ReflectionTestUtils.setField(valintakoelaskentaSuorittajaService, "valintakoeOsallistuminenDAO",
                valintakoeOsallistuminenDAOMock);
        ReflectionTestUtils.setField(valintakoelaskentaSuorittajaService, "valintakoeosallistumislaskin",
                valintakoeosallistumislaskinMock);
    }

    private static HakemusTyyppi luoHakemus(String hakemusOid, String hakijaOid) {
        HakemusTyyppi hakemus = new HakemusTyyppi();
        hakemus.setHakemusOid(hakemusOid);
        hakemus.setHakijaOid(hakijaOid);

        return hakemus;
    }

    private static HakemusTyyppi luoHakemus(String hakemusOid, String hakijaOid, String... hakutoiveet) {
        HakemusTyyppi hakemus = luoHakemus(hakemusOid, hakijaOid);

        int i = 1;
        for (String hakutoive : hakutoiveet) {
            HakukohdeTyyppi toive = new HakukohdeTyyppi();
            toive.setHakukohdeOid(hakutoive);
            toive.setPrioriteetti(i);
            hakemus.getHakutoive().add(toive);
            ++i;
        }

        return hakemus;
    }

    private static HakemusTyyppi luoHakemus(String hakemusOid, String hakijaOid, HakukohdeTyyppi... hakutoiveet) {
        HakemusTyyppi hakemus = luoHakemus(hakemusOid, hakijaOid);
        hakemus.getHakutoive().addAll(Arrays.asList(hakutoiveet));

        return hakemus;
    }

    private static ValintaperusteetTyyppi luoValintaperusteet(String hakuOid, String hakukohdeOid,
                                                              String valinnanVaiheOid,
                                                              int valinnanVaiheJarjestysluku,
                                                              String... valintakoeTunnisteet) {
        ValintaperusteetTyyppi perusteet = new ValintaperusteetTyyppi();
        perusteet.setHakukohdeOid(hakukohdeOid);
        perusteet.setHakuOid(hakuOid);
        perusteet.setValinnanVaihe(luoValinnanVaihe(valinnanVaiheOid,
                valinnanVaiheJarjestysluku,
                valintakoeTunnisteet));

        return perusteet;
    }

    private static ValintakoeValinnanVaiheTyyppi luoValinnanVaihe(String valinnanVaiheOid, int jarjestysluku,
                                                                  String... valintakoeTunnisteet) {
        ValintakoeValinnanVaiheTyyppi vaihe = new ValintakoeValinnanVaiheTyyppi();
        vaihe.setValinnanVaiheOid(valinnanVaiheOid);
        vaihe.setValinnanVaiheJarjestysluku(jarjestysluku);

        for (String tunniste : valintakoeTunnisteet) {
            vaihe.getValintakoe().add(luoValintakoe(tunniste, tunniste));
        }

        return vaihe;
    }

    private static ValintakoeTyyppi luoValintakoe(String valintakoeOid, String tunniste) {
        ValintakoeTyyppi koe = new ValintakoeTyyppi();
        koe.setFunktiokutsu(new FunktiokutsuTyyppi());
        koe.setTunniste(tunniste);
        koe.setOid(valintakoeOid);
        return koe;
    }

    @Test
    public void testHakijaOsallistuuVainKorkeammanHakutoiveenValintakokeeseen() {

        final String hakukohdeOid1 = "hakukohdeOid1";
        final String hakukohdeOid2 = "hakukohdeOid2";

        final HakemusTyyppi hakemus = luoHakemus("hakemusOid", "hakijaOid", hakukohdeOid1, hakukohdeOid2);

        final String hakuOid = "hakuOid";
        final String valintakoetunniste = "valintakoetunniste";

        final String valinnanVaiheOid1 = "valinnanVaiheOid1";
        final int valinnanVaiheJarjestysluku1 = 1;

        ValintaperusteetTyyppi valintaperusteet1 = luoValintaperusteet(hakuOid, hakukohdeOid1, valinnanVaiheOid1,
                valinnanVaiheJarjestysluku1, valintakoetunniste);

        final String valinnanVaiheOid2 = "valinnanVaiheOid2";
        final int valinnanVaiheJarjestysluku2 = 2;


        ValintaperusteetTyyppi valintaperusteet2 = luoValintaperusteet(hakuOid, hakukohdeOid2, valinnanVaiheOid2,
                valinnanVaiheJarjestysluku2, valintakoetunniste);

        when(valintakoeosallistumislaskinMock.laskeOsallistuminenYhdelleHakukohteelle(eq(hakukohdeOid1),
                Matchers.<HakemusTyyppi>any(), Matchers.<FunktiokutsuTyyppi>any())).thenReturn(true);

        when(valintakoeosallistumislaskinMock.laskeOsallistuminenYhdelleHakukohteelle(eq(hakukohdeOid2),
                Matchers.<HakemusTyyppi>any(), Matchers.<FunktiokutsuTyyppi>any())).thenReturn(true);

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

            ValinnanVaihe vaihe1 = hakutoive1.getValinnanVaiheet().get(0);
            assertEquals(valinnanVaiheOid1, vaihe1.getValinnanVaiheOid());
            assertEquals(valinnanVaiheJarjestysluku1, vaihe1.getValinnanVaiheJarjestysluku().intValue());
            assertEquals(1, vaihe1.getValintakokeet().size());

            Valintakoe vk1 = vaihe1.getValintakokeet().get(0);
            assertEquals(valintakoetunniste, vk1.getValintakoeOid());
            assertEquals(valintakoetunniste, vk1.getValintakoeTunniste());
            assertEquals(Osallistuminen.OSALLISTUU, vk1.getOsallistuminen());
        }

        {
            Hakutoive hakutoive2 = hakutoiveet.get(1);
            assertEquals(hakukohdeOid2, hakutoive2.getHakukohdeOid());
            assertEquals(1, hakutoive2.getValinnanVaiheet().size());

            ValinnanVaihe vaihe2 = hakutoive2.getValinnanVaiheet().get(0);
            assertEquals(valinnanVaiheOid2, vaihe2.getValinnanVaiheOid());
            assertEquals(valinnanVaiheJarjestysluku2, vaihe2.getValinnanVaiheJarjestysluku().intValue());
            assertEquals(1, vaihe2.getValintakokeet().size());

            Valintakoe vk2 = vaihe2.getValintakokeet().get(0);
            assertEquals(valintakoetunniste, vk2.getValintakoeOid());
            assertEquals(valintakoetunniste, vk2.getValintakoeTunniste());
            assertEquals(Osallistuminen.EI_OSALLISTU, vk2.getOsallistuminen());
        }
    }

    @Test
    public void testHakijaOsallistuuVainKorkeimmanPakollisenHakutoiveenValintakokeeseen() {

    }

}
