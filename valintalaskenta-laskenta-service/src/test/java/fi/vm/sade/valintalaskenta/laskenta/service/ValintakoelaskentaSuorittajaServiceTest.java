package fi.vm.sade.valintalaskenta.laskenta.service;

import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.hakemus.schema.HakukohdeTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.FunktiokutsuTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteetTyyppi;
import fi.vm.sade.valintalaskenta.dao.ValintakoeOsallistuminenDAO;
import fi.vm.sade.valintalaskenta.domain.valintakoe.*;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.ValintakoelaskentaSuorittajaService;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.Valintakoeosallistumislaskin;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.impl.ValintakoelaskentaSuorittajaServiceImpl;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.impl.util.HakukohdeValintakoeData;
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

    @Test
    public void testBasic() {

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

        final OsallistuminenTulos osallistuu1 = new OsallistuminenTulos();
        osallistuu1.setOsallistuminen(Osallistuminen.OSALLISTUU);

        final OsallistuminenTulos osallistuu2 = new OsallistuminenTulos();
        osallistuu2.setOsallistuminen(Osallistuminen.OSALLISTUU);

        when(valintakoeosallistumislaskinMock.laskeOsallistuminenYhdelleHakukohteelle(eq(hakukohdeOid1),
                Matchers.<HakemusTyyppi>any(), Matchers.<FunktiokutsuTyyppi>any())).thenReturn(osallistuu1);

        when(valintakoeosallistumislaskinMock.laskeOsallistuminenYhdelleHakukohteelle(eq(hakukohdeOid2),
                Matchers.<HakemusTyyppi>any(), Matchers.<FunktiokutsuTyyppi>any())).thenReturn(osallistuu2);

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
