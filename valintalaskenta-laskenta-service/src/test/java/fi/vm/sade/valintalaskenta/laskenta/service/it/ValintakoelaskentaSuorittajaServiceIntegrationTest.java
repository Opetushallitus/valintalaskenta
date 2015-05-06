package fi.vm.sade.valintalaskenta.laskenta.service.it;

import com.google.common.reflect.TypeToken;
import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;
import com.lordofthejars.nosqlunit.annotation.UsingDataSet;
import com.lordofthejars.nosqlunit.core.LoadStrategyEnum;
import com.lordofthejars.nosqlunit.mongodb.MongoDbRule;
import fi.vm.sade.service.valintaperusteet.dto.FunktiokutsuDTO;
import fi.vm.sade.service.valintaperusteet.dto.SyoteparametriDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.model.Funktionimi;
import fi.vm.sade.service.valintaperusteet.dto.model.Koekutsu;
import fi.vm.sade.valintalaskenta.domain.dto.AvainArvoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.domain.dto.LaskeDTO;
import fi.vm.sade.valintalaskenta.domain.valintakoe.*;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValintakoeOsallistuminenDAO;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.EdellinenValinnanvaiheKasittelija;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.ValintakoelaskentaSuorittajaService;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.impl.ValintakoelaskentaSuorittajaServiceImpl;
import fi.vm.sade.valintalaskenta.laskenta.testdata.TestDataUtil;
import org.apache.commons.io.IOUtils;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.core.io.ClassPathResource;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestExecutionListeners;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.support.DependencyInjectionTestExecutionListener;
import org.springframework.test.context.support.DirtiesContextTestExecutionListener;

import java.io.IOException;
import java.util.*;
import java.util.stream.Collectors;

import static com.lordofthejars.nosqlunit.mongodb.MongoDbRule.MongoDbRuleBuilder.newMongoDbRule;
import static fi.vm.sade.valintalaskenta.laskenta.testdata.TestDataUtil.luoHakemus;
import static fi.vm.sade.valintalaskenta.laskenta.testdata.TestDataUtil.luoValintaperusteetJaValintakoeValinnanvaihe;
import static org.junit.Assert.*;

/**
 * User: wuoti
 * Date: 6.5.2013
 * Time: 12.54
 */
@ContextConfiguration(locations = "classpath:application-context-test.xml")
@RunWith(SpringJUnit4ClassRunner.class)
@TestExecutionListeners(listeners = {DependencyInjectionTestExecutionListener.class,
        DirtiesContextTestExecutionListener.class})
public class ValintakoelaskentaSuorittajaServiceIntegrationTest {

    @Rule
    public MongoDbRule mongoDbRule = newMongoDbRule().defaultSpringMongoDb("test");

    @Autowired
    private ApplicationContext applicationContext;

    @Autowired
    private ValintakoelaskentaSuorittajaService valintakoelaskentaSuorittajaService;

    @Autowired
    private ValintakoeOsallistuminenDAO valintakoeOsallistuminenDAO;

    @Autowired
    private EdellinenValinnanvaiheKasittelija edellinenValinnanvaiheKasittelija;

    private static final FunktiokutsuDTO totuusarvoTrue;
    private static final FunktiokutsuDTO totuusarvoFalse;


    static {
        totuusarvoTrue = new FunktiokutsuDTO();
        totuusarvoTrue.setFunktionimi(Funktionimi.TOTUUSARVO);

        {
            SyoteparametriDTO param = new SyoteparametriDTO();
            param.setAvain("totuusarvo");
            param.setArvo(Boolean.TRUE.toString());
            totuusarvoTrue.getSyoteparametrit().add(param);
        }

        totuusarvoFalse = new FunktiokutsuDTO();
        totuusarvoFalse.setFunktionimi(Funktionimi.TOTUUSARVO);
        {
            SyoteparametriDTO param = new SyoteparametriDTO();
            param.setAvain("totuusarvo");
            param.setArvo(Boolean.FALSE.toString());
            totuusarvoFalse.getSyoteparametrit().add(param);
        }
    }

    @Test
    @UsingDataSet(loadStrategy = LoadStrategyEnum.DELETE_ALL)
    public void test() {
        final String hakukohdeOid1 = "hakukohdeOid1";
        final String hakukohdeOid2 = "hakukohdeOid2";
        final String hakemusOid = "hakemusOid";

        final HakemusDTO hakemus = luoHakemus(hakemusOid, "hakijaOid", hakukohdeOid1, hakukohdeOid2);

        final String hakuOid = "hakuOid";
        final String valintakoetunniste = "valintakoetunniste";

        final String valinnanVaiheOid1 = "valinnanVaiheOid1";
        final int valinnanVaiheJarjestysluku1 = 0;

        Map<String, FunktiokutsuDTO> kokeet1 = new HashMap<String, FunktiokutsuDTO>();
        kokeet1.put(valintakoetunniste, totuusarvoTrue);
        kokeet1.put("valintakoetunniste2", totuusarvoTrue);

        ValintaperusteetDTO valintaperusteet1 = TestDataUtil.luoValintaperusteetJaValintakoeValinnanVaihe(hakuOid, hakukohdeOid1, valinnanVaiheOid1,
                valinnanVaiheJarjestysluku1, kokeet1, Koekutsu.YLIN_TOIVE, "kutsunKohdeAvain");

        final String valinnanVaiheOid2 = "valinnanVaiheOid2";
        final int valinnanVaiheJarjestysluku2 = 1;

        Map<String, FunktiokutsuDTO> kokeet2 = new HashMap<String, FunktiokutsuDTO>();
        kokeet2.put(valintakoetunniste, totuusarvoFalse);

        ValintaperusteetDTO valintaperusteet2 = TestDataUtil.luoValintaperusteetJaValintakoeValinnanVaihe(hakuOid, hakukohdeOid2, valinnanVaiheOid2,
                valinnanVaiheJarjestysluku2, kokeet2, Koekutsu.YLIN_TOIVE, "kutsunKohdeAvain");

        List<ValintaperusteetDTO> valintaperusteet = new ArrayList<ValintaperusteetDTO>();
        valintaperusteet.add(valintaperusteet1);
        valintaperusteet.add(valintaperusteet2);

        assertNull(valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid));
        valintakoelaskentaSuorittajaService.laske(hakemus, valintaperusteet);
        ValintakoeOsallistuminen osallistuminen = valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid);
        assertNotNull(osallistuminen);
        assertEquals(osallistuminen.getHakutoiveet().get(0).getValinnanVaiheet().get(0).getValintakokeet().get(0).getOsallistuminenTulos().getOsallistuminen(), Osallistuminen.OSALLISTUU);
        assertEquals(osallistuminen.getHakutoiveet().get(0).getValinnanVaiheet().get(0).getValintakokeet().get(1).getOsallistuminenTulos().getOsallistuminen(), Osallistuminen.OSALLISTUU);
    }

    @Test
    @UsingDataSet(loadStrategy = LoadStrategyEnum.DELETE_ALL)
    public void testKoekutsuHakijanValinta() {
        final String hakukohdeOid1 = "hakukohdeOid1";
        final String hakukohdeOid2 = "hakukohdeOid2";
        final String hakemusOid = "hakemusOid";

        final HakemusDTO hakemus = luoHakemus(hakemusOid, "hakijaOid", hakukohdeOid1, hakukohdeOid2);

        final String hakuOid = "hakuOid";
        final String valintakoetunniste = "valintakoetunniste";

        final String valinnanVaiheOid1 = "valinnanVaiheOid1";
        final int valinnanVaiheJarjestysluku1 = 0;

        Map<String, FunktiokutsuDTO> kokeet1 = new HashMap<String, FunktiokutsuDTO>();
        kokeet1.put(valintakoetunniste, totuusarvoTrue);

        ValintaperusteetDTO valintaperusteet1 = TestDataUtil.luoValintaperusteetJaValintakoeValinnanVaihe(hakuOid, hakukohdeOid1, valinnanVaiheOid1,
                valinnanVaiheJarjestysluku1, kokeet1, Koekutsu.HAKIJAN_VALINTA, "hakukohdeKutsunKohde2");

        List<ValintaperusteetDTO> valintaperusteet = new ArrayList<ValintaperusteetDTO>();
        valintaperusteet.add(valintaperusteet1);

        assertNull(valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid));
        valintakoelaskentaSuorittajaService.laske(hakemus, valintaperusteet);
        ValintakoeOsallistuminen osallistuminen = valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid);
        assertNotNull(osallistuminen);
        assertEquals(osallistuminen.getHakutoiveet().get(0).getValinnanVaiheet().get(0).getValintakokeet().get(0).getOsallistuminenTulos().getOsallistuminen(), Osallistuminen.OSALLISTUU);
        assertEquals(osallistuminen.getHakutoiveet().get(0).getValinnanVaiheet().get(0).getValinnanVaiheOid(), ValintakoelaskentaSuorittajaServiceImpl.VALINNANVAIHE_HAKIJAN_VALINTA);
        assertEquals(osallistuminen.getHakutoiveet().get(0).getValinnanVaiheet().get(0).getValinnanVaiheJarjestysluku(), new Integer(100));
    }

    @Test
    @UsingDataSet(loadStrategy = LoadStrategyEnum.DELETE_ALL)
    public void test1() {
        final String hakukohdeOid1 = "hakukohdeOid1";
        final String hakukohdeOid2 = "hakukohdeOid2";
        final String hakukohdeOid3 = "hakukohdeOid3";

        final String hakuOid = "hakuOid";
        final String valintakoetunniste1 = "valintakoetunniste1";
        final String valintakoetunniste2 = "valintakoetunniste2";
        final String valintakoetunniste3 = "valintakoetunniste3";

        final String valinnanVaiheOid1 = "valinnanVaiheOid1";
        final int valinnanVaiheJarjestysluku1 = 0;

        Map<String, FunktiokutsuDTO> kokeet1 = new HashMap<>();
        kokeet1.put(valintakoetunniste1, totuusarvoTrue);

        ValintaperusteetDTO valintaperusteet1 = TestDataUtil.luoValintaperusteetJaValintakoeValinnanVaihe(hakuOid, hakukohdeOid1, valinnanVaiheOid1,
                valinnanVaiheJarjestysluku1, kokeet1, Koekutsu.HAKIJAN_VALINTA, "hakukohdeKutsunKohde2");

        final String valinnanVaiheOid2 = "valinnanVaiheOid2";
        final int valinnanVaiheJarjestysluku2 = 0;

        Map<String, FunktiokutsuDTO> kokeet2 = new TreeMap<>();
        kokeet2.put(valintakoetunniste1, totuusarvoTrue);
        kokeet2.put(valintakoetunniste2, totuusarvoTrue);

        ValintaperusteetDTO valintaperusteet2 = TestDataUtil.luoValintaperusteetJaValintakoeValinnanVaihe(hakuOid, hakukohdeOid2, valinnanVaiheOid2,
                valinnanVaiheJarjestysluku2, kokeet2, Koekutsu.HAKIJAN_VALINTA, "hakukohdeKutsunKohde2");
        valintaperusteet2.getValinnanVaihe().getValintakoe().get(1).setKutsunKohde(Koekutsu.YLIN_TOIVE);

        final String valinnanVaiheOid3 = "valinnanVaiheOid3";
        final int valinnanVaiheJarjestysluku3 = 0;

        Map<String, FunktiokutsuDTO> kokeet3 = new TreeMap<>();
        kokeet3.put(valintakoetunniste2, totuusarvoTrue);
        kokeet3.put(valintakoetunniste3, totuusarvoTrue);

        ValintaperusteetDTO valintaperusteet3 = TestDataUtil.luoValintaperusteetJaValintakoeValinnanVaihe(hakuOid, hakukohdeOid3, valinnanVaiheOid3,
                valinnanVaiheJarjestysluku3, kokeet3, Koekutsu.YLIN_TOIVE, "hakukohdeKutsunKohde2");

        testWithOrder("hakemusOid1", hakukohdeOid1, hakukohdeOid2, hakukohdeOid3, hakuOid, valintaperusteet1, valintaperusteet2, valintaperusteet3);
        testWithOrder("hakemusOid2", hakukohdeOid1, hakukohdeOid2, hakukohdeOid3, hakuOid, valintaperusteet1, valintaperusteet3, valintaperusteet2);
        testWithOrder("hakemusOid3", hakukohdeOid1, hakukohdeOid2, hakukohdeOid3, hakuOid, valintaperusteet2, valintaperusteet1, valintaperusteet3);
        testWithOrder("hakemusOid4", hakukohdeOid1, hakukohdeOid2, hakukohdeOid3, hakuOid, valintaperusteet2, valintaperusteet3, valintaperusteet1);
        testWithOrder("hakemusOid5", hakukohdeOid1, hakukohdeOid2, hakukohdeOid3, hakuOid, valintaperusteet3, valintaperusteet1, valintaperusteet2);
        testWithOrder("hakemusOid6", hakukohdeOid1, hakukohdeOid2, hakukohdeOid3, hakuOid, valintaperusteet3, valintaperusteet2, valintaperusteet1);
    }

    private void testWithOrder(String hakemusOid, String hakukohdeOid1, String hakukohdeOid2, String hakukohdeOid3, String hakuOid, ValintaperusteetDTO vp1, ValintaperusteetDTO vp2, ValintaperusteetDTO vp3) {
        final HakemusDTO hakemus = luoHakemus(hakemusOid, "hakijaOid", hakukohdeOid1, hakukohdeOid2, hakukohdeOid3);
        assertNull(valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid));
        valintakoelaskentaSuorittajaService.laske(hakemus, Arrays.asList(vp1));
        valintakoelaskentaSuorittajaService.laske(hakemus, Arrays.asList(vp2));
        valintakoelaskentaSuorittajaService.laske(hakemus, Arrays.asList(vp3));
        assertTest1Results(valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid));
    }

    private void assertTest1Results(ValintakoeOsallistuminen osallistuminen) {
        assertNotNull(osallistuminen);

        assertEquals(2, osallistuminen.getHakutoiveet().size());

        Optional<Hakutoive> hakutoive1 = osallistuminen.getHakutoiveet().stream().filter(h -> h.getHakukohdeOid().equals("hakukohdeOid2")).findFirst();
        assertTrue(hakutoive1.isPresent());

        assertEquals(2, hakutoive1.get().getValinnanVaiheet().size());
        assertEquals(1, hakutoive1.get().getValinnanVaiheet().get(0).getValintakokeet().size());
        assertEquals(1, hakutoive1.get().getValinnanVaiheet().get(1).getValintakokeet().size());

        final Optional<Valintakoe> valintakoetunniste1 = hakutoive1.get().getValinnanVaiheet().stream().flatMap(v -> v.getValintakokeet().stream()).filter(k -> k.getValintakoeTunniste().equals("valintakoetunniste1")).findFirst();
        assertTrue(valintakoetunniste1.isPresent());
        assertEquals(Osallistuminen.OSALLISTUU, valintakoetunniste1.get().getOsallistuminenTulos().getOsallistuminen());

        final Optional<Valintakoe> valintakoetunniste2 = hakutoive1.get().getValinnanVaiheet().stream().flatMap(v -> v.getValintakokeet().stream()).filter(k -> k.getValintakoeTunniste().equals("valintakoetunniste2")).findFirst();
        assertTrue(valintakoetunniste2.isPresent());
        assertEquals(Osallistuminen.OSALLISTUU, valintakoetunniste2.get().getOsallistuminenTulos().getOsallistuminen());

        Optional<Hakutoive> hakutoive2 = osallistuminen.getHakutoiveet().stream().filter(h -> h.getHakukohdeOid().equals("hakukohdeOid3")).findFirst();
        assertTrue(hakutoive2.isPresent());

        assertEquals(1, hakutoive2.get().getValinnanVaiheet().size());
        assertEquals(2, hakutoive2.get().getValinnanVaiheet().get(0).getValintakokeet().size());

        final Optional<Valintakoe> valintakoetunniste3 = hakutoive2.get().getValinnanVaiheet().stream().flatMap(v -> v.getValintakokeet().stream()).filter(k -> k.getValintakoeTunniste().equals("valintakoetunniste3")).findFirst();
        assertTrue(valintakoetunniste3.isPresent());
        assertEquals(Osallistuminen.OSALLISTUU, valintakoetunniste3.get().getOsallistuminenTulos().getOsallistuminen());

        final Optional<Valintakoe> valintakoetunniste2b = hakutoive2.get().getValinnanVaiheet().stream().flatMap(v -> v.getValintakokeet().stream()).filter(k -> k.getValintakoeTunniste().equals("valintakoetunniste2")).findFirst();
        assertTrue(valintakoetunniste2b.isPresent());
        assertEquals(Osallistuminen.EI_OSALLISTU, valintakoetunniste2b.get().getOsallistuminenTulos().getOsallistuminen());
    }

    @Test
    @UsingDataSet(loadStrategy = LoadStrategyEnum.DELETE_ALL)
    public void test2() {
        final String hakukohdeOid1 = "hakukohdeOid1";
        final String hakukohdeOid2 = "hakukohdeOid2";
        final String hakukohdeOid3 = "hakukohdeOid3";

        final String hakuOid = "hakuOid";
        final String valintakoetunniste1 = "valintakoetunniste1";
        final String valintakoetunniste2 = "valintakoetunniste2";
        final String valintakoetunniste3 = "valintakoetunniste3";

        final String valinnanVaiheOid1 = "valinnanVaiheOid1";
        final int valinnanVaiheJarjestysluku1 = 0;

        Map<String, FunktiokutsuDTO> kokeet1 = new HashMap<>();
        kokeet1.put(valintakoetunniste1, totuusarvoTrue);
        kokeet1.put(valintakoetunniste2, totuusarvoTrue);

        ValintaperusteetDTO valintaperusteet1 = TestDataUtil.luoValintaperusteetJaValintakoeValinnanVaihe(hakuOid, hakukohdeOid1, valinnanVaiheOid1,
                valinnanVaiheJarjestysluku1, kokeet1, Koekutsu.HAKIJAN_VALINTA, "hakukohdeKutsunKohde1");
        valintaperusteet1.getValinnanVaihe().getValintakoe().stream().filter(koe -> koe.getTunniste().equals(valintakoetunniste2)).findFirst().get().setKutsunKohdeAvain("hakukohdeKutsunKohde2");

        final String valinnanVaiheOid2 = "valinnanVaiheOid2";
        final int valinnanVaiheJarjestysluku2 = 0;

        Map<String, FunktiokutsuDTO> kokeet2 = new TreeMap<>();
        kokeet2.put(valintakoetunniste2, totuusarvoTrue);
        kokeet2.put(valintakoetunniste3, totuusarvoTrue);

        ValintaperusteetDTO valintaperusteet2 = TestDataUtil.luoValintaperusteetJaValintakoeValinnanVaihe(hakuOid, hakukohdeOid2, valinnanVaiheOid2,
                valinnanVaiheJarjestysluku2, kokeet2, Koekutsu.HAKIJAN_VALINTA, "hakukohdeKutsunKohde2");
        valintaperusteet2.getValinnanVaihe().getValintakoe().stream().filter(koe -> koe.getTunniste().equals(valintakoetunniste3)).findFirst().get().setKutsunKohde(Koekutsu.YLIN_TOIVE);

        final String valinnanVaiheOid3 = "valinnanVaiheOid3";
        final int valinnanVaiheJarjestysluku3 = 0;

        Map<String, FunktiokutsuDTO> kokeet3 = new TreeMap<>();
        kokeet3.put(valintakoetunniste1, totuusarvoTrue);
        kokeet3.put(valintakoetunniste2, totuusarvoTrue);

        ValintaperusteetDTO valintaperusteet3 = TestDataUtil.luoValintaperusteetJaValintakoeValinnanVaihe(hakuOid, hakukohdeOid3, valinnanVaiheOid3,
                valinnanVaiheJarjestysluku3, kokeet3, Koekutsu.HAKIJAN_VALINTA, "hakukohdeKutsunKohde1");
        valintaperusteet3.getValinnanVaihe().getValintakoe().stream().filter(koe -> koe.getTunniste().equals(valintakoetunniste2)).findFirst().get().setKutsunKohdeAvain("hakukohdeKutsunKohde2");

        testWithOrder2("hakemusOid1", hakukohdeOid1, hakukohdeOid2, hakukohdeOid3, hakuOid, valintaperusteet1, valintaperusteet2, valintaperusteet3);
        testWithOrder2("hakemusOid2", hakukohdeOid1, hakukohdeOid2, hakukohdeOid3, hakuOid, valintaperusteet1, valintaperusteet3, valintaperusteet2);
        testWithOrder2("hakemusOid3", hakukohdeOid1, hakukohdeOid2, hakukohdeOid3, hakuOid, valintaperusteet2, valintaperusteet1, valintaperusteet3);
        testWithOrder2("hakemusOid4", hakukohdeOid1, hakukohdeOid2, hakukohdeOid3, hakuOid, valintaperusteet2, valintaperusteet3, valintaperusteet1);
        testWithOrder2("hakemusOid5", hakukohdeOid1, hakukohdeOid2, hakukohdeOid3, hakuOid, valintaperusteet3, valintaperusteet1, valintaperusteet2);
        testWithOrder2("hakemusOid6", hakukohdeOid1, hakukohdeOid2, hakukohdeOid3, hakuOid, valintaperusteet3, valintaperusteet2, valintaperusteet1);
    }

    private void testWithOrder2(String hakemusOid, String hakukohdeOid1, String hakukohdeOid2, String hakukohdeOid3, String hakuOid, ValintaperusteetDTO vp1, ValintaperusteetDTO vp2, ValintaperusteetDTO vp3) {
        final HakemusDTO hakemus = luoHakemus(hakemusOid, "hakijaOid", hakukohdeOid1, hakukohdeOid2, hakukohdeOid3);
        AvainArvoDTO avain = new AvainArvoDTO();
        avain.setAvain("hakukohdeKutsunKohde1");
        avain.setArvo("hakukohdeOid2");
        AvainArvoDTO avain2 = new AvainArvoDTO();
        avain2.setAvain("hakukohdeKutsunKohde2");
        avain2.setArvo("hakukohdeOid3");
        hakemus.setAvaimet(Arrays.asList(avain, avain2));

        assertNull(valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid));
        valintakoelaskentaSuorittajaService.laske(hakemus, Arrays.asList(vp1));
        valintakoelaskentaSuorittajaService.laske(hakemus, Arrays.asList(vp2));
        valintakoelaskentaSuorittajaService.laske(hakemus, Arrays.asList(vp3));
        assertTest2Results(valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid));
    }

    private void assertTest2Results(ValintakoeOsallistuminen osallistuminen) {
        assertNotNull(osallistuminen);

        assertEquals(2, osallistuminen.getHakutoiveet().size());

        Optional<Hakutoive> hakutoive1 = osallistuminen.getHakutoiveet().stream().filter(h -> h.getHakukohdeOid().equals("hakukohdeOid2")).findFirst();
        assertTrue(hakutoive1.isPresent());

        assertEquals(2, hakutoive1.get().getValinnanVaiheet().size());
        assertEquals(1, hakutoive1.get().getValinnanVaiheet().get(0).getValintakokeet().size());
        assertEquals(1, hakutoive1.get().getValinnanVaiheet().get(1).getValintakokeet().size());

        final Optional<Valintakoe> valintakoetunniste1 = hakutoive1.get().getValinnanVaiheet().stream().flatMap(v -> v.getValintakokeet().stream()).filter(k -> k.getValintakoeTunniste().equals("valintakoetunniste1")).findFirst();
        assertTrue(valintakoetunniste1.isPresent());
        assertEquals(Osallistuminen.OSALLISTUU, valintakoetunniste1.get().getOsallistuminenTulos().getOsallistuminen());

        final Optional<Valintakoe> valintakoetunniste3 = hakutoive1.get().getValinnanVaiheet().stream().flatMap(v -> v.getValintakokeet().stream()).filter(k -> k.getValintakoeTunniste().equals("valintakoetunniste3")).findFirst();
        assertTrue(valintakoetunniste3.isPresent());
        assertEquals(Osallistuminen.OSALLISTUU, valintakoetunniste3.get().getOsallistuminenTulos().getOsallistuminen());

        Optional<Hakutoive> hakutoive2 = osallistuminen.getHakutoiveet().stream().filter(h -> h.getHakukohdeOid().equals("hakukohdeOid3")).findFirst();
        assertTrue(hakutoive2.isPresent());

        assertEquals(1, hakutoive2.get().getValinnanVaiheet().size());
        assertEquals(1, hakutoive2.get().getValinnanVaiheet().get(0).getValintakokeet().size());

        final Optional<Valintakoe> valintakoetunniste2 = hakutoive2.get().getValinnanVaiheet().stream().flatMap(v -> v.getValintakokeet().stream()).filter(k -> k.getValintakoeTunniste().equals("valintakoetunniste2")).findFirst();
        assertTrue(valintakoetunniste2.isPresent());
        assertEquals(Osallistuminen.OSALLISTUU, valintakoetunniste2.get().getOsallistuminenTulos().getOsallistuminen());
    }

    @Test
    @UsingDataSet(locations = "testViimeisinValinnanVaihe.json", loadStrategy = LoadStrategyEnum.CLEAN_INSERT)
    public void testViimeisinValinnanVaihe() {
        final String hakemusOid = "1.2.246.562.11.00000072753";
        final String hakukohdeOid = "1.2.246.562.5.91937845484";
        final String hakuOid = "1.2.246.562.5.2013080813081926341927";
        final String valinnanVaiheOid = "vv2";
        final String valintakoetunniste = "koe1";

        ValintaperusteetDTO vv2 = luoValintaperusteetJaValintakoeValinnanvaihe(hakuOid, hakukohdeOid, valinnanVaiheOid, 1, valintakoetunniste);
        valintakoelaskentaSuorittajaService.laske(luoHakemus(hakemusOid, hakemusOid, hakukohdeOid), Arrays.asList(vv2));

        ValintakoeOsallistuminen osallistuminen = valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid);
        assertNotNull(osallistuminen);
        assertEquals(hakemusOid, osallistuminen.getHakemusOid());
        assertEquals(1, osallistuminen.getHakutoiveet().size());

        Hakutoive hakutoive = osallistuminen.getHakutoiveet().get(0);
        assertEquals(hakukohdeOid, hakutoive.getHakukohdeOid());

        assertEquals(1, hakutoive.getValinnanVaiheet().size());
        ValintakoeValinnanvaihe vv = hakutoive.getValinnanVaiheet().get(0);
        assertEquals(valinnanVaiheOid, vv.getValinnanVaiheOid());
        assertEquals(1, vv.getValintakokeet().size());

        Valintakoe koe = vv.getValintakokeet().get(0);
        assertEquals(valintakoetunniste, koe.getValintakoeTunniste());
        assertEquals(Osallistuminen.EI_OSALLISTU, koe.getOsallistuminenTulos().getOsallistuminen());
    }


    @Test
    @UsingDataSet(locations = "osallistuminen.json", loadStrategy = LoadStrategyEnum.CLEAN_INSERT)
    public void testOlemassaolevatKokoeet() throws JsonSyntaxException,
            IOException {


        LaskeDTO dto = new Gson().fromJson(IOUtils
                .toString(new ClassPathResource("laskeDTO.json")
                        .getInputStream()), new TypeToken<LaskeDTO>() {
        }.getType());

        valintakoelaskentaSuorittajaService.laske(dto.getHakemus().get(0), dto.getValintaperuste());

        ValintakoeOsallistuminen osallistuminen = valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid("1.2.246.562.5.2013080813081926341927", "1.2.246.562.11.00000304421");

        assertTrue(osallistuminen.getHakutoiveet().size() == 2);

        osallistuminen.getHakutoiveet().sort((h1, h2) -> h1.getHakukohdeOid().compareTo(h2.getHakukohdeOid()));

        assertTrue(osallistuminen.getHakutoiveet().get(0).getValinnanVaiheet().get(0).getValintakokeet().size() == 3);
        assertTrue(osallistuminen.getHakutoiveet().get(1).getValinnanVaiheet().get(0).getValintakokeet().size() == 3);

        osallistuminen.getHakutoiveet().get(0).getValinnanVaiheet().get(0).getValintakokeet().sort((k1, k2) -> k1.getValintakoeTunniste().compareTo(k2.getValintakoeTunniste()));
        osallistuminen.getHakutoiveet().get(1).getValinnanVaiheet().get(0).getValintakokeet().sort((k1, k2) -> k1.getValintakoeTunniste().compareTo(k2.getValintakoeTunniste()));

        assertEquals(Osallistuminen.OSALLISTUU, osallistuminen.getHakutoiveet().get(1).getValinnanVaiheet().get(0).getValintakokeet().get(1).getOsallistuminenTulos().getOsallistuminen());
        assertEquals(Osallistuminen.EI_OSALLISTU, osallistuminen.getHakutoiveet().get(0).getValinnanVaiheet().get(0).getValintakokeet().get(1).getOsallistuminenTulos().getOsallistuminen());

    }

    @Test
    @UsingDataSet(locations = "voidaanHyvaksya.json", loadStrategy = LoadStrategyEnum.CLEAN_INSERT)
    public void testMukanaKokeessaToisessaKohteessa() {
        final String hakemusOid = "1.2.246.562.11.00001212279";
        final String hakukohdeOid = "1.2.246.562.20.66128426039";
        final String hakuOid = "1.2.246.562.29.173465377510";
        final String hakemusOid2 = "1.2.246.562.11.00001223556";

        ValintakoeOsallistuminen osallistuminen = valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid);
        assertNotNull(osallistuminen);
        boolean voidaanHyvaksya = edellinenValinnanvaiheKasittelija.koeOsallistuminenToisessaKohteessa(hakukohdeOid,osallistuminen);
        assertTrue(voidaanHyvaksya);

        osallistuminen = valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid2);
        assertNotNull(osallistuminen);
        voidaanHyvaksya = edellinenValinnanvaiheKasittelija.koeOsallistuminenToisessaKohteessa(hakukohdeOid,osallistuminen);
        assertTrue(!voidaanHyvaksya);

        // Kopio koodia että nähdään mitä tapahtuu
        List<String> kohteenValintakokeet = osallistuminen.getHakutoiveet()
                .stream()
                .filter(h -> h.getHakukohdeOid().equals(hakukohdeOid))
                .flatMap(h->h.getValinnanVaiheet().stream())
                .flatMap(v->v.getValintakokeet().stream())
                .map(k->k.getValintakoeTunniste())
                .collect(Collectors.toList());

        assertEquals(kohteenValintakokeet, Arrays.asList("SOTE1_kaikkiosiot", "SOTEKOE_VK_RYHMA1"));

        List<String> kokeidenTunnisteet = osallistuminen.getHakutoiveet()
                .stream()
                .filter(h -> !h.getHakukohdeOid().equals(hakukohdeOid))
                .flatMap(h -> h.getValinnanVaiheet().stream())
                .flatMap(v -> v.getValintakokeet().stream())
                .map(k -> k.getValintakoeTunniste())
                .collect(Collectors.toList());

        assertEquals(3, kokeidenTunnisteet.stream().filter(s -> s.equals("SOTE1_kaikkiosiot")).count());
        assertTrue(kokeidenTunnisteet.contains("SOTEKOE_KYAMK_ENSIHOITO"));
        assertTrue(kokeidenTunnisteet.contains("kielikoe_amk_fi"));

    }

}
