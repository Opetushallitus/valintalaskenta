package fi.vm.sade.valintalaskenta.laskenta.service.it;

import static ch.lambdaj.Lambda.having;
import static ch.lambdaj.Lambda.on;
import static com.lordofthejars.nosqlunit.core.LoadStrategyEnum.CLEAN_INSERT;
import static com.lordofthejars.nosqlunit.core.LoadStrategyEnum.DELETE_ALL;
import static com.lordofthejars.nosqlunit.mongodb.MongoDbRule.MongoDbRuleBuilder.newMongoDbRule;
import static fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA;
import static fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen.EI_OSALLISTU;
import static fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen.OSALLISTUU;
import static fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.impl.ValintakoelaskentaSuorittajaServiceImpl.VALINNANVAIHE_HAKIJAN_VALINTA;
import static fi.vm.sade.valintalaskenta.laskenta.testdata.TestDataUtil.luoHakemus;
import static fi.vm.sade.valintalaskenta.laskenta.testdata.TestDataUtil.luoValintaperusteetJaValintakoeValinnanVaihe;
import static fi.vm.sade.valintalaskenta.laskenta.testdata.TestDataUtil.luoValintaperusteetJaValintakoeValinnanvaihe;
import static java.util.stream.Collectors.groupingBy;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.not;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import com.google.common.reflect.TypeToken;
import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;
import com.lordofthejars.nosqlunit.annotation.UsingDataSet;
import com.lordofthejars.nosqlunit.mongodb.MongoDbRule;

import co.unruly.matchers.StreamMatchers;
import fi.vm.sade.service.valintaperusteet.dto.FunktiokutsuDTO;
import fi.vm.sade.service.valintaperusteet.dto.SyoteparametriDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.model.Funktionimi;
import fi.vm.sade.service.valintaperusteet.dto.model.Koekutsu;
import fi.vm.sade.valintalaskenta.domain.dto.AvainArvoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.domain.dto.LaskeDTO;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Hakutoive;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.OsallistuminenTulos;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Valintakoe;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeValinnanvaihe;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValintakoeOsallistuminenDAO;
import fi.vm.sade.valintalaskenta.laskenta.resource.ValintakoelaskennanKumulatiivisetTulokset;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.EdellinenValinnanvaiheKasittelija;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.ValintakoelaskentaSuorittajaService;
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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;
import java.util.function.Predicate;
import java.util.stream.Collectors;

@ContextConfiguration(locations = "classpath:application-context-test.xml")
@RunWith(SpringJUnit4ClassRunner.class)
@TestExecutionListeners(listeners = {DependencyInjectionTestExecutionListener.class,
        DirtiesContextTestExecutionListener.class})
public class ValintakoelaskentaSuorittajaServiceIntegrationTest {
    private final String uuid = null;
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

    private boolean korkeakouluhaku = false;

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

    private ValintakoelaskennanKumulatiivisetTulokset kumulatiivisetTulokset = new ValintakoelaskennanKumulatiivisetTulokset();

    @Test
    @UsingDataSet(loadStrategy = DELETE_ALL)
    public void test() {
        final String hakukohdeOid1 = "hakukohdeOid1";
        final String hakukohdeOid2 = "hakukohdeOid2";
        final String hakemusOid = "hakemusOid";
        final String hakuOid = "hakuOid";

        final HakemusDTO hakemus = luoHakemus(hakuOid, hakemusOid, "hakijaOid", hakukohdeOid1, hakukohdeOid2);

        final String valintakoetunniste = "valintakoetunniste";

        final String valinnanVaiheOid1 = "valinnanVaiheOid1";
        final int valinnanVaiheJarjestysluku1 = 0;
        Map<String, FunktiokutsuDTO> kokeet1 = createValintakokeet(valintakoetunniste);
        kokeet1.put("valintakoetunniste2", totuusarvoTrue);

        ValintaperusteetDTO valintaperusteet1 = luoValintaperusteetJaValintakoeValinnanVaihe(hakuOid, hakukohdeOid1, valinnanVaiheOid1, valinnanVaiheJarjestysluku1, kokeet1, Koekutsu.YLIN_TOIVE, "kutsunKohdeAvain");

        final String valinnanVaiheOid2 = "valinnanVaiheOid2";
        final int valinnanVaiheJarjestysluku2 = 1;

        Map<String, FunktiokutsuDTO> kokeet2 = new HashMap<>();
        kokeet2.put(valintakoetunniste, totuusarvoFalse);

        ValintaperusteetDTO valintaperusteet2 = luoValintaperusteetJaValintakoeValinnanVaihe(hakuOid, hakukohdeOid2, valinnanVaiheOid2, valinnanVaiheJarjestysluku2, kokeet2, Koekutsu.YLIN_TOIVE, "kutsunKohdeAvain");

        List<ValintaperusteetDTO> valintaperusteet = new ArrayList<>();
        valintaperusteet.add(valintaperusteet1);
        valintaperusteet.add(valintaperusteet2);

        assertNull(valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid));
        valintakoelaskentaSuorittajaService.laske(hakemus, valintaperusteet, uuid, kumulatiivisetTulokset, korkeakouluhaku);
        ValintakoeOsallistuminen osallistuminen = valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid);
        assertNotNull(osallistuminen);
        assertEquals(osallistuminen.getHakutoiveet().get(0).getValinnanVaiheet().get(0).getValintakokeet().get(0).getOsallistuminenTulos().getOsallistuminen(), OSALLISTUU);
        assertEquals(osallistuminen.getHakutoiveet().get(0).getValinnanVaiheet().get(0).getValintakokeet().get(1).getOsallistuminenTulos().getOsallistuminen(), OSALLISTUU);
    }

    @Test
    @UsingDataSet(locations = "testVanhaTulos.json", loadStrategy = CLEAN_INSERT)
    public void testKoekutsuHakijanValinta() {
        final String hakukohdeOid1 = "hakukohdeOid1";
        final String hakukohdeOid2 = "hakukohdeOid2";
        final String hakemusOid = "hakemusOid";
        final String hakuOid = "hakuOid";

        final HakemusDTO hakemus = luoHakemus(hakuOid, hakemusOid, "hakijaOid", hakukohdeOid1, hakukohdeOid2);

        final String valintakoetunniste = "valintakoetunniste";

        final String valinnanVaiheOid1 = "valinnanVaiheOid1";
        final int valinnanVaiheJarjestysluku1 = 0;
        Map<String, FunktiokutsuDTO> kokeet1 = createValintakokeet(valintakoetunniste);

        ValintaperusteetDTO valintaperusteet1 = luoValintaperusteetJaValintakoeValinnanVaihe(hakuOid, hakukohdeOid1, valinnanVaiheOid1, valinnanVaiheJarjestysluku1, kokeet1, Koekutsu.HAKIJAN_VALINTA, "hakukohdeKutsunKohde2");

        List<ValintaperusteetDTO> valintaperusteet = new ArrayList<>();
        valintaperusteet.add(valintaperusteet1);

        valintakoelaskentaSuorittajaService.laske(hakemus, valintaperusteet, uuid, kumulatiivisetTulokset, korkeakouluhaku);

        ValintakoeOsallistuminen osallistuminen = valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid);
        assertNotNull(osallistuminen);
        assertEquals(OSALLISTUU, osallistuminen.getHakutoiveet().get(0).getValinnanVaiheet().get(0).getValintakokeet().get(0).getOsallistuminenTulos().getOsallistuminen());
        assertEquals(VALINNANVAIHE_HAKIJAN_VALINTA, osallistuminen.getHakutoiveet().get(0).getValinnanVaiheet().get(0).getValinnanVaiheOid());
        assertEquals(new Integer(100), osallistuminen.getHakutoiveet().get(0).getValinnanVaiheet().get(0).getValinnanVaiheJarjestysluku());
        assertEquals(1, osallistuminen.getHakutoiveet().size());
    }

    @Test
    @UsingDataSet(loadStrategy = DELETE_ALL)
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
        Map<String, FunktiokutsuDTO> kokeet1 = createValintakokeet(valintakoetunniste1);

        ValintaperusteetDTO valintaperusteet1 = luoValintaperusteetJaValintakoeValinnanVaihe(hakuOid, hakukohdeOid1, valinnanVaiheOid1, valinnanVaiheJarjestysluku1, kokeet1, Koekutsu.HAKIJAN_VALINTA, "hakukohdeKutsunKohde2");

        final String valinnanVaiheOid2 = "valinnanVaiheOid2";
        final int valinnanVaiheJarjestysluku2 = 0;

        Map<String, FunktiokutsuDTO> kokeet2 = new TreeMap<>();
        kokeet2.put(valintakoetunniste1, totuusarvoTrue);
        kokeet2.put(valintakoetunniste2, totuusarvoTrue);

        ValintaperusteetDTO valintaperusteet2 = luoValintaperusteetJaValintakoeValinnanVaihe(hakuOid, hakukohdeOid2, valinnanVaiheOid2, valinnanVaiheJarjestysluku2, kokeet2, Koekutsu.HAKIJAN_VALINTA, "hakukohdeKutsunKohde2");
        valintaperusteet2.getValinnanVaihe().getValintakoe().get(1).setKutsunKohde(Koekutsu.YLIN_TOIVE);

        final String valinnanVaiheOid3 = "valinnanVaiheOid3";
        final int valinnanVaiheJarjestysluku3 = 0;

        Map<String, FunktiokutsuDTO> kokeet3 = new TreeMap<>();
        kokeet3.put(valintakoetunniste2, totuusarvoTrue);
        kokeet3.put(valintakoetunniste3, totuusarvoTrue);

        ValintaperusteetDTO valintaperusteet3 = luoValintaperusteetJaValintakoeValinnanVaihe(hakuOid, hakukohdeOid3, valinnanVaiheOid3, valinnanVaiheJarjestysluku3, kokeet3, Koekutsu.YLIN_TOIVE, "hakukohdeKutsunKohde2");

        testWithOrder("hakemusOid1", hakukohdeOid1, hakukohdeOid2, hakukohdeOid3, hakuOid, valintaperusteet1, valintaperusteet2, valintaperusteet3);
        testWithOrder("hakemusOid2", hakukohdeOid1, hakukohdeOid2, hakukohdeOid3, hakuOid, valintaperusteet1, valintaperusteet3, valintaperusteet2);
        testWithOrder("hakemusOid3", hakukohdeOid1, hakukohdeOid2, hakukohdeOid3, hakuOid, valintaperusteet2, valintaperusteet1, valintaperusteet3);
        testWithOrder("hakemusOid4", hakukohdeOid1, hakukohdeOid2, hakukohdeOid3, hakuOid, valintaperusteet2, valintaperusteet3, valintaperusteet1);
        testWithOrder("hakemusOid5", hakukohdeOid1, hakukohdeOid2, hakukohdeOid3, hakuOid, valintaperusteet3, valintaperusteet1, valintaperusteet2);
        testWithOrder("hakemusOid6", hakukohdeOid1, hakukohdeOid2, hakukohdeOid3, hakuOid, valintaperusteet3, valintaperusteet2, valintaperusteet1);
    }

    private void testWithOrder(String hakemusOid, String hakukohdeOid1, String hakukohdeOid2, String hakukohdeOid3, String hakuOid, ValintaperusteetDTO vp1, ValintaperusteetDTO vp2, ValintaperusteetDTO vp3) {
        final HakemusDTO hakemus = luoHakemus("hakuOid", hakemusOid, "hakijaOid", hakukohdeOid1, hakukohdeOid2, hakukohdeOid3);
        assertNull(valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid));
        valintakoelaskentaSuorittajaService.laske(hakemus, Collections.singletonList(vp1), uuid, kumulatiivisetTulokset, korkeakouluhaku);
        valintakoelaskentaSuorittajaService.laske(hakemus, Collections.singletonList(vp2), uuid, kumulatiivisetTulokset, korkeakouluhaku);
        valintakoelaskentaSuorittajaService.laske(hakemus, Collections.singletonList(vp3), uuid, kumulatiivisetTulokset, korkeakouluhaku);
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

        final Optional<Valintakoe> valintakoetunniste1 = hakutoive1.get().getValinnanVaiheet().stream().flatMap(v -> v.getValintakokeet().stream()).filter(koeWithTunniste("valintakoetunniste1")).findFirst();
        assertTrue(valintakoetunniste1.isPresent());
        assertEquals(OSALLISTUU, valintakoetunniste1.get().getOsallistuminenTulos().getOsallistuminen());

        final Optional<Valintakoe> valintakoetunniste2 = hakutoive1.get().getValinnanVaiheet().stream().flatMap(v -> v.getValintakokeet().stream()).filter(koeWithTunniste("valintakoetunniste2")).findFirst();
        assertTrue(valintakoetunniste2.isPresent());
        assertEquals(OSALLISTUU, valintakoetunniste2.get().getOsallistuminenTulos().getOsallistuminen());

        Optional<Hakutoive> hakutoive2 = osallistuminen.getHakutoiveet().stream().filter(h -> h.getHakukohdeOid().equals("hakukohdeOid3")).findFirst();
        assertTrue(hakutoive2.isPresent());

        assertEquals(1, hakutoive2.get().getValinnanVaiheet().size());
        assertEquals(2, hakutoive2.get().getValinnanVaiheet().get(0).getValintakokeet().size());

        final Optional<Valintakoe> valintakoetunniste3 = hakutoive2.get().getValinnanVaiheet().stream().flatMap(v -> v.getValintakokeet().stream()).filter(koeWithTunniste("valintakoetunniste3")).findFirst();
        assertTrue(valintakoetunniste3.isPresent());
        assertEquals(OSALLISTUU, valintakoetunniste3.get().getOsallistuminenTulos().getOsallistuminen());

        final Optional<Valintakoe> valintakoetunniste2b = hakutoive2.get().getValinnanVaiheet().stream().flatMap(v -> v.getValintakokeet().stream()).filter(koeWithTunniste("valintakoetunniste2")).findFirst();
        assertTrue(valintakoetunniste2b.isPresent());
        assertEquals(EI_OSALLISTU, valintakoetunniste2b.get().getOsallistuminenTulos().getOsallistuminen());
    }

    @Test
    @UsingDataSet(loadStrategy = DELETE_ALL)
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
        Map<String, FunktiokutsuDTO> kokeet1 = createValintakokeet(valintakoetunniste1);
        kokeet1.put(valintakoetunniste2, totuusarvoTrue);

        ValintaperusteetDTO valintaperusteet1 = luoValintaperusteetJaValintakoeValinnanVaihe(hakuOid, hakukohdeOid1, valinnanVaiheOid1, valinnanVaiheJarjestysluku1, kokeet1, Koekutsu.HAKIJAN_VALINTA, "hakukohdeKutsunKohde1");
        valintaperusteet1.getValinnanVaihe().getValintakoe().stream().filter(koe -> koe.getTunniste().equals(valintakoetunniste2)).findFirst().get().setKutsunKohdeAvain("hakukohdeKutsunKohde2");

        final String valinnanVaiheOid2 = "valinnanVaiheOid2";
        final int valinnanVaiheJarjestysluku2 = 0;

        Map<String, FunktiokutsuDTO> kokeet2 = new TreeMap<>();
        kokeet2.put(valintakoetunniste2, totuusarvoTrue);
        kokeet2.put(valintakoetunniste3, totuusarvoTrue);

        ValintaperusteetDTO valintaperusteet2 = luoValintaperusteetJaValintakoeValinnanVaihe(hakuOid, hakukohdeOid2, valinnanVaiheOid2, valinnanVaiheJarjestysluku2, kokeet2, Koekutsu.HAKIJAN_VALINTA, "hakukohdeKutsunKohde2");
        valintaperusteet2.getValinnanVaihe().getValintakoe().stream().filter(koe -> koe.getTunniste().equals(valintakoetunniste3)).findFirst().get().setKutsunKohde(Koekutsu.YLIN_TOIVE);

        final String valinnanVaiheOid3 = "valinnanVaiheOid3";
        final int valinnanVaiheJarjestysluku3 = 0;

        Map<String, FunktiokutsuDTO> kokeet3 = new TreeMap<>();
        kokeet3.put(valintakoetunniste1, totuusarvoTrue);
        kokeet3.put(valintakoetunniste2, totuusarvoTrue);

        ValintaperusteetDTO valintaperusteet3 = luoValintaperusteetJaValintakoeValinnanVaihe(hakuOid, hakukohdeOid3, valinnanVaiheOid3, valinnanVaiheJarjestysluku3, kokeet3, Koekutsu.HAKIJAN_VALINTA, "hakukohdeKutsunKohde1");
        valintaperusteet3.getValinnanVaihe().getValintakoe().stream().filter(koe -> koe.getTunniste().equals(valintakoetunniste2)).findFirst().get().setKutsunKohdeAvain("hakukohdeKutsunKohde2");

        testWithOrder2("hakemusOid1", hakukohdeOid1, hakukohdeOid2, hakukohdeOid3, hakuOid, valintaperusteet1, valintaperusteet2, valintaperusteet3);
        testWithOrder2("hakemusOid2", hakukohdeOid1, hakukohdeOid2, hakukohdeOid3, hakuOid, valintaperusteet1, valintaperusteet3, valintaperusteet2);
        testWithOrder2("hakemusOid3", hakukohdeOid1, hakukohdeOid2, hakukohdeOid3, hakuOid, valintaperusteet2, valintaperusteet1, valintaperusteet3);
        testWithOrder2("hakemusOid4", hakukohdeOid1, hakukohdeOid2, hakukohdeOid3, hakuOid, valintaperusteet2, valintaperusteet3, valintaperusteet1);
        testWithOrder2("hakemusOid5", hakukohdeOid1, hakukohdeOid2, hakukohdeOid3, hakuOid, valintaperusteet3, valintaperusteet1, valintaperusteet2);
        testWithOrder2("hakemusOid6", hakukohdeOid1, hakukohdeOid2, hakukohdeOid3, hakuOid, valintaperusteet3, valintaperusteet2, valintaperusteet1);
    }

    private void testWithOrder2(String hakemusOid, String hakukohdeOid1, String hakukohdeOid2, String hakukohdeOid3, String hakuOid, ValintaperusteetDTO vp1, ValintaperusteetDTO vp2, ValintaperusteetDTO vp3) {
        final HakemusDTO hakemus = luoHakemus("hakuOid", hakemusOid, "hakijaOid", hakukohdeOid1, hakukohdeOid2, hakukohdeOid3);
        AvainArvoDTO avain = new AvainArvoDTO();
        avain.setAvain("hakukohdeKutsunKohde1");
        avain.setArvo("hakukohdeOid2");
        AvainArvoDTO avain2 = new AvainArvoDTO();
        avain2.setAvain("hakukohdeKutsunKohde2");
        avain2.setArvo("hakukohdeOid3");
        hakemus.setAvaimet(Arrays.asList(avain, avain2));

        assertNull(valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid));
        valintakoelaskentaSuorittajaService.laske(hakemus, Collections.singletonList(vp1), uuid, kumulatiivisetTulokset, korkeakouluhaku);
        valintakoelaskentaSuorittajaService.laske(hakemus, Collections.singletonList(vp2), uuid, kumulatiivisetTulokset, korkeakouluhaku);
        valintakoelaskentaSuorittajaService.laske(hakemus, Collections.singletonList(vp3), uuid, kumulatiivisetTulokset, korkeakouluhaku);
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

        final Optional<Valintakoe> valintakoetunniste1 = hakutoive1.get().getValinnanVaiheet().stream().flatMap(v -> v.getValintakokeet().stream()).filter(koeWithTunniste("valintakoetunniste1")).findFirst();
        assertTrue(valintakoetunniste1.isPresent());
        assertEquals(OSALLISTUU, valintakoetunniste1.get().getOsallistuminenTulos().getOsallistuminen());

        final Optional<Valintakoe> valintakoetunniste3 = hakutoive1.get().getValinnanVaiheet().stream().flatMap(v -> v.getValintakokeet().stream()).filter(koeWithTunniste("valintakoetunniste3")).findFirst();
        assertTrue(valintakoetunniste3.isPresent());
        assertEquals(OSALLISTUU, valintakoetunniste3.get().getOsallistuminenTulos().getOsallistuminen());

        Optional<Hakutoive> hakutoive2 = osallistuminen.getHakutoiveet().stream().filter(h -> h.getHakukohdeOid().equals("hakukohdeOid3")).findFirst();
        assertTrue(hakutoive2.isPresent());

        assertEquals(1, hakutoive2.get().getValinnanVaiheet().size());
        assertEquals(1, hakutoive2.get().getValinnanVaiheet().get(0).getValintakokeet().size());

        final Optional<Valintakoe> valintakoetunniste2 = hakutoive2.get().getValinnanVaiheet().stream().flatMap(v -> v.getValintakokeet().stream()).filter(koeWithTunniste("valintakoetunniste2")).findFirst();
        assertTrue(valintakoetunniste2.isPresent());
        assertEquals(OSALLISTUU, valintakoetunniste2.get().getOsallistuminenTulos().getOsallistuminen());
    }

    @Test
    @UsingDataSet(loadStrategy = DELETE_ALL)
    public void test3() {
        final String hakukohdeOid1 = "hakukohdeOid1";
        final String hakukohdeOid2 = "hakukohdeOid2";
        final String hakukohdeOid3 = "hakukohdeOid3";

        final String hakuOid = "hakuOid";
        final String valintakoetunniste1 = "valintakoetunniste1";
        final String valintakoetunniste2 = "valintakoetunniste2";

        final String valinnanVaiheOid1 = "valinnanVaiheOid1";
        final int valinnanVaiheJarjestysluku1 = 0;
        Map<String, FunktiokutsuDTO> kokeet1 = createValintakokeet(valintakoetunniste1);
        kokeet1.put(valintakoetunniste2, totuusarvoTrue);

        ValintaperusteetDTO valintaperusteet1 = luoValintaperusteetJaValintakoeValinnanVaihe(hakuOid, hakukohdeOid1, valinnanVaiheOid1, valinnanVaiheJarjestysluku1, kokeet1, Koekutsu.HAKIJAN_VALINTA, "hakukohdeKutsunKohde1");
        valintaperusteet1.getValinnanVaihe().getValintakoe().stream().filter(koe -> koe.getTunniste().equals(valintakoetunniste2)).findFirst().get().setKutsunKohdeAvain("hakukohdeKutsunKohde2");

        final String valinnanVaiheOid2 = "valinnanVaiheOid2";
        final int valinnanVaiheJarjestysluku2 = 0;

        Map<String, FunktiokutsuDTO> kokeet2 = new TreeMap<>();
        kokeet2.put(valintakoetunniste2, totuusarvoTrue);

        ValintaperusteetDTO valintaperusteet2 = luoValintaperusteetJaValintakoeValinnanVaihe(hakuOid, hakukohdeOid2, valinnanVaiheOid2, valinnanVaiheJarjestysluku2, kokeet2, Koekutsu.HAKIJAN_VALINTA, "hakukohdeKutsunKohde2");

        final String valinnanVaiheOid3 = "valinnanVaiheOid3";
        final int valinnanVaiheJarjestysluku3 = 0;

        Map<String, FunktiokutsuDTO> kokeet3 = new TreeMap<>();
        kokeet3.put(valintakoetunniste1, totuusarvoTrue);

        ValintaperusteetDTO valintaperusteet3 = luoValintaperusteetJaValintakoeValinnanVaihe(hakuOid, hakukohdeOid3, valinnanVaiheOid3, valinnanVaiheJarjestysluku3, kokeet3, Koekutsu.HAKIJAN_VALINTA, "hakukohdeKutsunKohde1");

        testWithOrder3("hakemusOid1", hakukohdeOid2, hakukohdeOid3, hakuOid, valintaperusteet1, valintaperusteet2, valintaperusteet3);
        testWithOrder3("hakemusOid2", hakukohdeOid2, hakukohdeOid3, hakuOid, valintaperusteet1, valintaperusteet3, valintaperusteet2);
        testWithOrder3("hakemusOid3", hakukohdeOid2, hakukohdeOid3, hakuOid, valintaperusteet2, valintaperusteet1, valintaperusteet3);
        testWithOrder3("hakemusOid4", hakukohdeOid2, hakukohdeOid3, hakuOid, valintaperusteet2, valintaperusteet3, valintaperusteet1);
        testWithOrder3("hakemusOid5", hakukohdeOid2, hakukohdeOid3, hakuOid, valintaperusteet3, valintaperusteet1, valintaperusteet2);
        testWithOrder3("hakemusOid6", hakukohdeOid2, hakukohdeOid3, hakuOid, valintaperusteet3, valintaperusteet2, valintaperusteet1);
    }

    private void testWithOrder3(String hakemusOid, String hakukohdeOid2, String hakukohdeOid3, String hakuOid, ValintaperusteetDTO vp1, ValintaperusteetDTO vp2, ValintaperusteetDTO vp3) {
        final HakemusDTO hakemus = luoHakemus("hakuOid", hakemusOid, "hakijaOid", hakukohdeOid2, hakukohdeOid3);
        AvainArvoDTO avain = new AvainArvoDTO();
        avain.setAvain("hakukohdeKutsunKohde1");
        avain.setArvo("hakukohdeOid1");
        AvainArvoDTO avain2 = new AvainArvoDTO();
        avain2.setAvain("hakukohdeKutsunKohde2");
        avain2.setArvo("hakukohdeOid1");
        hakemus.setAvaimet(Arrays.asList(avain, avain2));

        assertNull(valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid));
        valintakoelaskentaSuorittajaService.laske(hakemus, Collections.singletonList(vp1), uuid, kumulatiivisetTulokset, korkeakouluhaku);
        valintakoelaskentaSuorittajaService.laske(hakemus, Collections.singletonList(vp2), uuid, kumulatiivisetTulokset, korkeakouluhaku);
        valintakoelaskentaSuorittajaService.laske(hakemus, Collections.singletonList(vp3), uuid, kumulatiivisetTulokset, korkeakouluhaku);
        assertTest3Results(valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid));
    }

    private void assertTest3Results(ValintakoeOsallistuminen osallistuminen) {
        assertNotNull(osallistuminen);

        assertEquals(1, osallistuminen.getHakutoiveet().size());

        Hakutoive hakutoive1 = osallistuminen.getHakutoiveet().get(0);
        assertEquals(1, hakutoive1.getValinnanVaiheet().size());

        assertEquals(2, hakutoive1.getValinnanVaiheet().get(0).getValintakokeet().size());

        final Optional<Valintakoe> valintakoetunniste2 = hakutoive1.getValinnanVaiheet().stream().flatMap(v -> v.getValintakokeet().stream()).filter(koeWithTunniste("valintakoetunniste2")).findFirst();
        assertTrue(valintakoetunniste2.isPresent());
        assertEquals(OSALLISTUU, valintakoetunniste2.get().getOsallistuminenTulos().getOsallistuminen());

        final Optional<Valintakoe> valintakoetunniste3 = hakutoive1.getValinnanVaiheet().stream().flatMap(v -> v.getValintakokeet().stream()).filter(koeWithTunniste("valintakoetunniste1")).findFirst();
        assertTrue(valintakoetunniste3.isPresent());
        assertEquals(OSALLISTUU, valintakoetunniste3.get().getOsallistuminenTulos().getOsallistuminen());
    }

    @Test
    @UsingDataSet(loadStrategy = DELETE_ALL)
    public void test4() {
        final String hakukohdeOid1 = "hakukohdeOid1";
        final String hakukohdeOid2 = "hakukohdeOid2";
        final String hakukohdeOid3 = "hakukohdeOid3";

        final String hakuOid = "hakuOid";
        final String valintakoetunniste1 = "valintakoetunniste1";
        final String valintakoetunniste2 = "valintakoetunniste2";
        final String valintakoetunniste3 = "valintakoetunniste3";

        final String valinnanVaiheOid1 = "valinnanVaiheOid1";
        final int valinnanVaiheJarjestysluku1 = 0;
        Map<String, FunktiokutsuDTO> kokeet1 = createValintakokeet(valintakoetunniste1);
        kokeet1.put(valintakoetunniste2, totuusarvoTrue);

        ValintaperusteetDTO valintaperusteet1 = luoValintaperusteetJaValintakoeValinnanVaihe(hakuOid, hakukohdeOid1, valinnanVaiheOid1, valinnanVaiheJarjestysluku1, kokeet1, Koekutsu.HAKIJAN_VALINTA, "hakukohdeKutsunKohde1");
        valintaperusteet1.getValinnanVaihe().getValintakoe().stream().filter(koe -> koe.getTunniste().equals(valintakoetunniste2)).findFirst().get().setKutsunKohdeAvain("hakukohdeKutsunKohde2");

        final String valinnanVaiheOid2 = "valinnanVaiheOid2";
        final int valinnanVaiheJarjestysluku2 = 0;

        Map<String, FunktiokutsuDTO> kokeet2 = new TreeMap<>();
        kokeet2.put(valintakoetunniste2, totuusarvoTrue);
        kokeet2.put(valintakoetunniste3, totuusarvoTrue);

        ValintaperusteetDTO valintaperusteet2 = luoValintaperusteetJaValintakoeValinnanVaihe(hakuOid, hakukohdeOid2, valinnanVaiheOid2, valinnanVaiheJarjestysluku2, kokeet2, Koekutsu.HAKIJAN_VALINTA, "hakukohdeKutsunKohde2");
        valintaperusteet2.getValinnanVaihe().getValintakoe().stream().filter(koe -> koe.getTunniste().equals(valintakoetunniste3)).findFirst().get().setKutsunKohde(Koekutsu.YLIN_TOIVE);

        final String valinnanVaiheOid3 = "valinnanVaiheOid3";
        final int valinnanVaiheJarjestysluku3 = 0;

        Map<String, FunktiokutsuDTO> kokeet3 = new TreeMap<>();
        kokeet3.put(valintakoetunniste1, totuusarvoTrue);
        kokeet3.put(valintakoetunniste2, totuusarvoTrue);

        ValintaperusteetDTO valintaperusteet3 = luoValintaperusteetJaValintakoeValinnanVaihe(hakuOid, hakukohdeOid3, valinnanVaiheOid3, valinnanVaiheJarjestysluku3, kokeet3, Koekutsu.HAKIJAN_VALINTA, "hakukohdeKutsunKohde1");
        valintaperusteet3.getValinnanVaihe().getValintakoe().stream().filter(koe -> koe.getTunniste().equals(valintakoetunniste2)).findFirst().get().setKutsunKohdeAvain("hakukohdeKutsunKohde2");

        AvainArvoDTO avain = new AvainArvoDTO();
        avain.setAvain("hakukohdeKutsunKohde1");
        avain.setArvo("hakukohdeOid2");
        AvainArvoDTO avain2 = new AvainArvoDTO();
        avain2.setAvain("hakukohdeKutsunKohde2");
        avain2.setArvo("hakukohdeOid3");

        assertNull(valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, "hakemusOid1"));
        final HakemusDTO hakemus = luoHakemus("hakuOid", "hakemusOid1", "hakijaOid", hakukohdeOid1, hakukohdeOid2, hakukohdeOid3);
        hakemus.setAvaimet(Arrays.asList(avain, avain2));

        valintakoelaskentaSuorittajaService.laske(hakemus, Arrays.asList(valintaperusteet1, valintaperusteet2, valintaperusteet3), uuid, kumulatiivisetTulokset, korkeakouluhaku);
        final HakemusDTO hakemus2 = luoHakemus("hakuOid", "hakemusOid2", "hakijaOid", hakukohdeOid1, hakukohdeOid3);
        hakemus2.setAvaimet(Arrays.asList(avain, avain2));

        valintakoelaskentaSuorittajaService.laske(hakemus2, Arrays.asList(valintaperusteet1, valintaperusteet2, valintaperusteet3), uuid, kumulatiivisetTulokset, korkeakouluhaku);
        assertTest4Results(valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, "hakemusOid2"));
    }

    private void assertTest4Results(ValintakoeOsallistuminen osallistuminen) {
        assertNotNull(osallistuminen);

        assertEquals(2, osallistuminen.getHakutoiveet().size());

        Hakutoive hakutoive1 = osallistuminen.getHakutoiveet().get(0);
        assertEquals(1, hakutoive1.getValinnanVaiheet().size());
        assertEquals(1, hakutoive1.getValinnanVaiheet().get(0).getValintakokeet().size());

        final Optional<Valintakoe> valintakoetunniste2 = hakutoive1.getValinnanVaiheet().stream().flatMap(v -> v.getValintakokeet().stream()).filter(koeWithTunniste("valintakoetunniste2")).findFirst();
        assertTrue(valintakoetunniste2.isPresent());
        assertEquals(OSALLISTUU, valintakoetunniste2.get().getOsallistuminenTulos().getOsallistuminen());

        Hakutoive hakutoive2 = osallistuminen.getHakutoiveet().get(1);
        assertEquals(1, hakutoive2.getValinnanVaiheet().size());
        assertEquals(1, hakutoive2.getValinnanVaiheet().get(0).getValintakokeet().size());

        final Optional<Valintakoe> valintakoetunniste1 = hakutoive2.getValinnanVaiheet().stream().flatMap(v -> v.getValintakokeet().stream()).filter(koeWithTunniste("valintakoetunniste1")).findFirst();
        assertTrue(valintakoetunniste1.isPresent());
        assertEquals(OSALLISTUU, valintakoetunniste1.get().getOsallistuminenTulos().getOsallistuminen());
    }

    @Test
    @UsingDataSet(locations = "testEsivalinnassaHylatty.json", loadStrategy = CLEAN_INSERT)
    public void testEiKoekutsujaAikaisemminHylatyilleHakijanValinnoille() {
        final String hakemusOid = "1.2.246.562.11.00000072753";
        final String hakukohdeOid = "1.2.246.562.5.91937845484";
        final String hakuOid = "1.2.246.562.5.2013080813081926341927";
        final String valinnanVaiheOid = "valinnanVaiheHakijanValinta";
        final String valintakoetunniste = "kielikoe_tunniste";

        ValintaperusteetDTO vv2 = luoValintaperusteetJaValintakoeValinnanvaihe(hakuOid, hakukohdeOid, valinnanVaiheOid, 1, valintakoetunniste);
        vv2.getValinnanVaihe().getValintakoe().stream().filter(koe -> koe.getTunniste().equals(valintakoetunniste)).findFirst().get().setKutsunKohde(Koekutsu.HAKIJAN_VALINTA);
        vv2.getValinnanVaihe().getValintakoe().stream().filter(koe -> koe.getTunniste().equals(valintakoetunniste)).findFirst().get().setKutsunKohdeAvain("hakukohdeKutsunKohde");

        AvainArvoDTO avain2 = new AvainArvoDTO();
        avain2.setAvain("hakukohdeKutsunKohde");
        avain2.setArvo(hakukohdeOid);
        final HakemusDTO hakemus = luoHakemus(hakuOid, hakemusOid, "hakijaOid", hakukohdeOid);
        hakemus.setAvaimet(Collections.singletonList(avain2));

        valintakoelaskentaSuorittajaService.laske(hakemus, Collections.singletonList(vv2), uuid, kumulatiivisetTulokset, korkeakouluhaku);

        ValintakoeOsallistuminen osallistuminen = valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid);
        assertNotNull(osallistuminen);

        //todo: add asserts (work in progress)
    }

    @Test
    @UsingDataSet(locations = "testViimeisinValinnanVaihe.json", loadStrategy = CLEAN_INSERT)
    public void testViimeisinValinnanVaihe() {
        final String hakemusOid = "1.2.246.562.11.00000072753";
        final String hakukohdeOid = "1.2.246.562.5.91937845484";
        final String hakuOid = "1.2.246.562.5.2013080813081926341927";
        final String valinnanVaiheOid = "vv2";
        final String valintakoetunniste = "koe1";

        ValintaperusteetDTO vv2 = luoValintaperusteetJaValintakoeValinnanvaihe(hakuOid, hakukohdeOid, valinnanVaiheOid, 1, valintakoetunniste);
        valintakoelaskentaSuorittajaService.laske(luoHakemus(hakuOid, hakemusOid, hakemusOid, hakukohdeOid), Collections.singletonList(vv2), uuid, kumulatiivisetTulokset, korkeakouluhaku);

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
        assertEquals(EI_OSALLISTU, koe.getOsallistuminenTulos().getOsallistuminen());
    }

    @Test
    @UsingDataSet(locations = "testViimeisinValinnanVaiheEkaHylatty.json", loadStrategy = CLEAN_INSERT)
    public void testViimeisinValinnanVaiheEnsimmainenHakutoiveHylatty() {
        // Testa
        final String hakemusOid = "1.2.246.562.11.00000072753";
        final String hakukohdeOid1 = "1.2.246.562.5.91937845484";
        final String hakukohdeOid2 = "1.2.246.562.5.91937845485";
        final String hakuOid = "1.2.246.562.5.2013080813081926341927";
        final String koekutsuvaiheOid_kohde1 = "vv2_kohde1";
        final String koekutsuvaiheOid_kohde2 = "vv2_kohde2";
        final String valintakoetunniste = "koetunniste";

        ValintaperusteetDTO vv2_kohde1 = luoValintaperusteetJaValintakoeValinnanVaihe(hakuOid, hakukohdeOid1, koekutsuvaiheOid_kohde1, 1, createValintakokeet(valintakoetunniste), Koekutsu.YLIN_TOIVE, "hakukohdeKutsunKohde2");
        ValintaperusteetDTO vv2_kohde2 = luoValintaperusteetJaValintakoeValinnanVaihe(hakuOid, hakukohdeOid2, koekutsuvaiheOid_kohde2, 1, createValintakokeet(valintakoetunniste), Koekutsu.YLIN_TOIVE, "hakukohdeKutsunKohde2");

        valintakoelaskentaSuorittajaService.laske(luoHakemus(hakuOid, hakemusOid, hakemusOid, hakukohdeOid1, hakukohdeOid2), Collections.singletonList(vv2_kohde1), uuid, kumulatiivisetTulokset, korkeakouluhaku);
        valintakoelaskentaSuorittajaService.laske(luoHakemus(hakuOid, hakemusOid, hakemusOid, hakukohdeOid1, hakukohdeOid2), Collections.singletonList(vv2_kohde2), uuid, kumulatiivisetTulokset, korkeakouluhaku);

        ValintakoeOsallistuminen osallistuminen = valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid);

        assertNotNull(osallistuminen);

        assertEquals(hakukohdeOid1, osallistuminen.getHakutoiveet().get(0).getHakukohdeOid());
        assertEquals(EI_OSALLISTU, osallistuminen.getHakutoiveet().get(0).getValinnanVaiheet().get(0).getValintakokeet().get(0).getOsallistuminenTulos().getOsallistuminen());

        assertEquals(hakukohdeOid2, osallistuminen.getHakutoiveet().get(1).getHakukohdeOid());
        assertEquals(OSALLISTUU, osallistuminen.getHakutoiveet().get(1).getValinnanVaiheet().get(0).getValintakokeet().get(0).getOsallistuminenTulos().getOsallistuminen());

    }

    private Map<String, FunktiokutsuDTO> createValintakokeet(final String valintakoetunniste) {
        Map<String, FunktiokutsuDTO> kokeet1 = new HashMap<>();
        kokeet1.put(valintakoetunniste, totuusarvoTrue);
        return kokeet1;
    }

    @Test
    @UsingDataSet(locations = "osallistuminen.json", loadStrategy = CLEAN_INSERT)
    public void testOlemassaolevatKokoeet() throws JsonSyntaxException,
            IOException {


        LaskeDTO dto = readJson("laskeDTO.json", new TypeToken<LaskeDTO>() {});

        valintakoelaskentaSuorittajaService.laske(dto.getHakemus().get(0), dto.getValintaperuste(), uuid, kumulatiivisetTulokset, korkeakouluhaku);

        ValintakoeOsallistuminen osallistuminen = valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid("1.2.246.562.5.2013080813081926341927", "1.2.246.562.11.00000304421");

        assertTrue(osallistuminen.getHakutoiveet().size() == 2);

        osallistuminen.getHakutoiveet().sort(Comparator.comparing(Hakutoive::getHakukohdeOid));

        assertTrue(osallistuminen.getHakutoiveet().get(0).getValinnanVaiheet().get(0).getValintakokeet().size() == 3);
        assertTrue(osallistuminen.getHakutoiveet().get(1).getValinnanVaiheet().get(0).getValintakokeet().size() == 3);

        osallistuminen.getHakutoiveet().get(0).getValinnanVaiheet().get(0).getValintakokeet().sort(Comparator.comparing(Valintakoe::getValintakoeTunniste));
        osallistuminen.getHakutoiveet().get(1).getValinnanVaiheet().get(0).getValintakokeet().sort(Comparator.comparing(Valintakoe::getValintakoeTunniste));

        assertEquals(OSALLISTUU, osallistuminen.getHakutoiveet().get(1).getValinnanVaiheet().get(0).getValintakokeet().get(1).getOsallistuminenTulos().getOsallistuminen());
        assertEquals(EI_OSALLISTU, osallistuminen.getHakutoiveet().get(0).getValinnanVaiheet().get(0).getValintakokeet().get(1).getOsallistuminenTulos().getOsallistuminen());

    }

    @Test
    @UsingDataSet(loadStrategy = DELETE_ALL)
    public void kielikokeeseenKutsutaanJosSuoritustaTaiTodennettuaKielitaitoaEiLoydy() throws JsonSyntaxException, IOException {
        LaskeDTO laskeDTOIlmanKielikoetulosta = readJson("laskeDTOIlmanKielikoetulosta.json", new TypeToken<LaskeDTO>() {});

        valintakoelaskentaSuorittajaService.laske(laskeDTOIlmanKielikoetulosta.getHakemus().get(0), laskeDTOIlmanKielikoetulosta.getValintaperuste(), uuid, kumulatiivisetTulokset, korkeakouluhaku);

        ValintakoeOsallistuminen osallistuminen = valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid("1.2.246.562.5.2013080813081926341927", "1.2.246.562.11.00000304421");

        assertEquals(1, osallistuminen.getHakutoiveet().size());

        Hakutoive osallistumisenHakutoiveJohonOnKielikoe = osallistuminen.getHakutoiveet().get(0);
        ValintakoeValinnanvaihe kielikokeenPakollisuusVaihe = osallistumisenHakutoiveJohonOnKielikoe.getValinnanVaiheet().get(0);
        assertEquals(1, kielikokeenPakollisuusVaihe.getValintakokeet().size());

        Valintakoe kielikoetulos = kielikokeenPakollisuusVaihe.getValintakokeet().get(0);
        OsallistuminenTulos kielikoetulosOsallistuminenTulos = kielikoetulos.getOsallistuminenTulos();

        assertThat(kielikoetulosOsallistuminenTulos, having(on(OsallistuminenTulos.class).getOsallistuminen(), equalTo(OSALLISTUU)));
        assertThat(kielikoetulosOsallistuminenTulos, having(on(OsallistuminenTulos.class).getLaskentaTulos(), equalTo(true)));
        assertThat(kielikoetulosOsallistuminenTulos, having(on(OsallistuminenTulos.class).getLaskentaTila(), equalTo(HYVAKSYTTAVISSA.name())));
    }

    @Test
    @UsingDataSet(loadStrategy = DELETE_ALL)
    public void kielikokeeseenKutsutaanJosOsallistuminenLoytyyKyseiseltaHakemukselta() throws JsonSyntaxException, IOException {
        LaskeDTO laskeDTOIlmanKielikoetulosta = readJson("laskeDTOIlmanKielikoetulosta.json", new TypeToken<LaskeDTO>() {});
        HakemusDTO hakemus = laskeDTOIlmanKielikoetulosta.getHakemus().get(0);
        setValueOnCombinedHakemusData(hakemus, "kielikoe_fi", "true");
        setValueOnCombinedHakemusData(hakemus, "kielikoe_fi-OSALLISTUMINEN", "OSALLISTUI");

        valintakoelaskentaSuorittajaService.laske(hakemus, laskeDTOIlmanKielikoetulosta.getValintaperuste(), uuid, kumulatiivisetTulokset, korkeakouluhaku);

        ValintakoeOsallistuminen osallistuminen = valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid("1.2.246.562.5.2013080813081926341927", "1.2.246.562.11.00000304421");

        assertEquals(1, osallistuminen.getHakutoiveet().size());

        Hakutoive osallistumisenHakutoiveJohonOnKielikoe = osallistuminen.getHakutoiveet().get(0);
        ValintakoeValinnanvaihe kielikokeenPakollisuusVaihe = osallistumisenHakutoiveJohonOnKielikoe.getValinnanVaiheet().get(0);
        assertEquals(1, kielikokeenPakollisuusVaihe.getValintakokeet().size());

        Valintakoe kielikoetulos = kielikokeenPakollisuusVaihe.getValintakokeet().get(0);
        OsallistuminenTulos kielikoetulosOsallistuminenTulos = kielikoetulos.getOsallistuminenTulos();

        assertThat(kielikoetulosOsallistuminenTulos, having(on(OsallistuminenTulos.class).getOsallistuminen(), equalTo(OSALLISTUU)));
        assertThat(kielikoetulosOsallistuminenTulos, having(on(OsallistuminenTulos.class).getLaskentaTulos(), equalTo(true)));
        assertThat(kielikoetulosOsallistuminenTulos, having(on(OsallistuminenTulos.class).getLaskentaTila(), equalTo(HYVAKSYTTAVISSA.name())));
    }

    @Test
    @UsingDataSet(loadStrategy = DELETE_ALL)
    public void kielikokeeseenEiKutsutaJosSuoritusLoytyyEriHakemukselta() throws JsonSyntaxException, IOException {
        LaskeDTO laskeDTOIlmanKielikoetulosta = readJson("laskeDTOIlmanKielikoetulosta.json", new TypeToken<LaskeDTO>() {});
        HakemusDTO hakemus = laskeDTOIlmanKielikoetulosta.getHakemus().get(0);
        setValueOnCombinedHakemusData(hakemus, "kielikoe_fi", "true");
        setValueOnCombinedHakemusData(hakemus, "kielikoe_fi-OSALLISTUMINEN", "MERKITSEMATTA");

        valintakoelaskentaSuorittajaService.laske(hakemus, laskeDTOIlmanKielikoetulosta.getValintaperuste(), uuid, kumulatiivisetTulokset, korkeakouluhaku);

        ValintakoeOsallistuminen osallistuminen = valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid("1.2.246.562.5.2013080813081926341927", "1.2.246.562.11.00000304421");

        assertEquals(1, osallistuminen.getHakutoiveet().size());

        Hakutoive osallistumisenHakutoiveJohonOnKielikoe = osallistuminen.getHakutoiveet().get(0);
        ValintakoeValinnanvaihe kielikokeenPakollisuusVaihe = osallistumisenHakutoiveJohonOnKielikoe.getValinnanVaiheet().get(0);
        assertEquals(1, kielikokeenPakollisuusVaihe.getValintakokeet().size());

        Valintakoe kielikoetulos = kielikokeenPakollisuusVaihe.getValintakokeet().get(0);
        OsallistuminenTulos kielikoetulosOsallistuminenTulos = kielikoetulos.getOsallistuminenTulos();

        assertThat(kielikoetulosOsallistuminenTulos, having(on(OsallistuminenTulos.class).getOsallistuminen(), equalTo(EI_OSALLISTU)));
        assertThat(kielikoetulosOsallistuminenTulos, having(on(OsallistuminenTulos.class).getLaskentaTulos(), equalTo(false)));
        assertThat(kielikoetulosOsallistuminenTulos, having(on(OsallistuminenTulos.class).getLaskentaTila(), equalTo(HYVAKSYTTAVISSA.name())));
    }

    private <T> T readJson(String pathInClasspath, TypeToken<T> typeToken) throws IOException {
        return new Gson().fromJson(IOUtils.toString(new ClassPathResource(pathInClasspath).getInputStream()), typeToken.getType());
    }

    @Test
    @UsingDataSet(locations = "voidaanHyvaksya.json", loadStrategy = CLEAN_INSERT)
    public void testMukanaKokeessaToisessaKohteessa() {
        final String hakemusOid = "1.2.246.562.11.00001212279";
        final String hakukohdeOid = "1.2.246.562.20.66128426039";
        final String hakuOid = "1.2.246.562.29.173465377510";
        final String hakemusOid2 = "1.2.246.562.11.00001223556";

        ValintakoeOsallistuminen osallistuminen = valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid);
        assertNotNull(osallistuminen);
        boolean voidaanHyvaksya = edellinenValinnanvaiheKasittelija.kuhunkinKohteenKokeeseenOsallistutaanToisessaKohteessa(hakukohdeOid,osallistuminen);
        assertTrue(voidaanHyvaksya);

        osallistuminen = valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid2);
        assertNotNull(osallistuminen);
        voidaanHyvaksya = edellinenValinnanvaiheKasittelija.kuhunkinKohteenKokeeseenOsallistutaanToisessaKohteessa(hakukohdeOid,osallistuminen);
        assertTrue(!voidaanHyvaksya);

        // Kopio koodia että nähdään mitä tapahtuu
        List<String> kohteenValintakokeet = osallistuminen.getHakutoiveet()
                .stream()
                .filter(h -> h.getHakukohdeOid().equals(hakukohdeOid))
                .flatMap(h->h.getValinnanVaiheet().stream())
                .flatMap(v->v.getValintakokeet().stream())
                .map(Valintakoe::getValintakoeTunniste)
                .collect(Collectors.toList());

        assertEquals(kohteenValintakokeet, Arrays.asList("SOTE1_kaikkiosiot", "SOTEKOE_VK_RYHMA1"));

        List<String> kokeidenTunnisteet = osallistuminen.getHakutoiveet()
                .stream()
                .filter(h -> !h.getHakukohdeOid().equals(hakukohdeOid))
                .flatMap(h -> h.getValinnanVaiheet().stream())
                .flatMap(v -> v.getValintakokeet().stream())
                .map(Valintakoe::getValintakoeTunniste)
                .collect(Collectors.toList());

        assertEquals(3, kokeidenTunnisteet.stream().filter(s -> s.equals("SOTE1_kaikkiosiot")).count());
        assertTrue(kokeidenTunnisteet.contains("SOTEKOE_KYAMK_ENSIHOITO"));
        assertTrue(kokeidenTunnisteet.contains("kielikoe_amk_fi"));

    }

    @Test
    @UsingDataSet(locations = "toisessaKohteessaKoeJohonEiOsallistuta.json", loadStrategy = CLEAN_INSERT)
    public void testMukanaYhdessaMutteiKaikissaKokeissaToisessaKohteessa() {
        final String hakemusOid = "1.2.246.562.11.00001212279";
        final String hakukohdeOid = "1.2.246.562.20.66128426039";
        final String hakuOid = "1.2.246.562.29.173465377510";

        ValintakoeOsallistuminen osallistuminen = valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid);
        assertNotNull(osallistuminen);

        List<Valintakoe> kohteenValintakokeet = osallistuminen.getHakutoiveet()
            .stream()
            .filter(h -> h.getHakukohdeOid().equals(hakukohdeOid))
            .flatMap(h -> h.getValinnanVaiheet().stream())
            .flatMap(v -> v.getValintakokeet().stream())
            .sorted(Comparator.comparing(Valintakoe::getValintakoeTunniste))
            .collect(Collectors.toList());

        assertThat(kohteenValintakokeet, hasSize(3));
        assertThat(kohteenValintakokeet.stream().map(Valintakoe::getValintakoeTunniste),
            StreamMatchers.contains("KOHTEEN_SPESIFI_KOE_BUG-1564", "SOTE1_kaikkiosiot", "SOTEKOE_VK_RYHMA1"));

        Map<Osallistuminen, List<Valintakoe>> kohteenKokeetOsallistumisenMukaan =
            kohteenValintakokeet.stream().collect(groupingBy(k -> k.getOsallistuminenTulos().getOsallistuminen()));
        assertThat(kohteenKokeetOsallistumisenMukaan.get(OSALLISTUU), hasSize(1));
        assertThat(kohteenKokeetOsallistumisenMukaan.get(OSALLISTUU)
            .stream()
            .map(Valintakoe::getValintakoeTunniste), StreamMatchers.contains("KOHTEEN_SPESIFI_KOE_BUG-1564"));
        assertThat(kohteenKokeetOsallistumisenMukaan.get(EI_OSALLISTU), hasSize(2));
        assertThat(kohteenKokeetOsallistumisenMukaan.get(EI_OSALLISTU)
            .stream()
            .map(Valintakoe::getValintakoeTunniste), StreamMatchers.contains("SOTE1_kaikkiosiot", "SOTEKOE_VK_RYHMA1"));

        List<Valintakoe> muidenkohteidenKokeetJoihinOsallistutaan = osallistuminen.getHakutoiveet()
            .stream()
            .filter(h -> !h.getHakukohdeOid().equals(hakukohdeOid))
            .flatMap(h -> h.getValinnanVaiheet().stream())
            .flatMap(v -> v.getValintakokeet().stream())
            .filter(k -> OSALLISTUU.equals(k.getOsallistuminenTulos().getOsallistuminen()))
            .sorted(Comparator.comparing(Valintakoe::getValintakoeTunniste))
            .collect(Collectors.toList());

        assertThat(muidenkohteidenKokeetJoihinOsallistutaan, hasSize(2));
        assertThat(muidenkohteidenKokeetJoihinOsallistutaan.stream().map(Valintakoe::getValintakoeTunniste),
            StreamMatchers.contains("SOTE1_kaikkiosiot", "SOTEKOE_VK_RYHMA1"));
        assertThat(muidenkohteidenKokeetJoihinOsallistutaan.stream().map(Valintakoe::getValintakoeTunniste),
            not(StreamMatchers.contains("KOHTEEN_SPESIFI_KOE_BUG-1564")));

        assertFalse(edellinenValinnanvaiheKasittelija.kuhunkinKohteenKokeeseenOsallistutaanToisessaKohteessa(hakukohdeOid, osallistuminen));
    }

    private Predicate<Valintakoe> koeWithTunniste(String tunniste) {
        return k -> k.getValintakoeTunniste().equals(tunniste);
    }

    private void setValueOnCombinedHakemusData(HakemusDTO hakemus, String avain, String arvo) {
        hakemus.getAvaimet().stream().filter(a -> a.getAvain().equals(avain)).forEach(a -> a.setArvo(arvo));
    }
}
