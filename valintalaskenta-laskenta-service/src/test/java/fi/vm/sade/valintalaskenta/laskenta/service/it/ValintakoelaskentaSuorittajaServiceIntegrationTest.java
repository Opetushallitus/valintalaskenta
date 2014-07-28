package fi.vm.sade.valintalaskenta.laskenta.service.it;

import com.lordofthejars.nosqlunit.annotation.UsingDataSet;
import com.lordofthejars.nosqlunit.core.LoadStrategyEnum;
import com.lordofthejars.nosqlunit.mongodb.MongoDbRule;
import fi.vm.sade.service.valintaperusteet.dto.FunktiokutsuDTO;
import fi.vm.sade.service.valintaperusteet.dto.SyoteparametriDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.model.Funktionimi;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.domain.valintakoe.*;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValintakoeOsallistuminenDAO;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.ValintakoelaskentaSuorittajaService;
import fi.vm.sade.valintalaskenta.laskenta.testdata.TestDataUtil;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestExecutionListeners;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.support.DependencyInjectionTestExecutionListener;
import org.springframework.test.context.support.DirtiesContextTestExecutionListener;

import java.util.*;

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
                valinnanVaiheJarjestysluku1, kokeet1);

        final String valinnanVaiheOid2 = "valinnanVaiheOid2";
        final int valinnanVaiheJarjestysluku2 = 1;

        Map<String, FunktiokutsuDTO> kokeet2 = new HashMap<String, FunktiokutsuDTO>();
        kokeet2.put(valintakoetunniste, totuusarvoFalse);

        ValintaperusteetDTO valintaperusteet2 = TestDataUtil.luoValintaperusteetJaValintakoeValinnanVaihe(hakuOid, hakukohdeOid2, valinnanVaiheOid2,
                valinnanVaiheJarjestysluku2, kokeet2);

        List<ValintaperusteetDTO> valintaperusteet = new ArrayList<ValintaperusteetDTO>();
        valintaperusteet.add(valintaperusteet1);
        valintaperusteet.add(valintaperusteet2);

        assertNull(valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid));
        valintakoelaskentaSuorittajaService.laske(hakemus, valintaperusteet);
        ValintakoeOsallistuminen osallistuminen = valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid);
        assertNotNull(osallistuminen);
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

}
