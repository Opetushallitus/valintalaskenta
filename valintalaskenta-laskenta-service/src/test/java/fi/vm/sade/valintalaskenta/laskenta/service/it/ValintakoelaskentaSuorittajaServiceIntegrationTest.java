package fi.vm.sade.valintalaskenta.laskenta.service.it;

import com.lordofthejars.nosqlunit.annotation.UsingDataSet;
import com.lordofthejars.nosqlunit.core.LoadStrategyEnum;
import com.lordofthejars.nosqlunit.mongodb.MongoDbRule;
import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.valintaperusteet.model.Funktionimi;
import fi.vm.sade.service.valintaperusteet.schema.FunktiokutsuTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.SyoteparametriTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteetTyyppi;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.lordofthejars.nosqlunit.mongodb.MongoDbRule.MongoDbRuleBuilder.newMongoDbRule;
import static fi.vm.sade.valintalaskenta.laskenta.testdata.TestDataUtil.luoHakemus;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

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

    private static final FunktiokutsuTyyppi totuusarvoTrue;
    private static final FunktiokutsuTyyppi totuusarvoFalse;


    static {
        totuusarvoTrue = new FunktiokutsuTyyppi();
        totuusarvoTrue.setFunktionimi(Funktionimi.TOTUUSARVO.name());

        {
            SyoteparametriTyyppi param = new SyoteparametriTyyppi();
            param.setAvain("totuusarvo");
            param.setArvo(Boolean.TRUE.toString());
            totuusarvoTrue.getSyoteparametrit().add(param);
        }

        totuusarvoFalse = new FunktiokutsuTyyppi();
        totuusarvoFalse.setFunktionimi(Funktionimi.TOTUUSARVO.name());
        {
            SyoteparametriTyyppi param = new SyoteparametriTyyppi();
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

        final HakemusTyyppi hakemus = luoHakemus(hakemusOid, "hakijaOid", hakukohdeOid1, hakukohdeOid2);

        final String hakuOid = "hakuOid";
        final String valintakoetunniste = "valintakoetunniste";

        final String valinnanVaiheOid1 = "valinnanVaiheOid1";
        final int valinnanVaiheJarjestysluku1 = 0;

        Map<String, FunktiokutsuTyyppi> kokeet1 = new HashMap<String, FunktiokutsuTyyppi>();
        kokeet1.put(valintakoetunniste, totuusarvoTrue);
        kokeet1.put("valintakoetunniste2", totuusarvoTrue);

        ValintaperusteetTyyppi valintaperusteet1 = TestDataUtil.luoValintaperusteetJaValintakoeValinnanVaihe(hakuOid, hakukohdeOid1, valinnanVaiheOid1,
                valinnanVaiheJarjestysluku1, kokeet1);

        final String valinnanVaiheOid2 = "valinnanVaiheOid2";
        final int valinnanVaiheJarjestysluku2 = 1;

        Map<String, FunktiokutsuTyyppi> kokeet2 = new HashMap<String, FunktiokutsuTyyppi>();
        kokeet2.put(valintakoetunniste, totuusarvoFalse);

        ValintaperusteetTyyppi valintaperusteet2 = TestDataUtil.luoValintaperusteetJaValintakoeValinnanVaihe(hakuOid, hakukohdeOid2, valinnanVaiheOid2,
                valinnanVaiheJarjestysluku2, kokeet2);

        List<ValintaperusteetTyyppi> valintaperusteet = new ArrayList<ValintaperusteetTyyppi>();
        valintaperusteet.add(valintaperusteet1);
        valintaperusteet.add(valintaperusteet2);

        assertNull(valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid));
        valintakoelaskentaSuorittajaService.laske(hakemus, valintaperusteet);
        ValintakoeOsallistuminen osallistuminen = valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid);
        assertNotNull(osallistuminen);
    }

}
