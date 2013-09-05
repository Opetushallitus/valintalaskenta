package fi.vm.sade.valintalaskenta.laskenta.dao;

import com.lordofthejars.nosqlunit.annotation.UsingDataSet;
import com.lordofthejars.nosqlunit.core.LoadStrategyEnum;
import com.lordofthejars.nosqlunit.mongodb.MongoDbRule;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
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

import static com.lordofthejars.nosqlunit.mongodb.MongoDbRule.MongoDbRuleBuilder.newMongoDbRule;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;


/**
 * User: wuoti
 * Date: 4.9.2013
 * Time: 12.27
 */
@ContextConfiguration(locations = "classpath:application-context-test.xml")
@RunWith(SpringJUnit4ClassRunner.class)
@TestExecutionListeners(listeners = {DependencyInjectionTestExecutionListener.class,
        DirtiesContextTestExecutionListener.class})
public class ValinnanvaiheDAOTest {

    @Rule
    public MongoDbRule mongoDbRule = newMongoDbRule().defaultSpringMongoDb("test");

    @Autowired
    private ApplicationContext applicationContext;

    @Autowired
    private ValinnanvaiheDAO valinnanvaiheDAO;

    @Test
    @UsingDataSet(locations = "valinnanvaiheDAOTestInitialData.json", loadStrategy = LoadStrategyEnum.CLEAN_INSERT)
    public void testHaeEdellinenValinnanvaihe() throws InterruptedException {
        final String hakuOid = "hakuOid1";
        final String hakukohdeOid = "hakukohdeOid1";
        final int jarjestysnumero = 3;


        final String edellinenValinnanvaiheOid = "valinnanvaiheOid2";
        final int edellinenValinnanvaiheJarjestysnumero = 2;

        Valinnanvaihe valinnanvaihe = valinnanvaiheDAO.haeEdellinenValinnanvaihe(hakuOid, hakukohdeOid, jarjestysnumero);
        assertEquals(hakuOid, valinnanvaihe.getHakuOid());
        assertEquals(hakukohdeOid, valinnanvaihe.getHakukohdeOid());
        assertEquals(edellinenValinnanvaiheOid, valinnanvaihe.getValinnanvaiheOid());
        assertEquals(edellinenValinnanvaiheJarjestysnumero, valinnanvaihe.getJarjestysnumero());

        assertNull(valinnanvaiheDAO.haeEdellinenValinnanvaihe(hakuOid, hakukohdeOid, 1));
    }

    @Test
    @UsingDataSet(locations = "valinnanvaiheDAOTestInitialData.json", loadStrategy = LoadStrategyEnum.CLEAN_INSERT)
    public void testHaeValinnanvaihe() throws InterruptedException {
        final String valinnanvaiheOid = "valinnanvaiheOid2";

        Valinnanvaihe valinnanvaihe = valinnanvaiheDAO.haeValinnanvaihe(valinnanvaiheOid);
        assertEquals(valinnanvaiheOid, valinnanvaihe.getValinnanvaiheOid());
    }

}
