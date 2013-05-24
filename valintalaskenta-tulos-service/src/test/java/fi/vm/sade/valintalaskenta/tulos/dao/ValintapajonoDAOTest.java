package fi.vm.sade.valintalaskenta.tulos.dao;

import com.google.code.morphia.Datastore;
import com.lordofthejars.nosqlunit.annotation.UsingDataSet;
import com.lordofthejars.nosqlunit.core.LoadStrategyEnum;
import com.lordofthejars.nosqlunit.mongodb.MongoDbRule;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import static com.lordofthejars.nosqlunit.mongodb.MongoDbRule.MongoDbRuleBuilder.newMongoDbRule;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

/**
 * User: wuoti
 * Date: 23.5.2013
 * Time: 14.57
 */
@ContextConfiguration(locations = "classpath:application-context-test.xml")
@RunWith(SpringJUnit4ClassRunner.class)
@UsingDataSet
public class ValintapajonoDAOTest {

    @Autowired
    private ValintatapajonoDAO valintatapajonoDAO;

    @Autowired
    private ApplicationContext applicationContext;

    @Rule
    public MongoDbRule mongoDbRule = newMongoDbRule().defaultSpringMongoDb("test");

    @Autowired
    private Datastore ds;

    @Test
    @UsingDataSet(locations = "initialData.json", loadStrategy = LoadStrategyEnum.CLEAN_INSERT)
    public void testFindByValintapajonoOidHakemusOidAndJarjestyskriteeriPrioriteetti() {
        final String valintatapajonoOid = "valintatapajonooid1";
        final String hakemusOid = "hakemusoid1";
        final Integer jarjestyskriteeriPrioriteetti = 0;
        final Integer jarjestyskriteeriPrioriteettiEiLoydy = 1;

        assertNull(valintatapajonoDAO.findByValintatapajonoOidHakemusOidAndJarjestyskriteeriPrioriteetti(
                valintatapajonoOid, hakemusOid, jarjestyskriteeriPrioriteettiEiLoydy));

        assertNotNull(valintatapajonoDAO.findByValintatapajonoOidHakemusOidAndJarjestyskriteeriPrioriteetti(
                valintatapajonoOid, hakemusOid, jarjestyskriteeriPrioriteetti));
    }

}
