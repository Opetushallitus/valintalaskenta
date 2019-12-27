package fi.vm.sade.valintalaskenta.tulos.dao;

import com.lordofthejars.nosqlunit.annotation.UsingDataSet;
import com.lordofthejars.nosqlunit.core.LoadStrategyEnum;
import com.lordofthejars.nosqlunit.mongodb.MongoDbRule;
import fi.vm.sade.auditlog.User;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;
import org.bson.types.ObjectId;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.Collections;
import java.util.List;

import static com.lordofthejars.nosqlunit.mongodb.MongoDbRule.MongoDbRuleBuilder.newMongoDbRule;
import static org.junit.Assert.assertEquals;

/**
 * User: wuoti
 * Date: 20.8.2013
 * Time: 19.30
 */
@ContextConfiguration(locations = "classpath:application-context-test.xml")
@RunWith(SpringJUnit4ClassRunner.class)
@UsingDataSet
public class JonosijaHistoriaTulosDAOTest {
    @Autowired
    private JarjestyskriteerihistoriaDAO jonosijaHistoriaTulosDAO;

    @Autowired
    private ApplicationContext applicationContext;

    @Rule
    public MongoDbRule mongoDbRule = newMongoDbRule().defaultSpringMongoDb("test");

    private final User auditUser = null;

    @Test
    @UsingDataSet(locations = "jonosijaHistoriaDAOInitialData.json", loadStrategy = LoadStrategyEnum.CLEAN_INSERT)
    public void testFindByValintapajonoOidHakemusOidAndJarjestyskriteeriPrioriteetti() {
        final String valintatapajonoOid = "valintatapajonoOid1";
        final String hakemusOid = "hakemusOid1";
        List<Jarjestyskriteerihistoria> jonosijaHistoriat =
                jonosijaHistoriaTulosDAO.findByValintatapajonoAndHakemusOid(valintatapajonoOid, hakemusOid, auditUser);

        assertEquals(2, jonosijaHistoriat.size());
        Collections.sort(jonosijaHistoriat, (o1, o2) -> o1.getHistoria().compareTo(o2.getHistoria()));

        assertEquals("historia1", jonosijaHistoriat.get(0).getHistoria());
        assertEquals("historia2", jonosijaHistoriat.get(1).getHistoria());
    }

    @Test
    @UsingDataSet(locations = "jonosijaHistoria.json", loadStrategy = LoadStrategyEnum.CLEAN_INSERT)
    public void testJonosijaHistoriat() {
        final String valintatapajonoOid = "1410335755064-1436990924193196531";
        final String hakemusOid = "1.2.246.562.11.00000876962";
        List<Jarjestyskriteerihistoria> jonosijaHistoriat =
                jonosijaHistoriaTulosDAO.findByValintatapajonoAndHakemusOid(valintatapajonoOid, hakemusOid, auditUser);

        assertEquals(3, jonosijaHistoriat.size());
        Collections.sort(jonosijaHistoriat, (o1, o2) -> o1.getHistoria().compareTo(o2.getHistoria()));

        assertEquals(new ObjectId("541bef0ae4b0e1d22689e606"), jonosijaHistoriat.get(0).getId());
        assertEquals(new ObjectId("541bef0ae4b0e1d22689e60a"), jonosijaHistoriat.get(1).getId());
        assertEquals(new ObjectId("541bef0ae4b0e1d22689e60e"), jonosijaHistoriat.get(2).getId());
    }
}
