package fi.vm.sade.valintalaskenta.tulos.dao;

import com.lordofthejars.nosqlunit.annotation.UsingDataSet;
import com.lordofthejars.nosqlunit.core.LoadStrategyEnum;
import com.lordofthejars.nosqlunit.mongodb.MongoDbRule;
import fi.vm.sade.valintalaskenta.domain.JonosijaHistoria;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.Collections;
import java.util.Comparator;
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
    private JonosijaHistoriaTulosDAO jonosijaHistoriaTulosDAO;

    @Autowired
    private ApplicationContext applicationContext;

    @Rule
    public MongoDbRule mongoDbRule = newMongoDbRule().defaultSpringMongoDb("test");

    @Test
    @UsingDataSet(locations = "jonosijaHistoriaDAOInitialData.json", loadStrategy = LoadStrategyEnum.CLEAN_INSERT)
    public void testFindByValintapajonoOidHakemusOidAndJarjestyskriteeriPrioriteetti() {
        final String valintatapajonoOid = "valintatapajonooid1";
        final String hakemusOid = "hakemusoid1";
        List<JonosijaHistoria> jonosijaHistoriat =
                jonosijaHistoriaTulosDAO.findByValintatapajonoAndVersioAndHakemusOid(valintatapajonoOid, hakemusOid);

        assertEquals(2, jonosijaHistoriat.size());
        Collections.sort(jonosijaHistoriat, new Comparator<JonosijaHistoria>() {
            @Override
            public int compare(JonosijaHistoria o1, JonosijaHistoria o2) {
                return o1.getHistoria().compareTo(o2.getHistoria());
            }
        });

        assertEquals("historia3", jonosijaHistoriat.get(0).getHistoria());
        assertEquals("historia4", jonosijaHistoriat.get(1).getHistoria());
    }
}
