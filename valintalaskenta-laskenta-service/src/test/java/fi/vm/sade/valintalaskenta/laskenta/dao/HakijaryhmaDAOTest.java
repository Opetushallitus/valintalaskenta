package fi.vm.sade.valintalaskenta.laskenta.dao;

import com.lordofthejars.nosqlunit.annotation.UsingDataSet;
import com.lordofthejars.nosqlunit.core.LoadStrategyEnum;
import com.lordofthejars.nosqlunit.mongodb.MongoDbRule;
import fi.vm.sade.valintalaskenta.domain.valinta.Hakijaryhma;
import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import org.bson.types.ObjectId;
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

import java.util.List;

import static com.lordofthejars.nosqlunit.mongodb.MongoDbRule.MongoDbRuleBuilder.newMongoDbRule;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;


@ContextConfiguration(locations = "classpath:application-context-test.xml")
@RunWith(SpringJUnit4ClassRunner.class)
@TestExecutionListeners(listeners = {DependencyInjectionTestExecutionListener.class,
        DirtiesContextTestExecutionListener.class})
public class HakijaryhmaDAOTest {

    @Rule
    public MongoDbRule mongoDbRule = newMongoDbRule().defaultSpringMongoDb("test");

    @Autowired
    private ApplicationContext applicationContext;

    @Autowired
    private HakijaryhmaDAO hakijaryhmaDAO;

    @Test
    @UsingDataSet(locations = "hakijaryhmaMigrationTestData.json", loadStrategy = LoadStrategyEnum.CLEAN_INSERT)
    public void testMigrationOfHakijaryma() throws InterruptedException {
        Hakijaryhma hakijaryhma = hakijaryhmaDAO.haeHakijaryhma("vanhaHakijaryhmaOid").get();
        List<Jonosija> jonosijat = hakijaryhma.getJonosijat();
        List<ObjectId> jonosijaIdt = hakijaryhma.getJonosijaIdt();
        assertNotNull(jonosijat);
        assertNotNull(jonosijaIdt);
        assertEquals(3, jonosijat.size());
        assertEquals(3, jonosijaIdt.size());
        for (int i = 0; i < Math.max(jonosijat.size(), jonosijaIdt.size()); i++) {
            assertEquals(jonosijaIdt.get(i), jonosijat.get(i).getId());
        }
    }

    @Test
    @UsingDataSet(locations = "hakijaryhmaMigrationTestData.json", loadStrategy = LoadStrategyEnum.CLEAN_INSERT)
    public void testLoadingOfMigratedHakijaryhma() throws InterruptedException {
        Hakijaryhma hakijaryhma = hakijaryhmaDAO.haeHakijaryhma("migroituHakijaryhmaOid").get();
        List<Jonosija> jonosijat = hakijaryhma.getJonosijat();
        List<ObjectId> jonosijaIdt = hakijaryhma.getJonosijaIdt();
        assertNotNull(jonosijat);
        assertNotNull(jonosijaIdt);
        assertEquals(3, jonosijat.size());
        assertEquals(3, jonosijaIdt.size());
        for (int i = 0; i < Math.max(jonosijat.size(), jonosijaIdt.size()); i++) {
            assertEquals(jonosijaIdt.get(i), jonosijat.get(i).getId());
        }
    }
}
