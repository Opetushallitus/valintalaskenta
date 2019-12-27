package fi.vm.sade.valintalaskenta.tulos.dao;

import com.lordofthejars.nosqlunit.annotation.UsingDataSet;
import com.lordofthejars.nosqlunit.core.LoadStrategyEnum;
import com.lordofthejars.nosqlunit.mongodb.MongoDbRule;
import fi.vm.sade.auditlog.User;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import static com.lordofthejars.nosqlunit.mongodb.MongoDbRule.MongoDbRuleBuilder.newMongoDbRule;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

/**
* Created by kjsaila on 11/03/15.
*/
@ContextConfiguration(locations = "classpath:application-context-test.xml")
@RunWith(SpringJUnit4ClassRunner.class)
@UsingDataSet
public class ValinnanvaiheDaoTest {

    @Autowired
    ValinnanvaiheDAO valinnanvaiheDAO;

    @Autowired
    private ApplicationContext applicationContext;

    @Rule
    public MongoDbRule mongoDbRule = newMongoDbRule().defaultSpringMongoDb("test");

    private final User auditUser = null;

    @Test
    @UsingDataSet(locations = "valinnanvaiheJono.json", loadStrategy = LoadStrategyEnum.CLEAN_INSERT)
    public void testHaeTuloksetValinnantapajonolle() {
        {
            Valinnanvaihe vaihe = valinnanvaiheDAO.findByValintatapajonoOid("jono1", auditUser);

            assertNotNull(vaihe);
            assertEquals("vaihe1", vaihe.getValinnanvaiheOid());
        }

    }
}
