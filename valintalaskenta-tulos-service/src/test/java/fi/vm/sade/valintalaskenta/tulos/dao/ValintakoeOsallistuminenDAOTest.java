package fi.vm.sade.valintalaskenta.tulos.dao;

import com.lordofthejars.nosqlunit.annotation.UsingDataSet;
import com.lordofthejars.nosqlunit.core.LoadStrategyEnum;
import com.lordofthejars.nosqlunit.mongodb.MongoDbRule;
import fi.vm.sade.valintalaskenta.domain.valintakoe.*;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.List;

import static com.lordofthejars.nosqlunit.mongodb.MongoDbRule.MongoDbRuleBuilder.newMongoDbRule;
import static org.junit.Assert.assertEquals;

/**
 * User: wuoti
 * Date: 28.8.2013
 * Time: 15.47
 */
@ContextConfiguration(locations = "classpath:application-context-test.xml")
@RunWith(SpringJUnit4ClassRunner.class)
@UsingDataSet
public class ValintakoeOsallistuminenDAOTest {

    @Autowired
    private ValintakoeOsallistuminenDAO valintakoeOsallistuminenDAO;

    @Autowired
    private ApplicationContext applicationContext;

    @Rule
    public MongoDbRule mongoDbRule = newMongoDbRule().defaultSpringMongoDb("test");

    @Test
    @UsingDataSet(locations = "valintakoeOsallistuminenDAOInitialData.json", loadStrategy = LoadStrategyEnum.CLEAN_INSERT)
    public void testFindByHakuAndOsallistuminen() {
        final String hakuOid = "hakuOid1";
        final Osallistuminen osallistuminen = Osallistuminen.VIRHE;

        final String hakemusOid = "hakemusOid1";
        List<ValintakoeOsallistuminen> osallistumiset =
                valintakoeOsallistuminenDAO.findByHakuAndOsallistuminen(hakuOid, osallistuminen);
        assertEquals(1, osallistumiset.size());
        ValintakoeOsallistuminen vko = osallistumiset.get(0);
        assertEquals(vko.getHakemusOid(), hakemusOid);
        assertEquals(1, vko.getHakutoiveet().size());
        Hakutoive hakutoive = vko.getHakutoiveet().get(0);
        assertEquals(2, hakutoive.getValinnanVaiheet().size());
        ValintakoeValinnanvaihe vaihe = hakutoive.getValinnanVaiheet().get(0);
        assertEquals(1, vaihe.getValintakokeet().size());
        Valintakoe koe = vaihe.getValintakokeet().get(0);
        assertEquals(koe.getOsallistuminenTulos().getOsallistuminen(), osallistuminen);
    }

}
