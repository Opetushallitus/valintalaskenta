package fi.vm.sade.valintalaskenta.tulos.service;

import static com.lordofthejars.nosqlunit.mongodb.MongoDbRule.MongoDbRuleBuilder.newMongoDbRule;
import static org.junit.Assert.assertEquals;

import java.util.List;

import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.lordofthejars.nosqlunit.annotation.UsingDataSet;
import com.lordofthejars.nosqlunit.core.LoadStrategyEnum;
import com.lordofthejars.nosqlunit.mongodb.MongoDbRule;

import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;

@ContextConfiguration(locations = "classpath:application-context-test.xml")
@RunWith(SpringJUnit4ClassRunner.class)
@UsingDataSet
public class ValintalaskentaTulosServiceTest {

    @Autowired
    private ValintalaskentaTulosService valintalaskentaTulosService;

    @Autowired
    private ApplicationContext applicationContext;

    @Rule
    public MongoDbRule mongoDbRule = newMongoDbRule().defaultSpringMongoDb("test");

    @Test
    @UsingDataSet(locations = "initialData.json", loadStrategy = LoadStrategyEnum.CLEAN_INSERT)
    public void haeValintakoeOsallistumisetTest() {
        List<ValintakoeOsallistuminen> kaikki = valintalaskentaTulosService.haeValintakoeOsallistumiset();
        assertEquals(5, kaikki.size());
    }

    @Test
    @UsingDataSet(locations = "initialData.json", loadStrategy = LoadStrategyEnum.CLEAN_INSERT)
    public void haeValintakoeOsallistumisetByOidTest() {
        List<ValintakoeOsallistuminen> kaikki = valintalaskentaTulosService.haeValintakoeOsallistumiset("oid1");
        assertEquals(2, kaikki.size());
        assertEquals("oid1", kaikki.get(0).getHakijaOid());
    }

    @Test
    @UsingDataSet(locations = "initialData.json", loadStrategy = LoadStrategyEnum.CLEAN_INSERT)
    public void haeValintakoeOsallistumisetByHakutoiveTest() {
        List<ValintakoeOsallistuminen> kaikki = valintalaskentaTulosService
                .haeValintakoeOsallistumisetByHakutoive("oid1");
        assertEquals(2, kaikki.size());
        kaikki = valintalaskentaTulosService.haeValintakoeOsallistumisetByHakutoive("oid2");
        assertEquals(1, kaikki.size());
    }
}
