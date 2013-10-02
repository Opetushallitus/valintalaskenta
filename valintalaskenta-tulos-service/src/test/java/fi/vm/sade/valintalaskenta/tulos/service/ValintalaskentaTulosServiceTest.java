package fi.vm.sade.valintalaskenta.tulos.service;

import com.lordofthejars.nosqlunit.annotation.UsingDataSet;
import com.lordofthejars.nosqlunit.core.LoadStrategyEnum;
import com.lordofthejars.nosqlunit.mongodb.MongoDbRule;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.HakutoiveDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeOsallistuminenDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
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
    public void haeValintakoeOsallistumisetByOidTest() {
        List<ValintakoeOsallistuminen> kaikki = valintalaskentaTulosService.haeValintakoeOsallistumiset("oid1");
        assertEquals(1, kaikki.size());
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

    @Test
    @UsingDataSet(locations = "testHaeValintakoevirheetHaulle.json", loadStrategy = LoadStrategyEnum.CLEAN_INSERT)
    public void testHaeValintakoevirheetHaulle() {
        final String hakuOid = "hakuOid1";
        List<ValintakoeOsallistuminenDTO> osallistumiset = valintalaskentaTulosService.haeValintakoevirheetHaulle(hakuOid);
        assertEquals(1, osallistumiset.size());
        ValintakoeOsallistuminenDTO vko = osallistumiset.get(0);
        assertEquals(hakuOid, vko.getHakuOid());
        assertEquals(1, vko.getHakutoiveet().size());

        HakutoiveDTO ht = vko.getHakutoiveet().get(0);
        assertEquals(1, ht.getValinnanVaiheet().size());

        ValintakoeValinnanvaiheDTO vv = ht.getValinnanVaiheet().get(0);
        assertEquals(1, vv.getValintakokeet().size());

        ValintakoeDTO vk = vv.getValintakokeet().get(0);
        assertEquals(Osallistuminen.VIRHE, vk.getOsallistuminenTulos().getOsallistuminen());
    }
}
