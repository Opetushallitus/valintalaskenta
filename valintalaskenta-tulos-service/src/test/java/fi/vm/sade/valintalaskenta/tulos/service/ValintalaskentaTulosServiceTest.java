package fi.vm.sade.valintalaskenta.tulos.service;

import com.lordofthejars.nosqlunit.annotation.UsingDataSet;
import com.lordofthejars.nosqlunit.core.LoadStrategyEnum;
import com.lordofthejars.nosqlunit.mongodb.MongoDbRule;
import fi.vm.sade.valintalaskenta.domain.dto.*;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.HakutoiveDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeOsallistuminenDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.ValintatietoValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.math.BigDecimal;
import java.util.Iterator;
import java.util.List;

import static com.lordofthejars.nosqlunit.mongodb.MongoDbRule.MongoDbRuleBuilder.newMongoDbRule;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

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
        ValintakoeOsallistuminen kaikki = valintalaskentaTulosService.haeValintakoeOsallistumiset("oid1");

        assertEquals("oid1", kaikki.getHakijaOid());
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

    @Test
    @UsingDataSet(locations = "testHaeTuloksetHakemukselle.json", loadStrategy = LoadStrategyEnum.CLEAN_INSERT)
    public void testHaeTuloksetHakemukselle() {
        {
            HakemusDTO hakemus = valintalaskentaTulosService.haeTuloksetHakemukselle("hakuOid1", "hakemusOid1");
            assertEquals(1, hakemus.getHakukohteet().size());
            HakukohdeDTO hakukohde = hakemus.getHakukohteet().get(0);
            assertEquals(1, hakukohde.getValinnanvaihe().size());
            ValinnanvaiheDTO vv = hakukohde.getValinnanvaihe().get(0);
            assertEquals(1, vv.getValintatapajonot().size());
            ValintatapajonoDTO valintatapajono = vv.getValintatapajonot().get(0);
            assertEquals(1, valintatapajono.getJonosijat().size());
            JonosijaDTO jonosija = valintatapajono.getJonosijat().get(0);
            assertEquals(2, jonosija.getJarjestyskriteerit().size());
            assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jonosija.getTuloksenTila());

            Iterator<JarjestyskriteeritulosDTO> i = jonosija.getJarjestyskriteerit().iterator();
            JarjestyskriteeritulosDTO kriteeri1 = i.next();
            assertEquals(0, kriteeri1.getPrioriteetti());
            assertEquals(new BigDecimal("100.0"), kriteeri1.getArvo());
            assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, kriteeri1.getTila());

            JarjestyskriteeritulosDTO kriteeri2 = i.next();
            assertEquals(1, kriteeri2.getPrioriteetti());
            assertEquals(new BigDecimal("5.0"), kriteeri2.getArvo());
            assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, kriteeri2.getTila());
        }

        {
            HakemusDTO hakemus = valintalaskentaTulosService.haeTuloksetHakemukselle("hakuOid1", "hakemusOid2");
            assertEquals(1, hakemus.getHakukohteet().size());
            HakukohdeDTO hakukohde = hakemus.getHakukohteet().get(0);
            assertEquals(1, hakukohde.getValinnanvaihe().size());
            ValinnanvaiheDTO vv = hakukohde.getValinnanvaihe().get(0);
            assertEquals(1, vv.getValintatapajonot().size());
            ValintatapajonoDTO valintatapajono = vv.getValintatapajonot().get(0);
            assertEquals(1, valintatapajono.getJonosijat().size());
            JonosijaDTO jonosija = valintatapajono.getJonosijat().get(0);
            assertEquals(2, jonosija.getJarjestyskriteerit().size());
            assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTY_HARKINNANVARAISESTI, jonosija.getTuloksenTila());

            Iterator<JarjestyskriteeritulosDTO> i = jonosija.getJarjestyskriteerit().iterator();
            JarjestyskriteeritulosDTO kriteeri1 = i.next();
            assertEquals(0, kriteeri1.getPrioriteetti());
            assertEquals(new BigDecimal("20.0"), kriteeri1.getArvo());
            assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTY_HARKINNANVARAISESTI, kriteeri1.getTila());

            JarjestyskriteeritulosDTO kriteeri2 = i.next();
            assertEquals(1, kriteeri2.getPrioriteetti());
            assertEquals(new BigDecimal("10.0"), kriteeri2.getArvo());
            assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, kriteeri2.getTila());
        }
    }

    @Test
    @UsingDataSet(locations = "valinnanvaiheTasasija.json", loadStrategy = LoadStrategyEnum.CLEAN_INSERT)
    public void testHaeTuloksetHakukohteelle() {
        {
            List<ValintatietoValinnanvaiheDTO> hakukohde = valintalaskentaTulosService.haeValinnanvaiheetHakukohteelle("hakukohde1");
            hakukohde.get(0).getValintatapajonot().get(0).getJonosijat().stream().filter(h -> h.getSukunimi().equals("Lahtinen")).forEach(h -> {
                assertEquals(1, h.getJonosija());
            });
            hakukohde.get(0).getValintatapajonot().get(0).getJonosijat().stream().filter(h -> !h.getSukunimi().equals("Lahtinen")).forEach(h -> {
                assertFalse(h.getJonosija() == 1);
            });
        }

    }
}
