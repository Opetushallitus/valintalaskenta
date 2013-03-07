package fi.vm.sade.valintalaskenta.service;

import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.valintalaskenta.ValintalaskentaService;
import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteetTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintatapajonoJarjestyskriteereillaTyyppi;
import fi.vm.sade.valintalaskenta.domain.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.resource.HakukohdeResource;
import fi.vm.sade.valintalaskenta.service.util.ValintalaskentaServiceUtil;
import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import scala.actors.threadpool.Arrays;

import java.util.List;

/**
 * 
 * @author Jussi Jartamo
 * 
 *         Tekee laskennan muutamia kertoja samalle Valinnanvaiheelle ja
 *         verifioi että REST-kysely palauttaa uusimman version
 */
@ContextConfiguration(locations = "classpath:test-context.xml")
@RunWith(SpringJUnit4ClassRunner.class)
public class HakujenVersiointiTest {

    private static final Logger LOG = LoggerFactory.getLogger(HakujenVersiointiTest.class);

    @Autowired
    private ValintalaskentaService valintalaskentaService;

    @Autowired
    private HakukohdeResource hakukohdeResource;

    @SuppressWarnings("unchecked")
    @Test
    @Ignore
    public void testVersionti() {
        String hakemusoid = "hakemusoid1";
        String hakukohdeoid = "hakukohdeoid-" + System.currentTimeMillis();
        String valinnanvaiheoid = "valinnanvaiheoid-" + System.currentTimeMillis();
        Integer jarjestysluku = 1;
        String[] doubleValues = new String[] { "4.0", "5.0", "6.0", "7.0" };
        for (String doubleValue : doubleValues) {

            HakemusTyyppi[] hakemukset = new HakemusTyyppi[] { ValintalaskentaServiceUtil.createHakemusParilla(
                    hakemusoid, hakukohdeoid, new String[] { "matematiikka", doubleValue }, new String[] {
                            "aidinkieli", doubleValue }) };

            ValintaperusteetTyyppi valintaperusteet = ValintalaskentaServiceUtil.createValintaperusteet(hakukohdeoid,
                    valinnanvaiheoid, jarjestysluku);
            ValintatapajonoJarjestyskriteereillaTyyppi jono = ValintalaskentaServiceUtil
                    .createValintatapajono("jonooid0");
            valintaperusteet.getValintatapajonot().add(jono);
            jono.getJarjestyskriteerit().add(ValintalaskentaServiceUtil.createJarjestyskriteeri());

            jono.getJarjestyskriteerit().get(0)
                    .setFunktiokutsu(ValintalaskentaServiceUtil.createSummaFunktio("matematiikka", "aidinkieli"));
            valintalaskentaService.laske(Arrays.asList(hakemukset), Arrays.asList(new ValintaperusteetTyyppi[]{valintaperusteet}));

            List<Valinnanvaihe> kohteet = hakukohdeResource.hakukohde(hakukohdeoid);
            Assert.assertTrue(
                    "Muutos ei luonut kohteita tai lisäsi kohteiden määrää! Jokainen versioiva muutos ylikirjoittaa edellisen muutoksen, joten jokaisessa vaiheessa Hakukohteita on tasan yksi!",
                    kohteet != null && kohteet.size() == 1);
            Double summa = kohteet.get(0).getValintatapajono().get(0).getJarjestyskriteeritulokset().get(0).getArvo();
            Double tekija = new Double(doubleValue);
            Assert.assertTrue("Jokaisella kierroksella palautuvan summan tulisi olla tekijöiden summa!",
                    summa.equals(tekija + tekija));
        }
    }
}
