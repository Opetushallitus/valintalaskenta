package fi.vm.sade.valintalaskenta.service;

import java.io.IOException;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import scala.actors.threadpool.Arrays;
import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.valintalaskenta.ValintalaskentaService;
import fi.vm.sade.service.valintaperusteet.schema.FunktiokutsuTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.JarjestyskriteeriTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteetTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintatapajonoJarjestyskriteereillaTyyppi;
import fi.vm.sade.valintalaskenta.domain.Hakukohde;
import fi.vm.sade.valintalaskenta.resource.HakuResource;
import fi.vm.sade.valintalaskenta.service.util.ValintalaskentaServiceUtil;

/**
 * 
 * @author Jussi Jartamo
 * 
 *         Creates Temporary Embedded MongoDB in localhost:37400 Database will
 *         vanish when test ends.
 * 
 *         Can also be run with Maven profile dev Assumes local MongoDB can be
 *         found in localhost:37200
 */
@ContextConfiguration(locations = "classpath:test-context.xml")
@RunWith(SpringJUnit4ClassRunner.class)
public class PassThroughTest {

    @Autowired
    private ValintalaskentaService valintalaskentaService;

    @Autowired
    private HakuResource hakuResource;

    @SuppressWarnings("unchecked")
    @Test
    public void testHaku() throws IOException {
        final int initialSize = hakuResource.haku().size();
        String hakukohdeoid = "hakukohdeoid" + initialSize + "-" + System.currentTimeMillis();
        String valinnanvaiheoid = "valinnanvaiheoid-" + System.currentTimeMillis();
        HakemusTyyppi[] hakemukset = new HakemusTyyppi[] {
                ValintalaskentaServiceUtil.createHakemus("hakemusoid1", hakukohdeoid, "matematiikka", "aidinkieli"),
                ValintalaskentaServiceUtil.createHakemus("hakemusoid2", hakukohdeoid, "matematiikka", "aidinkieli") };
        ValintaperusteetTyyppi valintaperusteet = ValintalaskentaServiceUtil.createValintaperusteet(hakukohdeoid,
                valinnanvaiheoid, 1);
        ValintatapajonoJarjestyskriteereillaTyyppi jono = ValintalaskentaServiceUtil.createValintatapajono("jonooid0");
        valintaperusteet.getValintatapajonot().add(jono);
        JarjestyskriteeriTyyppi jarjestyskriteeri0 = ValintalaskentaServiceUtil.createJarjestyskriteeri();
        jono.getJarjestyskriteerit().add(jarjestyskriteeri0);
        FunktiokutsuTyyppi funktiokutsu = ValintalaskentaServiceUtil.createSummaFunktio("matematiikka", "aidinkieli");
        jarjestyskriteeri0.setFunktiokutsu(funktiokutsu);
        valintalaskentaService.laske(Arrays.asList(hakemukset),
                Arrays.asList(new ValintaperusteetTyyppi[] { valintaperusteet }));
        List<Hakukohde> hakukohteet = hakuResource.haku();
        Assert.assertTrue("Hakukohteet ei lisääntynyt yhdellä!", hakukohteet.size() == initialSize + 1);
        for (Hakukohde haku : hakukohteet) {
            if (hakukohdeoid.equals(haku.getOid())) {
                return;
            }
        }
        Assert.fail("MongoDB:n palauttamissa hakukohteissa ei ollut lisättyä Hakukohdetta!");
    }
}
