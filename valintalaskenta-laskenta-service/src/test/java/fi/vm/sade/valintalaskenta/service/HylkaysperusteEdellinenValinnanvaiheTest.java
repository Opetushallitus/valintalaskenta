package fi.vm.sade.valintalaskenta.service;

import java.util.List;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import scala.actors.threadpool.Arrays;
import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.valintalaskenta.ValintalaskentaService;
import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteetTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintatapajonoJarjestyskriteereillaTyyppi;
import fi.vm.sade.valintalaskenta.domain.Hakukohde;
import fi.vm.sade.valintalaskenta.domain.JarjestyskriteerituloksenTila;
import fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.resource.HakukohdeResource;
import fi.vm.sade.valintalaskenta.service.util.ValintalaskentaServiceUtil;

/**
 * 
 * @author Jussi Jartamo
 * 
 * 
 * 
 */
@ContextConfiguration(locations = "classpath:test-context.xml")
@RunWith(SpringJUnit4ClassRunner.class)
public class HylkaysperusteEdellinenValinnanvaiheTest {

    private static final Logger LOG = LoggerFactory.getLogger(HylkaysperusteEdellinenValinnanvaiheTest.class);

    @Autowired
    private ValintalaskentaService valintalaskentaService;

    @Autowired
    private HakukohdeResource hakukohdeResource;

    @SuppressWarnings("unchecked")
    @Test
    public void testVersiohallinnatJarjestysnumerolla() {
        String hakemusoid1 = "hakemusoid1";
        String hakemusoid2 = "hakemusoid2";
        String hakukohdeoid = "hakukohdeoid-" + System.currentTimeMillis();
        String valinnanvaiheoid = "valinnanvaiheoid-" + System.currentTimeMillis();

        // valintalaskenta jarjestysluvulla 1
        // hakemusoid2 epäonnistuu
        Integer jarjestysluku1 = 1;
        HakemusTyyppi[] hakemukset1 = new HakemusTyyppi[] {
                ValintalaskentaServiceUtil.createHakemus(hakemusoid1, hakukohdeoid, "matematiikka", "aidinkieli"),
                ValintalaskentaServiceUtil.createHakemus(hakemusoid2, hakukohdeoid, "matematiikka") };

        ValintaperusteetTyyppi valintaperusteet1 = ValintalaskentaServiceUtil.createValintaperusteet(hakukohdeoid,
                valinnanvaiheoid, jarjestysluku1);
        ValintatapajonoJarjestyskriteereillaTyyppi jono1 = ValintalaskentaServiceUtil.createValintatapajono("jonooid0");
        valintaperusteet1.getValintatapajonot().add(jono1);
        jono1.getJarjestyskriteerit().add(ValintalaskentaServiceUtil.createJarjestyskriteeri());

        jono1.getJarjestyskriteerit().get(0)
                .setFunktiokutsu(ValintalaskentaServiceUtil.createSummaFunktio("matematiikka", "aidinkieli"));
        valintalaskentaService.laske(hakukohdeoid, jarjestysluku1, Arrays.asList(hakemukset1),
                Arrays.asList(new ValintaperusteetTyyppi[] { valintaperusteet1 }));
        // valintalaskenta jarjestysluvulla 2
        // hakemusoid2 epäonnistuu koska se epäonnistui ensimmäisessä vaiheessa
        Integer jarjestysluku2 = 2;
        HakemusTyyppi[] hakemukset2 = new HakemusTyyppi[] {
                ValintalaskentaServiceUtil.createHakemus(hakemusoid1, hakukohdeoid, "aidinkieli"),
                ValintalaskentaServiceUtil.createHakemus(hakemusoid2, hakukohdeoid, "matematiikka", "aidinkieli") };
        ValintaperusteetTyyppi valintaperusteet2 = ValintalaskentaServiceUtil.createValintaperusteet(hakukohdeoid,
                valinnanvaiheoid, jarjestysluku2);
        ValintatapajonoJarjestyskriteereillaTyyppi jono2 = ValintalaskentaServiceUtil.createValintatapajono("jonooid0");
        valintaperusteet2.getValintatapajonot().add(jono2);
        jono2.getJarjestyskriteerit().add(ValintalaskentaServiceUtil.createJarjestyskriteeri());
        jono2.getJarjestyskriteerit().get(0)
                .setFunktiokutsu(ValintalaskentaServiceUtil.createSummaFunktio("matematiikka", "aidinkieli"));
        valintalaskentaService.laske(hakukohdeoid, jarjestysluku2, Arrays.asList(hakemukset2),
                Arrays.asList(new ValintaperusteetTyyppi[] { valintaperusteet2 }));
        List<Hakukohde> hakukohteet = hakukohdeResource.hakukohde(hakukohdeoid);
        Assert.assertTrue("Pitäisi olla kaksi hakukohdetta ('hakukohdeoid',1) ja ('hakukohdeoid',2)",
                hakukohteet != null && hakukohteet.size() == 2);
        LOG.info("Hakukohteita {} kpl niin kuin pitääkin!", hakukohteet.size());
        for (Hakukohde hakukohde : hakukohteet) {
            for (Jarjestyskriteeritulos tulos : hakukohde.getValinnanvaihe().getValintatapajono().get(0)
                    .getJarjestyskriteeritulokset()) {
                if (hakemusoid1.equals(tulos.getHakemusoid())) {
                    if (jarjestysluku1.equals(hakukohde.getValinnanvaihe().getJarjestysnumero())) {
                        Assert.assertTrue("Ensimmäisessä vaiheessa ensimmäinen hakemus pitäisi olla hyväksyttävissä!",
                                JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA.equals(tulos.getTila()));
                        LOG.info("Tulos järjestysluvulla {} hakemukselle {} on {}", new Object[] {
                                hakukohde.getValinnanvaihe().getJarjestysnumero(), hakemusoid1, tulos.getTila() });
                    } else if (jarjestysluku2.equals(hakukohde.getValinnanvaihe().getJarjestysnumero())) {
                        Assert.assertTrue("Toisessa vaiheessa ensimmäinen hakemus pitäisi olla hylätty!",
                                JarjestyskriteerituloksenTila.HYLATTY.equals(tulos.getTila()));
                        LOG.info("Tulos järjestysluvulla {} hakemukselle {} on {}", new Object[] {
                                hakukohde.getValinnanvaihe().getJarjestysnumero(), hakemusoid1, tulos.getTila() });
                    } else {
                        Assert.fail("Järjestysluku ei vastannut kumpaakaan syötetyn valinnanvaiheen järjestyslukua!");
                    }
                } else if (hakemusoid2.equals(tulos.getHakemusoid())) {
                    if (jarjestysluku1.equals(hakukohde.getValinnanvaihe().getJarjestysnumero())) {
                        Assert.assertTrue("Ensimmäisessä vaiheessa toinen hakemus pitäisi olla hylätty!",
                                JarjestyskriteerituloksenTila.HYLATTY.equals(tulos.getTila()));
                        LOG.info("Tulos järjestysluvulla {} hakemukselle {} on {}", new Object[] {
                                hakukohde.getValinnanvaihe().getJarjestysnumero(), hakemusoid2, tulos.getTila() });
                    } else if (jarjestysluku2.equals(hakukohde.getValinnanvaihe().getJarjestysnumero())) {
                        Assert.assertTrue("Toisessa vaiheessa toinen hakemus pitäisi olla hylätty!",
                                JarjestyskriteerituloksenTila.HYLATTY.equals(tulos.getTila()));
                        LOG.info("Tulos järjestysluvulla {} hakemukselle {} on {}", new Object[] {
                                hakukohde.getValinnanvaihe().getJarjestysnumero(), hakemusoid2, tulos.getTila() });
                    } else {
                        Assert.fail("Järjestysluku ei vastannut kumpaakaan syötetyn valinnanvaiheen järjestyslukua!");
                    }
                } else {
                    Assert.fail("Tulos ei vastannut kumpaakaan syötettyä hakemusta!");
                }
            }
        }
    }
}
