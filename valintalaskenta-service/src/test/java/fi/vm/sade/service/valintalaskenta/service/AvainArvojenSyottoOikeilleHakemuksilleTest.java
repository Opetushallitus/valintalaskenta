package fi.vm.sade.service.valintalaskenta.service;

import java.util.List;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import scala.actors.threadpool.Arrays;
import fi.vm.sade.service.hakemus.schema.AvainArvoTyyppi;
import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.valintalaskenta.ValintalaskentaService;
import fi.vm.sade.service.valintalaskenta.resource.HakukohdeResource;
import fi.vm.sade.service.valintalaskenta.service.test.util.ValintalaskentaServiceUtil;
import fi.vm.sade.service.valintaperusteet.algoritmi.domain.Hakukohde;
import fi.vm.sade.service.valintaperusteet.algoritmi.domain.JarjestyskriteerituloksenTila;
import fi.vm.sade.service.valintaperusteet.algoritmi.domain.Jarjestyskriteeritulos;
import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteetTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintatapajonoJarjestyskriteereillaTyyppi;

/**
 * 
 * @author Jussi Jartamo
 * 
 *         Testaa että oikeat hakijat saa oikeat avainarvoparit järjestelmän
 *         sisäisesti
 * 
 */
@ContextConfiguration(locations = "classpath:test-context.xml")
@RunWith(SpringJUnit4ClassRunner.class)
public class AvainArvojenSyottoOikeilleHakemuksilleTest {

    @Autowired
    private ValintalaskentaService valintalaskentaService;

    @Autowired
    private HakukohdeResource hakukohdeResource;

    @SuppressWarnings("unchecked")
    @Test
    public void testAvainarvoparitKahdellaHakijalla() {
        String hakukohdeoid = "hakukohdeoid-" + System.currentTimeMillis();
        String hakemusoid1 = "hakemusoid1";
        String hakemusoid2 = "hakemusoid2";
        String hakemusoid3 = "hakemusoid3";
        HakemusTyyppi hakemus1, hakemus2, hakemus3;
        Integer jarjestysluku = 1;
        HakemusTyyppi[] hakemukset = new HakemusTyyppi[] {
                hakemus1 = ValintalaskentaServiceUtil.createHakemusParilla(hakemusoid1, hakukohdeoid, new String[] {
                        "matematiikka", "10.0" }, new String[] { "aidinkieli", "1.0" }),
                hakemus2 = ValintalaskentaServiceUtil.createHakemusParilla(hakemusoid2, hakukohdeoid, new String[] {
                        "matematiikka", "3.0" }),
                hakemus3 = ValintalaskentaServiceUtil.createHakemusParilla(hakemusoid3, hakukohdeoid, new String[] {
                        "aidinkieli", "7.0" }) };
        {
            AvainArvoTyyppi hakemus1pari1 = hakemus1.getAvainArvo().get(0);
            AvainArvoTyyppi hakemus1pari2 = hakemus1.getAvainArvo().get(1);
            Assert.assertTrue("Hakemusoid1 matematiikka=10.0, aidinkieli=1.0",
                    hakemus1pari1.getAvain().equals("matematiikka") && hakemus1pari1.getArvo().equals("10.0")
                            && hakemus1pari2.getAvain().equals("aidinkieli") && hakemus1pari2.getArvo().equals("1.0"));
        }
        {
            AvainArvoTyyppi hakemus2pari1 = hakemus2.getAvainArvo().get(0);
            Assert.assertTrue("Hakemusoid2 matematiikka=3.0", hakemus2pari1.getAvain().equals("matematiikka")
                    && hakemus2pari1.getArvo().equals("3.0"));
        }
        {
            AvainArvoTyyppi hakemus3pari1 = hakemus3.getAvainArvo().get(0);
            Assert.assertTrue("Hakemusoid3 aidinkieli=7.0", hakemus3pari1.getAvain().equals("aidinkieli")
                    && hakemus3pari1.getArvo().equals("7.0"));
        }
        ValintaperusteetTyyppi valintaperusteet = ValintalaskentaServiceUtil.createValintaperusteet(hakukohdeoid,
                jarjestysluku);
        ValintatapajonoJarjestyskriteereillaTyyppi jono1 = ValintalaskentaServiceUtil.createValintatapajono("jonooid0");
        valintaperusteet.getValintatapajonot().add(jono1);
        jono1.getJarjestyskriteerit().add(ValintalaskentaServiceUtil.createJarjestyskriteeri());

        jono1.getJarjestyskriteerit().get(0)
                .setFunktiokutsu(ValintalaskentaServiceUtil.createSummaFunktio("matematiikka", "aidinkieli"));
        valintalaskentaService.laske(hakukohdeoid, jarjestysluku, Arrays.asList(hakemukset),
                Arrays.asList(new ValintaperusteetTyyppi[] { valintaperusteet }));

        List<Hakukohde> hakukohde = hakukohdeResource.hakukohde(hakukohdeoid);
        Assert.assertTrue("Yksi hakukohde ja yksi jono!", hakukohde != null && hakukohde.size() == 1
                && hakukohde.get(0).getValinnanvaihe().getValintatapajono().size() == 1);
        for (Jarjestyskriteeritulos tulos : hakukohde.get(0).getValinnanvaihe().getValintatapajono().get(0)
                .getJarjestyskriteeritulokset()) {
            if (hakemusoid1.equals(tulos.getHakemusoid())) {
                Assert.assertTrue("Hakemus1 hyväksyttävissä",
                        JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA.equals(tulos.getTila()));
            } else if (hakemusoid2.equals(tulos.getHakemusoid())) {
                Assert.assertTrue("Hakemus2 hylätty", JarjestyskriteerituloksenTila.HYLATTY.equals(tulos.getTila()));
            } else if (hakemusoid3.equals(tulos.getHakemusoid())) {
                Assert.assertTrue("Hakemus3 hylätty", JarjestyskriteerituloksenTila.HYLATTY.equals(tulos.getTila()));
            } else {
                Assert.fail("Hakemusoid ei vastannut syötettyjä!");
            }

        }
    }
}
