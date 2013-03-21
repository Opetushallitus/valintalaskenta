package fi.vm.sade.valintalaskenta.laskenta.service;

import java.util.Arrays;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import fi.vm.sade.service.hakemus.schema.AvainArvoTyyppi;
import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.hakemus.schema.HakukohdeTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.JarjestyskriteeriTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteetTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintatapajonoJarjestyskriteereillaTyyppi;
import fi.vm.sade.valintalaskenta.dao.VersiohallintaHakukohdeDAO;
import fi.vm.sade.valintalaskenta.domain.VersiohallintaHakukohde;
import fi.vm.sade.valintalaskenta.domain.Versioituhakukohde;
import fi.vm.sade.valintalaskenta.laskenta.service.ValintalaskentaSuorittajaService;

/**
 * 
 * @author Jussi Jartamo
 * 
 *         Testaa että versiointi toimii. Luo useita peräkkäisiä versioita
 *         samasta valinnanvaiheesta ja verifioi, että uusin versio on aina
 *         uusin versio ja että versiojärjestys säilyy oikeana.
 */
@ContextConfiguration(locations = "classpath:application-context-test.xml")
@RunWith(SpringJUnit4ClassRunner.class)
public class VersiointiToimiiTesti {

    @Autowired
    private ApplicationContext context;

    @Autowired
    private ValintalaskentaSuorittajaService valintalaskentaService;

    @Autowired
    private VersiohallintaHakukohdeDAO versiohallintaDAO;

    @Test
    public void testVersiontiToimii() {
        HakukohdeTyyppi hakukohde = context.getBean(HakukohdeTyyppi.class);
        ValintaperusteetTyyppi valintaperusteet = createValintaperusteet(hakukohde);

        // Suoritetaan yksitellen sama laskenta jokaiselle hakemukselle!
        // Uusimmassa versiossa pitäisi olla viimeisenä laskettu hakemus!
        HakemusTyyppi hakemus0 = createHakemus(hakukohde);
        HakemusTyyppi hakemus1 = createHakemus(hakukohde);
        HakemusTyyppi hakemus2 = createHakemus(hakukohde);
        HakemusTyyppi hakemus3 = createHakemus(hakukohde);

        List<HakemusTyyppi> hakemukset = Arrays.asList(hakemus0, hakemus1, hakemus2, hakemus3);
        for (int kierros = 0; kierros < hakemukset.size(); ++kierros) {
            HakemusTyyppi hakemusjollelaskentasuoritetaan = hakemukset.get(kierros);
            valintalaskentaService.suoritaLaskenta(Arrays.asList(hakemusjollelaskentasuoritetaan),
                    Arrays.asList(valintaperusteet));
            VersiohallintaHakukohde versiohallinta = versiohallintaDAO.readByHakukohdeOidAndJarjestysnumero(
                    hakukohde.getHakukohdeOid(), valintaperusteet.getValinnanVaiheJarjestysluku());
            Assert.assertTrue("Joka kierroksella versioiden määrän pitää lisääntyä yhdellä!", versiohallinta
                    .getHakukohteet().size() - 1 == kierros);
            Versioituhakukohde vanhinversioituhakukohde = versiohallinta.getHakukohteet().haeVanhinVersio();
            Assert.assertTrue(
                    "Vanhimman version laskennan tuloksen hakemuksenoid täytyy olla aina sama kuin ensimmäisellä hakemuksella, jolle laskenta suoritettiin!",
                    vanhinversioituhakukohde.getHakukohde().getValinnanvaihe().getValintatapajono().get(0)
                            .getJarjestyskriteeritulokset().get(0).getHakemusoid().equals(hakemus0.getHakemusOid()));
            Versioituhakukohde uusinversioituhakukohde = versiohallinta.getHakukohteet().haeUusinVersio();
            Assert.assertTrue(
                    "Uusimman version laskennan tuloksen hakemuksenoid täytyy olla aina sama kuin viimeiseksi lasketulla hakemuksella!",
                    uusinversioituhakukohde.getHakukohde().getValinnanvaihe().getValintatapajono().get(0)
                            .getJarjestyskriteeritulokset().get(0).getHakemusoid()
                            .equals(hakemusjollelaskentasuoritetaan.getHakemusOid()));
        }
    }

    private ValintaperusteetTyyppi createValintaperusteet(HakukohdeTyyppi hakukohde) {
        ValintatapajonoJarjestyskriteereillaTyyppi jarjestyskriteeri = context
                .getBean(ValintatapajonoJarjestyskriteereillaTyyppi.class);
        jarjestyskriteeri.getJarjestyskriteerit().add(context.getBean(JarjestyskriteeriTyyppi.class));
        ValintaperusteetTyyppi valintaperusteet = context.getBean(ValintaperusteetTyyppi.class);
        valintaperusteet.getValintatapajonot().add(jarjestyskriteeri);
        valintaperusteet.setHakukohdeOid(hakukohde.getHakukohdeOid());
        return valintaperusteet;
    }

    private HakemusTyyppi createHakemus(HakukohdeTyyppi hakukohde) {
        HakemusTyyppi hakemus0 = context.getBean(HakemusTyyppi.class);
        AvainArvoTyyppi aidinkieli = context.getBean(AvainArvoTyyppi.class);
        AvainArvoTyyppi matematiikka = context.getBean(AvainArvoTyyppi.class);
        hakemus0.getHakutoive().add(hakukohde);
        hakemus0.getAvainArvo().addAll(Arrays.asList(aidinkieli, matematiikka));
        return hakemus0;
    }
}
