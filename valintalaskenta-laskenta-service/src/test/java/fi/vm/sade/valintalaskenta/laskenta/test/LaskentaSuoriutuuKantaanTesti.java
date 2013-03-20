package fi.vm.sade.valintalaskenta.laskenta.test;

import java.util.Arrays;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.util.Assert;

import fi.vm.sade.service.hakemus.schema.AvainArvoTyyppi;
import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.hakemus.schema.HakukohdeTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.JarjestyskriteeriTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteetTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintatapajonoJarjestyskriteereillaTyyppi;
import fi.vm.sade.valintalaskenta.dao.VersiohallintaHakukohdeDAO;
import fi.vm.sade.valintalaskenta.domain.VersiohallintaHakukohde;
import fi.vm.sade.valintalaskenta.laskenta.service.ValintalaskentaSuorittajaService;

/**
 * 
 * @author Jussi Jartamo
 * 
 *         Testaa että kantaan muodostuu laskennasta dokumentteja
 */
@ContextConfiguration(locations = "classpath:application-context-test.xml")
@RunWith(SpringJUnit4ClassRunner.class)
public class LaskentaSuoriutuuKantaanTesti {

    @Autowired
    private ApplicationContext context;

    @Autowired
    private ValintalaskentaSuorittajaService valintalaskentaService;

    @Autowired
    private VersiohallintaHakukohdeDAO versiohallintaDAO;

    @Test
    public void testLaskentaSuoriutuuKantaan() {
        HakukohdeTyyppi hakukohde = context.getBean(HakukohdeTyyppi.class);

        HakemusTyyppi hakemus0 = context.getBean(HakemusTyyppi.class);
        HakemusTyyppi hakemus1 = context.getBean(HakemusTyyppi.class);

        AvainArvoTyyppi aidinkieli = context.getBean(AvainArvoTyyppi.class);
        AvainArvoTyyppi matematiikka = context.getBean(AvainArvoTyyppi.class);
        hakemus0.getHakutoive().add(hakukohde);
        hakemus0.getAvainArvo().addAll(Arrays.asList(aidinkieli, matematiikka));
        hakemus1.getHakutoive().add(hakukohde);
        hakemus1.getAvainArvo().addAll(Arrays.asList(aidinkieli, matematiikka));

        ValintatapajonoJarjestyskriteereillaTyyppi jarjestyskriteeri = context
                .getBean(ValintatapajonoJarjestyskriteereillaTyyppi.class);
        jarjestyskriteeri.getJarjestyskriteerit().add(context.getBean(JarjestyskriteeriTyyppi.class));
        ValintaperusteetTyyppi valintaperusteet = context.getBean(ValintaperusteetTyyppi.class);
        valintaperusteet.getValintatapajonot().add(jarjestyskriteeri);
        valintaperusteet.setHakukohdeOid(hakukohde.getHakukohdeOid());

        valintalaskentaService.suoritaLaskenta(Arrays.asList(hakemus0, hakemus1), Arrays.asList(valintaperusteet));

        VersiohallintaHakukohde versiohallinta = versiohallintaDAO.readByHakukohdeOidAndJarjestysnumero(
                hakukohde.getHakukohdeOid(), valintaperusteet.getValinnanVaiheJarjestysluku());
        Assert.notNull(versiohallinta, "Kannassa täytyy olla versiohallinta suoritetun laskennan jäljiltä!");

    }
}
