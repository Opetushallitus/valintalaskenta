package fi.vm.sade.valintalaskenta.laskenta.service;

import fi.vm.sade.service.valintaperusteet.dto.ValintakoeDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetValinnanVaiheDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintatapajonoJarjestyskriteereillaDTO;
import fi.vm.sade.valintalaskenta.domain.dto.LaskeDTO;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.ValisijoitteluKasittelija;

import org.junit.Before;
import org.junit.Test;

import java.util.*;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

/**
 * User: wuoti
 * Date: 4.9.2013
 * Time: 14.44
 */
public class ValisijoitteluKasittelijaTest {

    private ValisijoitteluKasittelija valisijoitteluKasittelija;

    @Before
    public void setUp() {
        valisijoitteluKasittelija = new ValisijoitteluKasittelija();
    }

    @Test
    public void valisijoiteltavatJonotTest() {

        ValintatapajonoJarjestyskriteereillaDTO jono1 = new ValintatapajonoJarjestyskriteereillaDTO();
        jono1.setOid("jono1");
        jono1.setValisijoittelu(true);

        ValintatapajonoJarjestyskriteereillaDTO jono2 = new ValintatapajonoJarjestyskriteereillaDTO();
        jono2.setOid("jono2");
        jono2.setValisijoittelu(false);

        ValintatapajonoJarjestyskriteereillaDTO jono3 = new ValintatapajonoJarjestyskriteereillaDTO();
        jono3.setOid("jono3");
        jono3.setValisijoittelu(true);

        ValintakoeDTO koe1 = new ValintakoeDTO();
        ValintakoeDTO koe2 = new ValintakoeDTO();

        ValintaperusteetValinnanVaiheDTO vaihe1 = new ValintaperusteetValinnanVaiheDTO();
        vaihe1.setAktiivinen(true);
        vaihe1.setValinnanVaiheOid("vaihe1");
        vaihe1.setValinnanVaiheJarjestysluku(1);
        vaihe1.setValintatapajono(Arrays.asList(jono1, jono2));

        ValintaperusteetValinnanVaiheDTO vaihe2 = new ValintaperusteetValinnanVaiheDTO();
        vaihe2.setAktiivinen(true);
        vaihe2.setValinnanVaiheOid("vaihe2");
        vaihe2.setValinnanVaiheJarjestysluku(2);
        vaihe2.setValintakoe(Arrays.asList(koe1));

        ValintaperusteetValinnanVaiheDTO vaihe3 = new ValintaperusteetValinnanVaiheDTO();
        vaihe3.setAktiivinen(true);
        vaihe3.setValinnanVaiheOid("vaihe3");
        vaihe3.setValinnanVaiheJarjestysluku(3);
        vaihe3.setValintatapajono(Arrays.asList(jono3));

        ValintaperusteetDTO perusteet1 = new ValintaperusteetDTO();
        perusteet1.setHakukohdeOid("hakukohde1");
        perusteet1.setValinnanVaihe(vaihe1);

        ValintaperusteetDTO perusteet2 = new ValintaperusteetDTO();
        perusteet2.setHakukohdeOid("hakukohde1");
        perusteet2.setValinnanVaihe(vaihe2);

        ValintaperusteetDTO perusteet3 = new ValintaperusteetDTO();
        perusteet3.setHakukohdeOid("hakukohde1");
        perusteet3.setValinnanVaihe(vaihe3);

        ValintaperusteetDTO perusteet4 = new ValintaperusteetDTO();
        perusteet4.setHakukohdeOid("hakukohde2");
        perusteet4.setValinnanVaihe(vaihe3);

        ValintaperusteetDTO perusteet5 = new ValintaperusteetDTO();
        perusteet5.setHakukohdeOid("hakukohde2");
        perusteet5.setValinnanVaihe(vaihe2);


        LaskeDTO hakukohde1 = new LaskeDTO(false,"hakukohde1", new ArrayList<>(), Arrays.asList(perusteet1, perusteet2, perusteet3));
        LaskeDTO hakukohde2 = new LaskeDTO(false,"hakukohde2", new ArrayList<>(), Arrays.asList(perusteet4, perusteet5));

        ValisijoitteluKasittelija.ValisijoiteltavatJonot jonot = valisijoitteluKasittelija.valisijoiteltavatJonot(Arrays.asList(hakukohde1, hakukohde2));
        assertEquals(2, jonot.jonot.size());
        assertEquals(2, jonot.jonot.get("hakukohde1").size());
        assertEquals(1, jonot.jonot.get("hakukohde2").size());
        assertTrue(jonot.valinnanvaiheet.contains(1));
        assertTrue(!jonot.valinnanvaiheet.contains(2));
        assertTrue(jonot.valinnanvaiheet.contains(3));
        assertTrue(jonot.jonot.get("hakukohde1").indexOf("jono1") != -1);
        assertTrue(jonot.jonot.get("hakukohde1").indexOf("jono3") != -1);
        assertTrue(jonot.jonot.get("hakukohde2").indexOf("jono3") != -1);

    }
}
