package fi.vm.sade.valintalaskenta.laskenta.service;

import fi.vm.sade.service.valintaperusteet.laskenta.Lukuarvofunktio;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakemus;
import fi.vm.sade.service.valintaperusteet.laskenta.api.LaskentaService;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Laskentatulos;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Hyvaksyttavissatila;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Tila;
import fi.vm.sade.valintalaskenta.domain.valinta.*;
import fi.vm.sade.valintalaskenta.laskenta.dao.JarjestyskriteerihistoriaDAO;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.HakemuslaskinService;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.EdellinenValinnanvaiheKasittelija;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.HakemusWrapper;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.HakemuslaskinImpl;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.TilaJaSelite;
import fi.vm.sade.valintalaskenta.laskenta.testdata.TestDataUtil;
import org.junit.Before;
import org.junit.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyCollection;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.*;
import static org.mockito.internal.verification.VerificationModeFactory.times;

/**
 * User: wuoti
 * Date: 5.9.2013
 * Time: 9.55
 */
public class HakemuslaskinServiceTest {

    private HakemuslaskinService hakemuslaskinService;

    private LaskentaService laskentaServiceMock;
    private JarjestyskriteerihistoriaDAO jarjestyskriteerihistoriaDAOMock;
    private EdellinenValinnanvaiheKasittelija edellinenValinnanvaiheKasittelijaMock;

    @Before
    public void setUp() {
        hakemuslaskinService = new HakemuslaskinImpl();
        laskentaServiceMock = mock(LaskentaService.class);
        jarjestyskriteerihistoriaDAOMock = mock(JarjestyskriteerihistoriaDAO.class);
        edellinenValinnanvaiheKasittelijaMock = mock(EdellinenValinnanvaiheKasittelija.class);

        ReflectionTestUtils.setField(hakemuslaskinService, "laskentaService", laskentaServiceMock);
        ReflectionTestUtils.setField(hakemuslaskinService, "jarjestyskriteerihistoriaDAO", jarjestyskriteerihistoriaDAOMock);
        ReflectionTestUtils.setField(hakemuslaskinService, "edellinenValinnanvaiheKasittelija", edellinenValinnanvaiheKasittelijaMock);
    }


    @Test
    public void test() {
        final String[] hakukohteet = {"hakukohdeOid1", "hakukohdeOid2", "hakukohdeOid3"};
        final String laskettavaHakukohde = hakukohteet[0];
        final String hakemusOid = "hakemusOid1";
        final String hakijaOid = "hakijaOid1";
        final boolean harkinnanvaraisuus = false;
        final int hakutoiveprioriteetti = 1;

        Map<String, Jonosija> jonosijat = new HashMap<String, Jonosija>();

        HakemusWrapper hakemus = new HakemusWrapper();
        hakemus.setHakutoiveprioriteetti(hakutoiveprioriteetti);
        hakemus.setHakemusTyyppi(TestDataUtil.luoHakemus(hakemusOid, hakijaOid, hakukohteet));

        hakemus.setHarkinnanvaraisuus(harkinnanvaraisuus);
        hakemus.setLaskentahakemus(new Hakemus(hakemusOid, new HashMap<Integer, String>(), new HashMap<String, String>()));

        final String edellinenValinnanvaiheTilaSelite = "selite";
        TilaJaSelite tilaEdellisenVaiheenMukaan = new TilaJaSelite(JarjestyskriteerituloksenTila.HYLATTY, edellinenValinnanvaiheTilaSelite);

        final Tila laskettuTila = new Hyvaksyttavissatila();
        final BigDecimal jarjestyskriteeriarvo = new BigDecimal("100.0");
        Laskentatulos<BigDecimal> tulos = new Laskentatulos<BigDecimal>(laskettuTila,
                jarjestyskriteeriarvo);
        when(laskentaServiceMock.suoritaLasku(eq(laskettavaHakukohde), any(Hakemus.class), anyCollection(),
                any(Lukuarvofunktio.class), any(StringBuffer.class))).thenReturn(tulos);
        when(edellinenValinnanvaiheKasittelijaMock.tilaEdellisenValinnanvaiheenMukaan(eq(hakemusOid),
                eq(laskettuTila), any(Valinnanvaihe.class))).thenReturn(tilaEdellisenVaiheenMukaan);

        hakemuslaskinService.suoritaLaskentaHakemukselle(hakukohteet[0], hakemus, new ArrayList<Hakemus>(),
                mock(Lukuarvofunktio.class), new Jarjestyskriteeritulos(), new Valinnanvaihe(), jonosijat);

        verify(jarjestyskriteerihistoriaDAOMock, times(1)).create(any(Jarjestyskriteerihistoria.class));

        assertEquals(1, jonosijat.values().size());
        Jonosija jonosija = jonosijat.values().iterator().next();

        assertEquals(hakemusOid, jonosija.getHakemusOid());
        assertEquals(hakijaOid, jonosija.getHakijaOid());
        assertEquals(hakutoiveprioriteetti, jonosija.getHakutoiveprioriteetti());
        assertEquals(1, jonosija.getJarjestyskriteeritulokset().size());
        Jarjestyskriteeritulos jktulos = jonosija.getJarjestyskriteeritulokset().get(0);

        assertEquals(jarjestyskriteeriarvo, jktulos.getArvo());
        assertEquals(tilaEdellisenVaiheenMukaan.getTila(), jktulos.getTila());
        assertEquals(tilaEdellisenVaiheenMukaan.getSelite(), jktulos.getKuvaus());
    }

}
