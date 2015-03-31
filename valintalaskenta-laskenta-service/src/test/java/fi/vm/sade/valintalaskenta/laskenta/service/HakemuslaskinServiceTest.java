package fi.vm.sade.valintalaskenta.laskenta.service;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyCollection;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.internal.verification.VerificationModeFactory.times;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import fi.vm.sade.valintalaskenta.laskenta.dao.ValintakoeOsallistuminenDAO;
import org.junit.Before;
import org.junit.Test;
import org.springframework.test.util.ReflectionTestUtils;

import fi.vm.sade.service.valintaperusteet.laskenta.Lukuarvofunktio;
import fi.vm.sade.service.valintaperusteet.laskenta.api.FunktioTulos;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakemus;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakukohde;
import fi.vm.sade.service.valintaperusteet.laskenta.api.LaskentaService;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Laskentatulos;
import fi.vm.sade.service.valintaperusteet.laskenta.api.SyotettyArvo;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Hyvaksyttavissatila;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Tila;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;
import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.laskenta.dao.JarjestyskriteerihistoriaDAO;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.HakemuslaskinService;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.EdellinenValinnanvaiheKasittelija;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.HakemusWrapper;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.HakemuslaskinImpl;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.JonosijaJaSyotetytArvot;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.TilaJaSelite;
import fi.vm.sade.valintalaskenta.laskenta.testdata.TestDataUtil;

/**
 * User: wuoti Date: 5.9.2013 Time: 9.55
 */
public class HakemuslaskinServiceTest {

	private HakemuslaskinService hakemuslaskinService;

	private LaskentaService laskentaServiceMock;
	private JarjestyskriteerihistoriaDAO jarjestyskriteerihistoriaDAOMock;
	private EdellinenValinnanvaiheKasittelija edellinenValinnanvaiheKasittelijaMock;
    private ValintakoeOsallistuminenDAO valintakoeOsallistuminenDAOMock;

	@Before
	public void setUp() {
		hakemuslaskinService = new HakemuslaskinImpl();
		laskentaServiceMock = mock(LaskentaService.class);
		jarjestyskriteerihistoriaDAOMock = mock(JarjestyskriteerihistoriaDAO.class);
		edellinenValinnanvaiheKasittelijaMock = mock(EdellinenValinnanvaiheKasittelija.class);
        valintakoeOsallistuminenDAOMock = mock(ValintakoeOsallistuminenDAO.class);

		ReflectionTestUtils.setField(hakemuslaskinService, "laskentaService",
				laskentaServiceMock);
		ReflectionTestUtils.setField(hakemuslaskinService,
				"jarjestyskriteerihistoriaDAO",
				jarjestyskriteerihistoriaDAOMock);
		ReflectionTestUtils.setField(hakemuslaskinService,
				"edellinenValinnanvaiheKasittelija",
				edellinenValinnanvaiheKasittelijaMock);
        ReflectionTestUtils.setField(hakemuslaskinService,
                "valintakoeOsallistuminenDAO",
                valintakoeOsallistuminenDAOMock);
	}

	@Test
	public void test() {
		final String[] hakukohteet = { "hakukohdeOid1", "hakukohdeOid2",
				"hakukohdeOid3" };
		final Hakukohde laskettavaHakukohde = new Hakukohde(hakukohteet[0],
				new HashMap<String, String>());
		final String hakemusOid = "hakemusOid1";
		final String hakijaOid = "hakijaOid1";
		final boolean harkinnanvaraisuus = false;
		final int hakutoiveprioriteetti = 1;

		Map<String, JonosijaJaSyotetytArvot> jonosijat = new HashMap<String, JonosijaJaSyotetytArvot>();

		HakemusWrapper hakemus = new HakemusWrapper();
		hakemus.setHakutoiveprioriteetti(hakutoiveprioriteetti);
		hakemus.setHakemusDTO(TestDataUtil.luoHakemus(hakemusOid, hakijaOid,
				hakukohteet));

		hakemus.setHarkinnanvaraisuus(harkinnanvaraisuus);
		hakemus.setLaskentahakemus(new Hakemus(hakemusOid, new HashMap<Integer, String>(), new HashMap<String, String>(), new HashMap<>()));

		final Map<String, String> edellinenValinnanvaiheTilaSelite = new HashMap<String, String>();
		edellinenValinnanvaiheTilaSelite.put("FI", "selite");
		TilaJaSelite tilaEdellisenVaiheenMukaan = new TilaJaSelite(
				JarjestyskriteerituloksenTila.HYLATTY,
				edellinenValinnanvaiheTilaSelite);

		final Tila laskettuTila = new Hyvaksyttavissatila();
		final BigDecimal jarjestyskriteeriarvo = new BigDecimal("100.0");
		Laskentatulos<BigDecimal> tulos = new Laskentatulos<BigDecimal>(
				laskettuTila, jarjestyskriteeriarvo, new StringBuffer(),
				new HashMap<String, SyotettyArvo>(),
				new HashMap<String, FunktioTulos>());
		when(
				laskentaServiceMock.suoritaValintalaskenta(
						eq(laskettavaHakukohde), any(Hakemus.class),
						anyCollection(), any(Lukuarvofunktio.class)))
				.thenReturn(tulos);
		when(
				edellinenValinnanvaiheKasittelijaMock
						.tilaEdellisenValinnanvaiheenMukaan(eq(hakemusOid),
								eq(laskettuTila), any(Valinnanvaihe.class)))
				.thenReturn(tilaEdellisenVaiheenMukaan);

		hakemuslaskinService.suoritaLaskentaHakemukselle(laskettavaHakukohde,
				hakemus, new ArrayList<Hakemus>(), mock(Lukuarvofunktio.class),
				1, new Valinnanvaihe(), jonosijat, "jkNimi", 1);

		verify(jarjestyskriteerihistoriaDAOMock, times(1)).create(
				any(Jarjestyskriteerihistoria.class));

		assertEquals(1, jonosijat.values().size());
		Jonosija jonosija = jonosijat.values().iterator().next().getJonosija();

		assertEquals(hakemusOid, jonosija.getHakemusOid());
		assertEquals(hakijaOid, jonosija.getHakijaOid());
		assertEquals(hakutoiveprioriteetti, jonosija.getHakutoiveprioriteetti());
		assertEquals(1, jonosija.getJarjestyskriteeritulokset().size());
		Jarjestyskriteeritulos jktulos = jonosija
				.getJarjestyskriteeritulokset().get(0);

		// Laskennan arvoa ei enää tallenneta järjestyskriteeritulokseen jos
		// hakemus on hylätty edellisessä valinnanvaiheessa
		// assertEquals(jarjestyskriteeriarvo, jktulos.getArvo());
		assertEquals(null, jktulos.getArvo());
		assertEquals(tilaEdellisenVaiheenMukaan.getTila(), jktulos.getTila());
		assertEquals(tilaEdellisenVaiheenMukaan.getSelite(),
				jktulos.getKuvaus());
	}

}
