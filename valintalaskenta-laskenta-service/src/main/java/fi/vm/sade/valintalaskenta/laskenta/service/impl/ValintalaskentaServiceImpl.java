package fi.vm.sade.valintalaskenta.laskenta.service.impl;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.CRUD;

import java.util.Arrays;
import java.util.List;

import javax.jws.WebParam;
import javax.jws.WebService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;

import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.valintalaskenta.LaskeFault_Exception;
import fi.vm.sade.service.valintalaskenta.ValintalaskentaService;
import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteetTyyppi;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.ValintalaskentaSuorittajaService;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.ValintakoelaskentaSuorittajaService;

/**
 * @author Jussi Jartamo
 */
@WebService(endpointInterface = "fi.vm.sade.service.valintalaskenta.ValintalaskentaService")
@PreAuthorize("isAuthenticated()")
public class ValintalaskentaServiceImpl implements ValintalaskentaService {

	private static final Logger LOG = LoggerFactory
			.getLogger(ValintalaskentaServiceImpl.class);

	@Autowired
	private ValintalaskentaSuorittajaService valintalaskentaSuorittaja;

	@Autowired
	private ValintakoelaskentaSuorittajaService valintakoelaskentaSuorittajaService;

	@Override
	@PreAuthorize(CRUD)
	public String laske(
			@WebParam(name = "hakemus", targetNamespace = "") List<HakemusTyyppi> hakemus,
			@WebParam(name = "valintaperuste", targetNamespace = "") List<ValintaperusteetTyyppi> valintaperuste)
			throws LaskeFault_Exception {
		try {
			LOG.info(
					"Suoritetaan laskenta. Hakemuksia {} kpl ja valintaperusteita {} kpl",
					new Object[] { hakemus.size(), valintaperuste.size() });
			valintalaskentaSuorittaja.suoritaLaskenta(hakemus, valintaperuste);
			return "Onnistui!";
		} catch (Exception e) {
			LOG.error("Valintalaskennassa tapahtui virhe {} {} {}",
					e.getMessage(), e.getCause(),
					Arrays.toString(e.getStackTrace()));

			throw new LaskeFault_Exception(e.getMessage(), e.getCause());
		}
	}

	/**
	 * Metodi ottaa hakemuksen, valintaperusteet ja tallentaa kantaan yhden
	 * hakijan tiedot
	 * 
	 * @param hakemus
	 * @param valintaperuste
	 * @return
	 */
	@Override
	@PreAuthorize(CRUD)
	public String valintakokeet(
			@WebParam(name = "hakemus", targetNamespace = "") HakemusTyyppi hakemus,
			@WebParam(name = "valintaperuste", targetNamespace = "") List<ValintaperusteetTyyppi> valintaperuste)
			throws LaskeFault_Exception {
		try {
			valintakoelaskentaSuorittajaService.laske(hakemus, valintaperuste);
			return "Onnistui!";
		} catch (Exception e) {
			LOG.error("Valintakoevaihe epäonnistui", e);
			throw new LaskeFault_Exception(e.getMessage(), e.getCause());
		}
	}

}
