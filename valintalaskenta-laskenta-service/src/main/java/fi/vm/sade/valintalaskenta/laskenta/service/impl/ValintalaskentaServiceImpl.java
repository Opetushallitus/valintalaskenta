package fi.vm.sade.valintalaskenta.laskenta.service.impl;

import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.laskenta.service.ValintalaskentaService;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.ValintalaskentaSuorittajaService;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.ValintakoelaskentaSuorittajaService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Arrays;
import java.util.List;

/**
 * @author Jussi Jartamo
 */
//@PreAuthorize("isAuthenticated()")
@Service
public class ValintalaskentaServiceImpl implements ValintalaskentaService {

	private static final Logger LOG = LoggerFactory
			.getLogger(ValintalaskentaServiceImpl.class);

	@Autowired
	private ValintalaskentaSuorittajaService valintalaskentaSuorittaja;

	@Autowired
	private ValintakoelaskentaSuorittajaService valintakoelaskentaSuorittajaService;

//	@PreAuthorize(CRUD)
	@Override
    public String laske(List<HakemusDTO> hakemus,
                        List<ValintaperusteetDTO> valintaperuste)
			throws RuntimeException {
		try {
			LOG.info(
					"Suoritetaan laskenta. Hakemuksia {} kpl ja valintaperusteita {} kpl",
					new Object[] { hakemus.size(), valintaperuste.size() });
			valintalaskentaSuorittaja.suoritaLaskenta(hakemus, valintaperuste);
			return "Onnistui!";
		} catch (Exception e) {
            e.printStackTrace();
			LOG.error("Valintalaskennassa tapahtui virhe {} {} {}",
					e.getMessage(), e.getCause(),
					Arrays.toString(e.getStackTrace()));

			throw new RuntimeException(e.getMessage(), e.getCause());
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
//	@PreAuthorize(CRUD)
	@Override
    public String valintakokeet(HakemusDTO hakemus,
                                List<ValintaperusteetDTO> valintaperuste)
			throws RuntimeException {
		try {
			valintakoelaskentaSuorittajaService.laske(hakemus, valintaperuste);
			return "Onnistui!";
		} catch (Exception e) {
			LOG.error("Valintakoevaihe epäonnistui", e);
			throw new RuntimeException(e.getMessage(), e.getCause());
		}
	}

}
