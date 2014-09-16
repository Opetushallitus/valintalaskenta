package fi.vm.sade.valintalaskenta.laskenta.service.impl;

import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetHakijaryhmaDTO;
import fi.vm.sade.service.valintaperusteet.dto.model.ValinnanVaiheTyyppi;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.laskenta.service.ValintalaskentaService;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.ValintalaskentaSuorittajaService;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.ValintakoelaskentaSuorittajaService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Service;

import java.util.Arrays;
import java.util.List;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.CRUD;

/**
 * @author Jussi Jartamo
 */
@Service
public class ValintalaskentaServiceImpl implements ValintalaskentaService {

	private static final Logger LOG = LoggerFactory
			.getLogger(ValintalaskentaServiceImpl.class);

	@Autowired
	private ValintalaskentaSuorittajaService valintalaskentaSuorittaja;

	@Autowired
	private ValintakoelaskentaSuorittajaService valintakoelaskentaSuorittajaService;

	@Override
    public String laske(List<HakemusDTO> hakemus,
                        List<ValintaperusteetDTO> valintaperuste,
                        List<ValintaperusteetHakijaryhmaDTO> hakijaryhmat,
                        String hakukohdeOid)
			throws RuntimeException {
		try {
			LOG.error(
					"Suoritetaan laskenta. Hakemuksia {} kpl ja valintaperusteita {} kpl",
					new Object[] { hakemus.size(), valintaperuste.size() });
			valintalaskentaSuorittaja.suoritaLaskenta(hakemus, valintaperuste, hakijaryhmat, hakukohdeOid);
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

    @Override
    public String laskeKaikki(List<HakemusDTO> hakemus,
                              List<ValintaperusteetDTO> valintaperuste,
                              List<ValintaperusteetHakijaryhmaDTO> hakijaryhmat,
                              String hakukohdeOid) throws RuntimeException {
        valintaperuste.sort((o1,o2) ->
                o1.getValinnanVaihe().getValinnanVaiheJarjestysluku() - o2.getValinnanVaihe().getValinnanVaiheJarjestysluku());

        valintaperuste.stream().forEachOrdered(peruste -> {
            if(peruste.getValinnanVaihe().getValinnanVaiheTyyppi().equals(ValinnanVaiheTyyppi.VALINTAKOE)) {
                hakemus.parallelStream().forEach(h -> valintakokeet(h, Arrays.asList(peruste)));
            } else {
                laske(hakemus, Arrays.asList(peruste), hakijaryhmat, hakukohdeOid);
            }
        });

        return "Onnistui!";
    }

}
