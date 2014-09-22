package fi.vm.sade.valintalaskenta.laskenta.service.impl;

import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetHakijaryhmaDTO;
import fi.vm.sade.service.valintaperusteet.dto.model.ValinnanVaiheTyyppi;
import fi.vm.sade.sijoittelu.tulos.dto.HakemuksenTila;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;
import fi.vm.sade.valintalaskenta.laskenta.service.ValintalaskentaService;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.ValintalaskentaSuorittajaService;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.ValintakoelaskentaSuorittajaService;
import fi.vm.sade.valintalaskenta.tulos.dao.ValinnanvaiheDAO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;

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

    @Autowired
    private ValinnanvaiheDAO valinnanvaiheDAO;

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

    @Override
    public void applyValisijoittelu(Map<String, List<String>> valisijoiteltavatJonot, Map<String, fi.vm.sade.sijoittelu.tulos.dto.HakemusDTO> hakemusHashMap) {

        valisijoiteltavatJonot.keySet().parallelStream().forEach(hakukohdeOid -> {
            List<Valinnanvaihe> vaiheet = valinnanvaiheDAO.readByHakukohdeOid(hakukohdeOid);
            vaiheet.forEach(vaihe -> {
                vaihe.getValintatapajonot()
                        .forEach(jono -> {
                            if (valisijoiteltavatJonot.getOrDefault(hakukohdeOid, new ArrayList<>()).indexOf(jono.getValintatapajonoOid()) != -1) {
                                jono.getJonosijat().forEach(jonosija -> {
                                    fi.vm.sade.sijoittelu.tulos.dto.HakemusDTO hakemusDTO = hakemusHashMap.get(hakukohdeOid + jono.getValintatapajonoOid()
                                            + jonosija.getHakemusOid());
                                    List<HakemuksenTila> tilat = Arrays.asList(HakemuksenTila.VARALLA, HakemuksenTila.PERUUNTUNUT);
                                    if (hakemusDTO != null && tilat.indexOf(hakemusDTO.getTila()) != -1) {
                                        Collections.sort(jonosija.getJarjestyskriteeritulokset(), (jk1, jk2) -> jk1.getPrioriteetti() - jk2.getPrioriteetti());
                                        Jarjestyskriteeritulos jarjestyskriteeritulos = jonosija.getJarjestyskriteeritulokset().get(0);
                                        jarjestyskriteeritulos.setTila(JarjestyskriteerituloksenTila.HYLATTY);
                                        Map<String, String> kuvaukset = new HashMap<>();
                                        if(hakemusDTO.getTila() == HakemuksenTila.VARALLA) {
                                            kuvaukset.put("FI", "Hakemus ei mahtunut aloituspaikkojen sisään välisijoittelussa");
                                        } else if(hakemusDTO.getTila() == HakemuksenTila.PERUUNTUNUT) {
                                            kuvaukset.put("FI", "Hakemus hyväksyttiin korkeammalle hakutoiveelle");
                                        }
                                        jarjestyskriteeritulos.setKuvaus(kuvaukset);
                                    }
                                });
                            }
                        });
                valinnanvaiheDAO.saveOrUpdate(vaihe);
            });
        });

    }

}
