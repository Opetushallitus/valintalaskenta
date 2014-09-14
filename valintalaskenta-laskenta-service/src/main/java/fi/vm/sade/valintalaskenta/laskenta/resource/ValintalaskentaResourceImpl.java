package fi.vm.sade.valintalaskenta.laskenta.resource;

import java.util.Arrays;

import fi.vm.sade.valintalaskenta.domain.dto.LaskeDTO;
import fi.vm.sade.valintalaskenta.domain.resource.ValintalaskentaResource;
import fi.vm.sade.valintalaskenta.laskenta.service.ValintalaskentaService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Component;

import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.CRUD;

/**
 * Created by jukais on 21.3.2014.
 */
@Component
@Path("valintalaskenta")
@PreAuthorize("isAuthenticated()")
public class ValintalaskentaResourceImpl implements ValintalaskentaResource {
	private static final Logger LOG = LoggerFactory
			.getLogger(ValintalaskentaResourceImpl.class);
	@Autowired
	private ValintalaskentaService valintalaskentaService;

	@Override
	@Path("laske")
	@Consumes("application/json")
	@Produces("text/plain")
	@POST
	@PreAuthorize(CRUD)
	public String laske(LaskeDTO laskeDTO) {
		try {
			return valintalaskentaService.laske(laskeDTO.getHakemus(),
					laskeDTO.getValintaperuste(), laskeDTO.getHakijaryhmat());
		} catch (Exception e) {
			LOG.error("Valintalaskenta epaonnistui: {}\r\n{}", e.getMessage(),
					Arrays.toString(e.getStackTrace()));
			throw e;
		}
	}

	@Override
	@Path("valintakokeet")
	@Consumes("application/json")
	@Produces("text/plain")
	@POST
	@PreAuthorize(CRUD)
	public String valintakokeet(LaskeDTO laskeDTO) {
		try {
			return valintalaskentaService.valintakokeet(laskeDTO.getHakemus()
					.get(0), laskeDTO.getValintaperuste());
		} catch (Exception e) {
			LOG.error("Valintakoelaskenta epaonnistui: {}\r\n{}",
					e.getMessage(), Arrays.toString(e.getStackTrace()));
			throw e;
		}
	}

	@Override
	@Path("laskekaikki")
	@Consumes("application/json")
	@Produces("text/plain")
	@POST
	@PreAuthorize(CRUD)
	public String laskeKaikki(LaskeDTO laskeDTO) {
		try {
			return valintalaskentaService.laskeKaikki(laskeDTO.getHakemus(),
					laskeDTO.getValintaperuste(), laskeDTO.getHakijaryhmat());
		} catch (Exception e) {
			LOG.error(
					"Valintalaskenta ja valintakoelaskenta epaonnistui: {}\r\n{}",
					e.getMessage(), Arrays.toString(e.getStackTrace()));
			throw e;
		}
	}
}
