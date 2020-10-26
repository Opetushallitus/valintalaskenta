package fi.vm.sade.valintalaskenta.tulos.resource.impl;

import fi.vm.sade.valintalaskenta.domain.dto.JonoDto;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import io.swagger.annotations.Api;
import java.util.List;
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;

@Controller
@Path("valintalaskentakoostepalvelu")
@Api(
    value = "/valintalaskentakoostepalvelu",
    description = "Resurssi tulosten hakemiseen hakukohteittain")
public class ValintalaskentakoostepalveluResourceImpl {
  protected static final Logger LOGGER =
      LoggerFactory.getLogger(ValintalaskentakoostepalveluResourceImpl.class);

  @Autowired private ValintalaskentaTulosService tulosService;

  /**
   * @param hakuOid
   * @return HAKUKOHDE OID -> LIST[VALINTATAPAJONO OID]
   */
  @GET
  @Path("jonotsijoittelussa/{hakuOid}")
  @Consumes("application/json")
  @Produces("application/json")
  public List<JonoDto> jonotSijoittelussa(@PathParam("hakuOid") String hakuOid) {
    return tulosService.haeJonotSijoittelussa(hakuOid);
  }
}
