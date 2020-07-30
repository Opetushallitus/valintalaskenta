package fi.vm.sade.valintalaskenta.tulos.resource.impl;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.READ_UPDATE_CRUD;

import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.HakemusOsallistuminenDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.HakuDTO;
import fi.vm.sade.valintalaskenta.tulos.resource.ValintatietoResource;
import fi.vm.sade.valintalaskenta.tulos.service.impl.ValintatietoService;
import java.util.List;
import javax.ws.rs.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;

@Controller
@Path("valintatieto")
@PreAuthorize("isAuthenticated()")
public class ValintatietoResourceImpl implements ValintatietoResource {
  @Autowired private ValintatietoService valintatietoService;

  @Override
  @POST
  @Path("hakukohde/{hakukohdeOid}")
  @Consumes("application/json")
  @Produces("application/json")
  @PreAuthorize(READ_UPDATE_CRUD)
  public List<HakemusOsallistuminenDTO> haeValintatiedotHakukohteelle(
      @PathParam("hakukohdeOid") String hakukohdeOid, List<String> valintakoeTunnisteet) {
    return valintatietoService.haeValintatiedotHakukohteelle(valintakoeTunnisteet, hakukohdeOid);
  }

  @Override
  @GET
  @Path("haku/{hakuOid}")
  @Produces("application/json")
  @PreAuthorize(READ_UPDATE_CRUD)
  public HakuDTO haeValintatiedot(@PathParam("hakuOid") String hakuOid) {
    return valintatietoService.haeValintatiedot(hakuOid);
  }
}
