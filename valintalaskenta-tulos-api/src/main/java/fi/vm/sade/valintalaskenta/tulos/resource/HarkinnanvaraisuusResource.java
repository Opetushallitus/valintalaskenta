package fi.vm.sade.valintalaskenta.tulos.resource;

import fi.vm.sade.valintalaskenta.domain.dto.HarkinnanvarainenHyvaksyminenDTO;
import java.util.List;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.*;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;

@Path("resources/harkinnanvarainenhyvaksynta")
public interface HarkinnanvaraisuusResource {
  @POST
  @Path("/haku/{hakuOid}/hakukohde/{hakukohdeOid}/hakemus/{hakemusOid}")
  @Produces(MediaType.APPLICATION_JSON)
  void asetaTila(
      @PathParam("hakuOid") final String hakuOid,
      @PathParam("hakukohdeOid") final String hakukohdeOid,
      @PathParam("hakemusOid") final String hakemusOid,
      final HarkinnanvarainenHyvaksyminenDTO harkinnanvarainenHyvaksyminen,
      @Context final HttpServletRequest request);

  @POST
  @Produces(MediaType.APPLICATION_JSON)
  void asetaTilat(
      final List<HarkinnanvarainenHyvaksyminenDTO> harkinnanvaraisetHyvaksymiset,
      @Context final HttpServletRequest request);

  @GET
  @Path("/haku/{hakuOid}/hakukohde/{hakukohdeOid}")
  @Produces(MediaType.APPLICATION_JSON)
  List<HarkinnanvarainenHyvaksyminenDTO> hakukohde(
      @PathParam("hakuOid") final String hakuOid,
      @PathParam("hakukohdeOid") final String hakukohdeOid);

  @GET
  @Path("/haku/{hakuOid}/hakemus/{hakemusOid}")
  @Produces(MediaType.APPLICATION_JSON)
  List<HarkinnanvarainenHyvaksyminenDTO> hakemus(
      @PathParam("hakuOid") final String hakuOid, @PathParam("hakemusOid") final String hakemusOid);
}
