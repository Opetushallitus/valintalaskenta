package fi.vm.sade.valintalaskenta.tulos.resource.impl;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.READ_UPDATE_CRUD;
import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.UPDATE_CRUD;

import fi.vm.sade.auditlog.User;
import fi.vm.sade.valintalaskenta.domain.dto.HarkinnanvarainenHyvaksyminenDTO;
import fi.vm.sade.valintalaskenta.tulos.logging.LaskentaAuditLog;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import fi.vm.sade.valintalaskenta.tulos.resource.HarkinnanvaraisuusResource;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import java.util.List;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;

@Controller
@Path("/harkinnanvarainenhyvaksynta")
@PreAuthorize("isAuthenticated()")
@Api(
    value = "/harkinnanvarainenhyvaksynta",
    description = "Resurssi harkinnanvaraisesti hakeneiden hakijoiden käsittelyyn")
public class HarkinnanvaraisuusResourceImpl implements HarkinnanvaraisuusResource {
  @Autowired private ValintalaskentaTulosService tulosService;

  @Autowired private ValintalaskentaModelMapper modelMapper;

  @Autowired private LaskentaAuditLog auditLog;

  @POST
  @Path("/haku/{hakuOid}/hakukohde/{hakukohdeOid}/hakemus/{hakemusOid}")
  @Produces(MediaType.APPLICATION_JSON)
  @PreAuthorize(UPDATE_CRUD)
  @ApiOperation(value = "Asettaa tilan harkinnanvaraisesti hakeneelle hakijalle")
  public void asetaTila(
      @ApiParam(value = "Haun OID", required = true) @PathParam("hakuOid") String hakuOid,
      @ApiParam(value = "Hakukohteen OID", required = true) @PathParam("hakukohdeOid")
          String hakukohdeOid,
      @ApiParam(value = "Hakemuksen OID", required = true) @PathParam("hakemusOid")
          String hakemusOid,
      @ApiParam(value = "Asetettava tila", required = true)
          HarkinnanvarainenHyvaksyminenDTO harkinnanvarainenHyvaksyminen,
      @Context HttpServletRequest request) {
    User user = auditLog.getUser(request);

    tulosService.asetaHarkinnanvaraisestiHyvaksymisenTila(
        hakuOid,
        hakukohdeOid,
        hakemusOid,
        harkinnanvarainenHyvaksyminen.getHarkinnanvaraisuusTila(),
        user);
  }

  @POST
  @Produces(MediaType.APPLICATION_JSON)
  @PreAuthorize(UPDATE_CRUD)
  @ApiOperation(value = "Asettaa tilan harkinnanvaraisesti hakeneelle hakijalle")
  public void asetaTilat(
      @ApiParam(value = "Asetettava tila", required = true)
          List<HarkinnanvarainenHyvaksyminenDTO> harkinnanvaraisetHyvaksymiset,
      @Context HttpServletRequest request) {
    User user = auditLog.getUser(request);

    for (HarkinnanvarainenHyvaksyminenDTO harkinnanvarainenHyvaksyminen :
        harkinnanvaraisetHyvaksymiset) {
      tulosService.asetaHarkinnanvaraisestiHyvaksymisenTila(
          harkinnanvarainenHyvaksyminen.getHakuOid(),
          harkinnanvarainenHyvaksyminen.getHakukohdeOid(),
          harkinnanvarainenHyvaksyminen.getHakemusOid(),
          harkinnanvarainenHyvaksyminen.getHarkinnanvaraisuusTila(),
          user);
    }
  }

  @GET
  @Path("/haku/{hakuOid}/hakukohde/{hakukohdeOid}")
  @Produces(MediaType.APPLICATION_JSON)
  @PreAuthorize(READ_UPDATE_CRUD)
  @ApiOperation(
      value = "Hakee hakukohteen harkinnanvaraisesti hakeneiden hakijoiden tilat",
      response = HarkinnanvarainenHyvaksyminenDTO.class)
  public List<HarkinnanvarainenHyvaksyminenDTO> hakukohde(
      @ApiParam(value = "Haku OID", required = true) @PathParam("hakuOid") String hakuOid,
      @ApiParam(value = "Hakukohde OID", required = true) @PathParam("hakukohdeOid")
          String hakukohdeOid) {
    return modelMapper.mapList(
        tulosService.haeHarkinnanvaraisestiHyvaksymisenTila(hakukohdeOid),
        HarkinnanvarainenHyvaksyminenDTO.class);
  }

  @GET
  @Path("/haku/{hakuOid}/hakemus/{hakemusOid}")
  @Produces(MediaType.APPLICATION_JSON)
  @PreAuthorize(READ_UPDATE_CRUD)
  @ApiOperation(
      value = "Hakee hakemuksen harkinnanvaraisesti tilat",
      response = HarkinnanvarainenHyvaksyminenDTO.class)
  public List<HarkinnanvarainenHyvaksyminenDTO> hakemus(
      @ApiParam(value = "Haku OID", required = true) @PathParam("hakuOid") String hakuOid,
      @ApiParam(value = "Hakemus OID", required = true) @PathParam("hakemusOid")
          String hakemusOid) {
    return modelMapper.mapList(
        tulosService.haeHakemuksenHarkinnanvaraisestiHyvaksymisenTilat(hakuOid, hakemusOid),
        HarkinnanvarainenHyvaksyminenDTO.class);
  }
}
