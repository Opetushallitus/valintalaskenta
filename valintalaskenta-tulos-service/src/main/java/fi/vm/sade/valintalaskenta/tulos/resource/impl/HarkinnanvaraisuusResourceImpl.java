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
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
@PreAuthorize("isAuthenticated()")
@Api(
    value = "/harkinnanvarainenhyvaksynta",
    description = "Resurssi harkinnanvaraisesti hakeneiden hakijoiden käsittelyyn")
@RequestMapping(value = "/harkinnanvarainenhyvaksynta")
public class HarkinnanvaraisuusResourceImpl implements HarkinnanvaraisuusResource {
  private final ValintalaskentaTulosService tulosService;
  private final ValintalaskentaModelMapper modelMapper;
  private final LaskentaAuditLog auditLog;

  @Autowired
  public HarkinnanvaraisuusResourceImpl(
      final ValintalaskentaTulosService tulosService,
      final ValintalaskentaModelMapper modelMapper,
      final LaskentaAuditLog auditLog) {
    this.tulosService = tulosService;
    this.modelMapper = modelMapper;
    this.auditLog = auditLog;
  }

  @PreAuthorize(UPDATE_CRUD)
  @ApiOperation(value = "Asettaa tilan harkinnanvaraisesti hakeneelle hakijalle")
  @PostMapping(
      value = "/haku/{hakuOid}/hakukohde/{hakukohdeOid}/hakemus/{hakemusOid}",
      produces = MediaType.APPLICATION_JSON_VALUE)
  public void asetaTila(
      @ApiParam(value = "Haun OID", required = true) @PathVariable("hakuOid") final String hakuOid,
      @ApiParam(value = "Hakukohteen OID", required = true) @PathVariable("hakukohdeOid")
          final String hakukohdeOid,
      @ApiParam(value = "Hakemuksen OID", required = true) @PathVariable("hakemusOid")
          final String hakemusOid,
      @ApiParam(value = "Asetettava tila", required = true)
          final HarkinnanvarainenHyvaksyminenDTO harkinnanvarainenHyvaksyminen,
      final HttpServletRequest request) {
    User user = auditLog.getUser(request);

    tulosService.asetaHarkinnanvaraisestiHyvaksymisenTila(
        hakuOid,
        hakukohdeOid,
        hakemusOid,
        harkinnanvarainenHyvaksyminen.getHarkinnanvaraisuusTila(),
        user);
  }

  @PreAuthorize(UPDATE_CRUD)
  @ApiOperation(value = "Asettaa tilan harkinnanvaraisesti hakeneelle hakijalle")
  @PostMapping(produces = MediaType.APPLICATION_JSON_VALUE)
  public void asetaTilat(
      @ApiParam(value = "Asetettava tila", required = true)
          final List<HarkinnanvarainenHyvaksyminenDTO> harkinnanvaraisetHyvaksymiset,
      final HttpServletRequest request) {
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

  @PreAuthorize(READ_UPDATE_CRUD)
  @ApiOperation(
      value = "Hakee hakukohteen harkinnanvaraisesti hakeneiden hakijoiden tilat",
      response = HarkinnanvarainenHyvaksyminenDTO.class)
  @GetMapping(
      value = "/haku/{hakuOid}/hakukohde/{hakukohdeOid}",
      produces = MediaType.APPLICATION_JSON_VALUE)
  public List<HarkinnanvarainenHyvaksyminenDTO> hakukohde(
      @ApiParam(value = "Haku OID", required = true) @PathVariable("hakuOid") final String hakuOid,
      @ApiParam(value = "Hakukohde OID", required = true) @PathVariable("hakukohdeOid")
          final String hakukohdeOid) {
    return modelMapper.mapList(
        tulosService.haeHarkinnanvaraisestiHyvaksymisenTila(hakukohdeOid),
        HarkinnanvarainenHyvaksyminenDTO.class);
  }

  @PreAuthorize(READ_UPDATE_CRUD)
  @ApiOperation(
      value = "Hakee hakemuksen harkinnanvaraisesti tilat",
      response = HarkinnanvarainenHyvaksyminenDTO.class)
  @GetMapping(
      value = "/haku/{hakuOid}/hakemus/{hakemusOid}",
      produces = MediaType.APPLICATION_JSON_VALUE)
  public List<HarkinnanvarainenHyvaksyminenDTO> hakemus(
      @ApiParam(value = "Haku OID", required = true) @PathVariable("hakuOid") final String hakuOid,
      @ApiParam(value = "Hakemus OID", required = true) @PathVariable("hakemusOid")
          final String hakemusOid) {
    return modelMapper.mapList(
        tulosService.haeHakemuksenHarkinnanvaraisestiHyvaksymisenTilat(hakuOid, hakemusOid),
        HarkinnanvarainenHyvaksyminenDTO.class);
  }
}
