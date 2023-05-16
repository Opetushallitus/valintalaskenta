package fi.vm.sade.valintalaskenta.tulos.resource.impl;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.READ_UPDATE_CRUD;

import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeOsallistuminenDTO;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import fi.vm.sade.valintalaskenta.tulos.resource.ValintakoeResource;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;

@Controller
@PreAuthorize("isAuthenticated()")
@Api(value = "/valintakoe", description = "Resurssi valintakoeosallistumistulosten hakemiseen")
@RequestMapping("/valintakoe")
public class ValintakoeResourceImpl implements ValintakoeResource {
  private final ValintalaskentaTulosService tulosService;
  private final ValintalaskentaModelMapper modelMapper;

  @Autowired
  public ValintakoeResourceImpl(
      final ValintalaskentaTulosService tulosService,
      final ValintalaskentaModelMapper modelMapper) {
    this.tulosService = tulosService;
    this.modelMapper = modelMapper;
  }

  @PreAuthorize(READ_UPDATE_CRUD)
  @ApiOperation(
      value = "Hakee valintakoeosallistumiset hakemukselle OID:n perusteella",
      response = ValintakoeOsallistuminenDTO.class)
  @GetMapping(value = "/hakemus/{hakemusOid}", produces = MediaType.APPLICATION_JSON_VALUE)
  public ValintakoeOsallistuminenDTO haku(
      @ApiParam(value = "Hakemus OID", required = true) @PathVariable("hakemusOid")
          final String hakemusOid) {
    return modelMapper.map(
        tulosService.haeValintakoeOsallistumiset(hakemusOid), ValintakoeOsallistuminenDTO.class);
  }

  @PreAuthorize(READ_UPDATE_CRUD)
  @ApiOperation(
      value = "Hakee valintakoeosallistumiset hakukohteelle OID:n perusteella",
      response = ValintakoeOsallistuminenDTO.class)
  @GetMapping(value = "/hakutoive/{hakukohdeOid}", produces = MediaType.APPLICATION_JSON_VALUE)
  public List<ValintakoeOsallistuminenDTO> hakuByHakutoive(
      @ApiParam(value = "Hakukohde OID", required = true) @PathVariable("hakukohdeOid")
          final String hakukohdeOid) {
    return modelMapper.mapList(
        tulosService.haeValintakoeOsallistumisetByHakutoive(hakukohdeOid),
        ValintakoeOsallistuminenDTO.class);
  }

  @PreAuthorize(READ_UPDATE_CRUD)
  @ApiOperation(
      value = "Hakee valintakoeosallistumiset hakukohteille OID:n perusteella",
      response = ValintakoeOsallistuminenDTO.class)
  @PostMapping(
      value = "/hakutoive",
      produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  public List<ValintakoeOsallistuminenDTO> hakuByOids(
      @RequestBody final List<String> hakukohdeOids) {
    return modelMapper.mapList(
        tulosService.haeValintakoeOsallistumisetByHakukohdes(hakukohdeOids),
        ValintakoeOsallistuminenDTO.class);
  }

  @PreAuthorize(READ_UPDATE_CRUD)
  @ApiOperation(
      value = "Hakee valintakoeosallistumiset hakijoille OID:n perusteella",
      response = ValintakoeOsallistuminenDTO.class)
  @PostMapping(
      value = "/hakijat",
      produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  public List<ValintakoeOsallistuminenDTO> hakijatByOids(
      @RequestBody final List<String> hakijaOids) {
    return modelMapper.mapList(
        tulosService.haeValintakoeOsallistumisetByHakijas(hakijaOids),
        ValintakoeOsallistuminenDTO.class);
  }
}
