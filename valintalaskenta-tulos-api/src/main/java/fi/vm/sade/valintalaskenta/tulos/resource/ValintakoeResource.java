package fi.vm.sade.valintalaskenta.tulos.resource;

import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeOsallistuminenDTO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import java.util.List;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;

@Api(value = "/valintakoe", description = "Resurssi valintakoeosallistumistulosten hakemiseen")
@RequestMapping("/valintakoe")
public interface ValintakoeResource {
  @ApiOperation(
      value = "Hakee valintakoeosallistumiset hakemukselle OID:n perusteella",
      response = ValintakoeOsallistuminenDTO.class)
  @GetMapping(value = "/hakemus/{hakemusOid}", produces = MediaType.APPLICATION_JSON_VALUE)
  ValintakoeOsallistuminenDTO haku(
      @ApiParam(value = "Hakemus OID", required = true) @PathVariable("hakemusOid")
          final String hakemusOid);

  @ApiOperation(
      value = "Hakee valintakoeosallistumiset hakukohteelle OID:n perusteella",
      response = ValintakoeOsallistuminenDTO.class)
  @GetMapping(value = "/hakutoive/{hakukohdeOid}", produces = MediaType.APPLICATION_JSON_VALUE)
  List<ValintakoeOsallistuminenDTO> hakuByHakutoive(
      @ApiParam(value = "Hakukohde OID", required = true) @PathVariable("hakukohdeOid")
          final String hakukohdeOid);

  @ApiOperation(
      value = "Hakee valintakoeosallistumiset hakukohteille OID:n perusteella",
      response = ValintakoeOsallistuminenDTO.class)
  @PostMapping(
      value = "/hakutoive",
      produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  List<ValintakoeOsallistuminenDTO> hakuByOids(
      @ApiParam(value = "Hakukohde OIDS", required = true) final List<String> hakukohdeOids);

  @ApiOperation(
      value = "Hakee valintakoeosallistumiset hakijoille OID:n perusteella",
      response = ValintakoeOsallistuminenDTO.class)
  @PostMapping(
      value = "/hakijat",
      produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  List<ValintakoeOsallistuminenDTO> hakijatByOids(@RequestBody final List<String> hakijaOids);
}
