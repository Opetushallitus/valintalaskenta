package fi.vm.sade.valintalaskenta.tulos.resource;

import fi.vm.sade.valintalaskenta.domain.dto.HakijaryhmaDTO;
import fi.vm.sade.valintalaskenta.domain.dto.ValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.ValintatietoValinnanvaiheDTO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import java.util.List;
import javax.servlet.http.HttpServletRequest;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@Api(value = "/hakukohde", description = "Resurssi tulosten hakemiseen hakukohteittain")
@RequestMapping(value = "/hakukohde")
public interface HakukohdeResource {
  @ApiOperation(
      value = "Hakee hakukohteen valinnan vaiheiden tulokset",
      response = ValinnanvaiheDTO.class)
  @GetMapping(value = "/{hakukohdeoid}/valinnanvaihe", produces = MediaType.APPLICATION_JSON_VALUE)
  List<ValintatietoValinnanvaiheDTO> hakukohde(
      @ApiParam(value = "Hakukohteen OID", required = true) @PathVariable("hakukohdeoid")
          String hakukohdeoid);

  @ApiOperation(value = "Lisää tuloksia valinnanvaiheelle", response = ValinnanvaiheDTO.class)
  @PostMapping(
      value = "/{hakukohdeoid}/valinnanvaihe",
      produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  ResponseEntity<Object> lisaaTuloksia(
      @ApiParam(value = "Hakukohteen OID", required = true) @PathVariable("hakukohdeoid")
          String hakukohdeoid,
      @ApiParam(value = "Tarjoaja OID", required = true) @RequestParam("tarjoajaOid")
          String tarjoajaOid,
      @ApiParam(value = "Muokattava valinnanvaihe", required = true) ValinnanvaiheDTO vaihe,
      final HttpServletRequest request);

  @ApiOperation(value = "Hakee hakukohteen hakijaryhmien tulokset", response = HakijaryhmaDTO.class)
  @GetMapping(value = "/{hakukohdeoid}/hakijaryhma", produces = MediaType.APPLICATION_JSON_VALUE)
  List<HakijaryhmaDTO> hakijaryhmat(
      @ApiParam(value = "Hakukohteen OID", required = true) @PathVariable("hakukohdeoid")
          String hakukohdeoid);
}
