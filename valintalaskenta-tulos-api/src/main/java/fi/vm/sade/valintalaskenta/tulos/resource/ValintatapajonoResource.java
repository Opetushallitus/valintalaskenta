package fi.vm.sade.valintalaskenta.tulos.resource;

import fi.vm.sade.service.valintaperusteet.dto.ValintatapajonoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.JonoDto;
import fi.vm.sade.valintalaskenta.domain.dto.MuokattuJonosijaArvoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.MuokattuJonosijaDTO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import java.util.List;
import javax.servlet.http.HttpServletRequest;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@Api(
    value = "/valintatapajono",
    description = "Resurssi valintatapajonon jonosijojen muokkaamiseen manuaalisesti")
@RequestMapping("/valintatapajono")
public interface ValintatapajonoResource {
  @ApiOperation(value = "Muokkaa jonosijaa", response = MuokattuJonosijaDTO.class)
  @PostMapping(
      value = "/{valintatapajonoOid}/{hakemusOid}/{jarjestyskriteeriPrioriteetti}/jonosija",
      produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  ResponseEntity<Object> muutaJonosija(
      @ApiParam(value = "Valintatapajonon OID", required = true) @PathVariable("valintatapajonoOid")
          final String valintatapajonoOid,
      @ApiParam(value = "Hakemus OID", required = true) @PathVariable("hakemusOid")
          final String hakemusOid,
      @ApiParam(value = "Muokattavan järjestyskriteerin prioriteetti", required = true)
          @PathVariable("jarjestyskriteeriPrioriteetti")
          final Integer jarjestyskriteeriPrioriteetti,
      @ApiParam(value = "Järjestyskriteerin uusi arvo", required = true) @RequestBody
          final MuokattuJonosijaArvoDTO arvo,
      final HttpServletRequest request);

  @ApiOperation(value = "Poista muokattu jonosijaa", response = MuokattuJonosijaDTO.class)
  @DeleteMapping(
      value = "/{valintatapajonoOid}/{hakemusOid}/{jarjestyskriteeriPrioriteetti}/jonosija",
      produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  ResponseEntity<Object> poistaMuokattuJonosija(
      @ApiParam(value = "Valintatapajonon OID", required = true) @PathVariable("valintatapajonoOid")
          final String valintatapajonoOid,
      @ApiParam(value = "Hakemus OID", required = true) @PathVariable("hakemusOid")
          final String hakemusOid,
      @ApiParam(value = "Muokattavan järjestyskriteerin prioriteetti", required = true)
          @PathVariable("jarjestyskriteeriPrioriteetti")
          final Integer jarjestyskriteeriPrioriteetti,
      final HttpServletRequest request);

  @ApiOperation(value = "Tallentaa/muokkaa valintatapajonoa", response = ValintatapajonoDTO.class)
  @PutMapping(
      value = "/{valintatapajonoOid}/valmissijoiteltavaksi",
      produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  ResponseEntity<ValintatapajonoDTO> muokkaaSijotteluStatusta(
      @ApiParam(value = "Valintatapajonon OID", required = true) @PathVariable("valintatapajonoOid")
          final String valintatapajonoOid,
      @ApiParam(value = "Sijoittelustatus", required = true) @RequestParam("status")
          final boolean status,
      @ApiParam(value = "Valintatapajono", required = true) final ValintatapajonoDTO valintapajono,
      final HttpServletRequest request);

  @GetMapping(
      value = "/{valintatapajonoOid}/valmissijoiteltavaksi",
      produces = MediaType.APPLICATION_JSON_VALUE)
  ResponseEntity<Object> haeSijoitteluStatus(
      @ApiParam(value = "Valintatapajonon OID", required = true) @PathVariable("valintatapajonoOid")
          final String oid);

  @GetMapping(value = "/jonotsijoittelussa/{hakuOid}", produces = MediaType.APPLICATION_JSON_VALUE)
  List<JonoDto> jonotSijoittelussa(@PathVariable("hakuOid") final String hakuOid);
}
