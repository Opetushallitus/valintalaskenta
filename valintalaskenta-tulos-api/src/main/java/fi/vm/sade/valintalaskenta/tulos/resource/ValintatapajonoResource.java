package fi.vm.sade.valintalaskenta.tulos.resource;

import fi.vm.sade.service.valintaperusteet.dto.ValintatapajonoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.JonoDto;
import fi.vm.sade.valintalaskenta.domain.dto.MuokattuJonosijaArvoDTO;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import java.util.List;
import javax.servlet.http.HttpServletRequest;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@Tag(
    name = "/valintatapajono",
    description = "Resurssi valintatapajonon jonosijojen muokkaamiseen manuaalisesti")
@RequestMapping("/valintatapajono")
public interface ValintatapajonoResource {
  @Operation(summary = "Muokkaa jonosijaa")
  @PostMapping(
      value = "/{valintatapajonoOid}/{hakemusOid}/{jarjestyskriteeriPrioriteetti}/jonosija",
      produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  ResponseEntity<Object> muutaJonosija(
      @Parameter(name = "Valintatapajonon OID", required = true) @PathVariable("valintatapajonoOid")
          final String valintatapajonoOid,
      @Parameter(name = "Hakemus OID", required = true) @PathVariable("hakemusOid")
          final String hakemusOid,
      @Parameter(name = "Muokattavan järjestyskriteerin prioriteetti", required = true)
          @PathVariable("jarjestyskriteeriPrioriteetti")
          final Integer jarjestyskriteeriPrioriteetti,
      @Parameter(name = "Järjestyskriteerin uusi arvo", required = true) @RequestBody
          final MuokattuJonosijaArvoDTO arvo,
      final HttpServletRequest request);

  @Operation(summary = "Poista muokattu jonosijaa")
  @DeleteMapping(
      value = "/{valintatapajonoOid}/{hakemusOid}/{jarjestyskriteeriPrioriteetti}/jonosija",
      produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  ResponseEntity<Object> poistaMuokattuJonosija(
      @Parameter(name = "Valintatapajonon OID", required = true) @PathVariable("valintatapajonoOid")
          final String valintatapajonoOid,
      @Parameter(name = "Hakemus OID", required = true) @PathVariable("hakemusOid")
          final String hakemusOid,
      @Parameter(name = "Muokattavan järjestyskriteerin prioriteetti", required = true)
          @PathVariable("jarjestyskriteeriPrioriteetti")
          final Integer jarjestyskriteeriPrioriteetti,
      final HttpServletRequest request);

  @Operation(summary = "Tallentaa/muokkaa valintatapajonoa")
  @PutMapping(
      value = "/{valintatapajonoOid}/valmissijoiteltavaksi",
      produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  ResponseEntity<ValintatapajonoDTO> muokkaaSijotteluStatusta(
      @Parameter(name = "Valintatapajonon OID", required = true) @PathVariable("valintatapajonoOid")
          final String valintatapajonoOid,
      @Parameter(name = "Sijoittelustatus", required = true) @RequestParam("status")
          final boolean status,
      @Parameter(name = "Valintatapajono", required = true) final ValintatapajonoDTO valintapajono,
      final HttpServletRequest request);

  @GetMapping(
      value = "/{valintatapajonoOid}/valmissijoiteltavaksi",
      produces = MediaType.APPLICATION_JSON_VALUE)
  ResponseEntity<Object> haeSijoitteluStatus(
      @Parameter(name = "Valintatapajonon OID", required = true) @PathVariable("valintatapajonoOid")
          final String oid);

  @GetMapping(value = "/jonotsijoittelussa/{hakuOid}", produces = MediaType.APPLICATION_JSON_VALUE)
  List<JonoDto> jonotSijoittelussa(@PathVariable("hakuOid") final String hakuOid);
}
