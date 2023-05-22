package fi.vm.sade.valintalaskenta.tulos.resource;

import fi.vm.sade.valintalaskenta.domain.dto.HakijaryhmaDTO;
import fi.vm.sade.valintalaskenta.domain.dto.ValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.ValintatietoValinnanvaiheDTO;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import java.util.List;
import javax.servlet.http.HttpServletRequest;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@Tag(name = "/hakukohde", description = "Resurssi tulosten hakemiseen hakukohteittain")
@RequestMapping(value = "/hakukohde")
public interface HakukohdeResource {
  @Operation(summary = "Hakee hakukohteen valinnan vaiheiden tulokset")
  @GetMapping(value = "/{hakukohdeoid}/valinnanvaihe", produces = MediaType.APPLICATION_JSON_VALUE)
  List<ValintatietoValinnanvaiheDTO> hakukohde(
      @Parameter(name = "Hakukohteen OID", required = true) @PathVariable("hakukohdeoid")
          String hakukohdeoid);

  @Operation(summary = "Lisää tuloksia valinnanvaiheelle")
  @PostMapping(
      value = "/{hakukohdeoid}/valinnanvaihe",
      produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  ResponseEntity<Object> lisaaTuloksia(
      @Parameter(name = "Hakukohteen OID", required = true) @PathVariable("hakukohdeoid")
          String hakukohdeoid,
      @Parameter(name = "Tarjoaja OID", required = true) @RequestParam("tarjoajaOid")
          String tarjoajaOid,
      @Parameter(name = "Muokattava valinnanvaihe", required = true) ValinnanvaiheDTO vaihe,
      final HttpServletRequest request);

  @Operation(summary = "Hakee hakukohteen hakijaryhmien tulokset")
  @GetMapping(value = "/{hakukohdeoid}/hakijaryhma", produces = MediaType.APPLICATION_JSON_VALUE)
  List<HakijaryhmaDTO> hakijaryhmat(
      @Parameter(name = "Hakukohteen OID", required = true) @PathVariable("hakukohdeoid")
          String hakukohdeoid);
}
