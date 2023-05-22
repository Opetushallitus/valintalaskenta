package fi.vm.sade.valintalaskenta.tulos.resource;

import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeOsallistuminenDTO;
import java.util.List;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;

@Tag(name = "/valintakoe", description = "Resurssi valintakoeosallistumistulosten hakemiseen")
@RequestMapping("/valintakoe")
public interface ValintakoeResource {
  @Operation(
      summary = "Hakee valintakoeosallistumiset hakemukselle OID:n perusteella")
  @GetMapping(value = "/hakemus/{hakemusOid}", produces = MediaType.APPLICATION_JSON_VALUE)
  ValintakoeOsallistuminenDTO haku(
      @Parameter(name = "Hakemus OID", required = true) @PathVariable("hakemusOid")
          final String hakemusOid);

  @Operation(
      summary = "Hakee valintakoeosallistumiset hakukohteelle OID:n perusteella")
  @GetMapping(value = "/hakutoive/{hakukohdeOid}", produces = MediaType.APPLICATION_JSON_VALUE)
  List<ValintakoeOsallistuminenDTO> hakuByHakutoive(
      @Parameter(name = "Hakukohde OID", required = true) @PathVariable("hakukohdeOid")
          final String hakukohdeOid);

  @Operation(
      summary = "Hakee valintakoeosallistumiset hakukohteille OID:n perusteella")
  @PostMapping(
      value = "/hakutoive",
      produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  List<ValintakoeOsallistuminenDTO> hakuByOids(
      @Parameter(name = "Hakukohde OIDS", required = true) final List<String> hakukohdeOids);

  @Operation(
      summary = "Hakee valintakoeosallistumiset hakijoille OID:n perusteella")
  @PostMapping(
      value = "/hakijat",
      produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  List<ValintakoeOsallistuminenDTO> hakijatByOids(@RequestBody final List<String> hakijaOids);
}
