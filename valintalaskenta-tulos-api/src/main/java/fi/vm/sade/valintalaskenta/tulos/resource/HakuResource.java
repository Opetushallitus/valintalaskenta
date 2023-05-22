package fi.vm.sade.valintalaskenta.tulos.resource;

import fi.vm.sade.valintalaskenta.domain.dto.HakukohdeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.MinimalJonoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeOsallistuminenDTO;
import java.util.List;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;

@Tag(name = "/haku", description = "Resurssi haun valintalaskennan virhetilanteiden hakemiseen")
@RequestMapping(value = "/haku")
public interface HakuResource {
  @Operation(
      summary = "Hakee haun valintalaskennan virhetilanteet OID:n perusteella")
  @GetMapping(value = "/{hakuOid}/virheet", produces = MediaType.APPLICATION_JSON_VALUE)
  List<HakukohdeDTO> virheet(@PathVariable("hakuOid") String hakuOid);

  @GetMapping(value = "/{hakuOid}/valintakoevirheet", produces = MediaType.APPLICATION_JSON_VALUE)
  List<ValintakoeOsallistuminenDTO> valintakoevirheet(@PathVariable("hakuOid") String hakuOid);

  @Operation(
      summary = "Hakee sijoitteluun siirretyt valintalaskennattomat valintatapajonot ODWlle")
  @GetMapping(
      value = "/ilmanvalintalaskentaasijoitteluun",
      produces = MediaType.APPLICATION_JSON_VALUE)
  List<MinimalJonoDTO> jonotSijoitteluun();
}
