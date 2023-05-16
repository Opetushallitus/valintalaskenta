package fi.vm.sade.valintalaskenta.tulos.resource;

import fi.vm.sade.valintalaskenta.domain.dto.HakukohdeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.MinimalJonoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeOsallistuminenDTO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import java.util.List;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;

@Api(value = "/haku", description = "Resurssi haun valintalaskennan virhetilanteiden hakemiseen")
@RequestMapping(value = "/haku")
public interface HakuResource {
  @ApiOperation(
      value = "Hakee haun valintalaskennan virhetilanteet OID:n perusteella",
      response = HakukohdeDTO.class)
  @GetMapping(value = "/{hakuOid}/virheet", produces = MediaType.APPLICATION_JSON_VALUE)
  List<HakukohdeDTO> virheet(@PathVariable("hakuOid") String hakuOid);

  @GetMapping(value = "/{hakuOid}/valintakoevirheet", produces = MediaType.APPLICATION_JSON_VALUE)
  List<ValintakoeOsallistuminenDTO> valintakoevirheet(@PathVariable("hakuOid") String hakuOid);

  @ApiOperation(
      value = "Hakee sijoitteluun siirretyt valintalaskennattomat valintatapajonot ODWlle",
      response = MinimalJonoDTO.class)
  @GetMapping(
      value = "/ilmanvalintalaskentaasijoitteluun",
      produces = MediaType.APPLICATION_JSON_VALUE)
  List<MinimalJonoDTO> jonotSijoitteluun();
}
