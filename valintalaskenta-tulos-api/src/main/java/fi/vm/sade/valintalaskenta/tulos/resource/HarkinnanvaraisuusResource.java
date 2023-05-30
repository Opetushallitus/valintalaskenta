package fi.vm.sade.valintalaskenta.tulos.resource;

import fi.vm.sade.valintalaskenta.domain.dto.HarkinnanvarainenHyvaksyminenDTO;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import java.util.List;
import javax.servlet.http.HttpServletRequest;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;

@Tag(
    name = "/resources/harkinnanvarainenhyvaksynta",
    description = "Resurssi harkinnanvaraisesti hakeneiden hakijoiden käsittelyyn")
@RestController
@RequestMapping(value = "/resources/harkinnanvarainenhyvaksynta")
public interface HarkinnanvaraisuusResource {
  @Operation(summary = "Asettaa tilan harkinnanvaraisesti hakeneelle hakijalle")
  @PostMapping(
      value = "/haku/{hakuOid}/hakukohde/{hakukohdeOid}/hakemus/{hakemusOid}",
      produces = MediaType.APPLICATION_JSON_VALUE)
  void asetaTila(
      @Parameter(name = "Haun OID", required = true) @PathVariable("hakuOid") String hakuOid,
      @Parameter(name = "Hakukohteen OID", required = true) @PathVariable("hakukohdeOid")
          String hakukohdeOid,
      @Parameter(name = "Hakemuksen OID", required = true) @PathVariable("hakemusOid")
          String hakemusOid,
      @Parameter(name = "Asetettava tila", required = true)
          HarkinnanvarainenHyvaksyminenDTO harkinnanvarainenHyvaksyminen,
      final HttpServletRequest request);

  @Operation(summary = "Asettaa tilan harkinnanvaraisesti hakeneelle hakijalle")
  @PostMapping(produces = MediaType.APPLICATION_JSON_VALUE)
  void asetaTilat(
      @Parameter(name = "Asetettava tila", required = true)
          List<HarkinnanvarainenHyvaksyminenDTO> harkinnanvaraisetHyvaksymiset,
      final HttpServletRequest request);

  @Operation(summary = "Hakee hakukohteen harkinnanvaraisesti hakeneiden hakijoiden tilat")
  @GetMapping(
      value = "/haku/{hakuOid}/hakukohde/{hakukohdeOid}",
      produces = MediaType.APPLICATION_JSON_VALUE)
  List<HarkinnanvarainenHyvaksyminenDTO> hakukohde(
      @Parameter(name = "Haku OID", required = true) @PathVariable("hakuOid") String hakuOid,
      @Parameter(name = "Hakukohde OID", required = true) @PathVariable("hakukohdeOid")
          String hakukohdeOid);

  @Operation(summary = "Hakee hakemuksen harkinnanvaraisesti tilat")
  @GetMapping(
      value = "/haku/{hakuOid}/hakemus/{hakemusOid}",
      produces = MediaType.APPLICATION_JSON_VALUE)
  List<HarkinnanvarainenHyvaksyminenDTO> hakemus(
      @Parameter(name = "Haku OID", required = true) @PathVariable("hakuOid") String hakuOid,
      @Parameter(name = "Hakemus OID", required = true) @PathVariable("hakemusOid")
          String hakemusOid);
}
