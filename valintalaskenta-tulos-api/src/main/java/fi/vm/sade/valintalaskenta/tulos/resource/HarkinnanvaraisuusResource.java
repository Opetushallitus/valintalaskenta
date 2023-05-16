package fi.vm.sade.valintalaskenta.tulos.resource;

import fi.vm.sade.valintalaskenta.domain.dto.HarkinnanvarainenHyvaksyminenDTO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import java.util.List;
import javax.servlet.http.HttpServletRequest;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;

@Api(
    value = "/harkinnanvarainenhyvaksynta",
    description = "Resurssi harkinnanvaraisesti hakeneiden hakijoiden käsittelyyn")
@Controller()
@RequestMapping(value = "/harkinnanvarainenhyvaksynta")
public interface HarkinnanvaraisuusResource {
  @ApiOperation(value = "Asettaa tilan harkinnanvaraisesti hakeneelle hakijalle")
  @PostMapping(
      value = "/haku/{hakuOid}/hakukohde/{hakukohdeOid}/hakemus/{hakemusOid}",
      produces = MediaType.APPLICATION_JSON_VALUE)
  void asetaTila(
      @ApiParam(value = "Haun OID", required = true) @PathVariable("hakuOid") String hakuOid,
      @ApiParam(value = "Hakukohteen OID", required = true) @PathVariable("hakukohdeOid")
          String hakukohdeOid,
      @ApiParam(value = "Hakemuksen OID", required = true) @PathVariable("hakemusOid")
          String hakemusOid,
      @ApiParam(value = "Asetettava tila", required = true)
          HarkinnanvarainenHyvaksyminenDTO harkinnanvarainenHyvaksyminen,
      final HttpServletRequest request);

  @ApiOperation(value = "Asettaa tilan harkinnanvaraisesti hakeneelle hakijalle")
  @PostMapping(produces = MediaType.APPLICATION_JSON_VALUE)
  void asetaTilat(
      @ApiParam(value = "Asetettava tila", required = true)
          List<HarkinnanvarainenHyvaksyminenDTO> harkinnanvaraisetHyvaksymiset,
      final HttpServletRequest request);

  @ApiOperation(
      value = "Hakee hakukohteen harkinnanvaraisesti hakeneiden hakijoiden tilat",
      response = HarkinnanvarainenHyvaksyminenDTO.class)
  @GetMapping(
      value = "/haku/{hakuOid}/hakukohde/{hakukohdeOid}",
      produces = MediaType.APPLICATION_JSON_VALUE)
  List<HarkinnanvarainenHyvaksyminenDTO> hakukohde(
      @ApiParam(value = "Haku OID", required = true) @PathVariable("hakuOid") String hakuOid,
      @ApiParam(value = "Hakukohde OID", required = true) @PathVariable("hakukohdeOid")
          String hakukohdeOid);

  @ApiOperation(
      value = "Hakee hakemuksen harkinnanvaraisesti tilat",
      response = HarkinnanvarainenHyvaksyminenDTO.class)
  @GetMapping(
      value = "/haku/{hakuOid}/hakemus/{hakemusOid}",
      produces = MediaType.APPLICATION_JSON_VALUE)
  List<HarkinnanvarainenHyvaksyminenDTO> hakemus(
      @ApiParam(value = "Haku OID", required = true) @PathVariable("hakuOid") String hakuOid,
      @ApiParam(value = "Hakemus OID", required = true) @PathVariable("hakemusOid")
          String hakemusOid);
}
