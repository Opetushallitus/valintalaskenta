package fi.vm.sade.valintalaskenta.tulos.resource;

import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;

@Api(value = "/hakemus", description = "Resurssi hakemuskohtaisten tulosten hakemiseen")
@RequestMapping(value = "/hakemus")
public interface HakemusResource {
  @ApiOperation(
      value = "Hakee hakemuksen tulokset haku OID:n ja hakemuksen OID:n perustella",
      response = HakemusDTO.class)
  @GetMapping(value = "/{hakuoid}/{hakemusoid}", produces = MediaType.APPLICATION_JSON_VALUE)
  HakemusDTO hakemus(
      @ApiParam(value = "Haku OID", required = true) @PathVariable("hakuoid") String hakuoid,
      @ApiParam(value = "Hakemus OID", required = true) @PathVariable("hakemusoid")
          String hakemusoid);
}
