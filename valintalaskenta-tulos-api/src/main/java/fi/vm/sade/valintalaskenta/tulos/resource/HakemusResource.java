package fi.vm.sade.valintalaskenta.tulos.resource;

import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;

@Tag(name = "/hakemus", description = "Resurssi hakemuskohtaisten tulosten hakemiseen")
@RequestMapping(value = "/hakemus")
public interface HakemusResource {
  @Operation(summary = "Hakee hakemuksen tulokset haku OID:n ja hakemuksen OID:n perustella")
  @GetMapping(value = "/{hakuoid}/{hakemusoid}", produces = MediaType.APPLICATION_JSON_VALUE)
  HakemusDTO hakemus(
      @Parameter(name = "Haku OID", required = true) @PathVariable("hakuoid") String hakuoid,
      @Parameter(name = "Hakemus OID", required = true) @PathVariable("hakemusoid")
          String hakemusoid);
}
