package fi.vm.sade.valintalaskenta.tulos.resource;

import fi.vm.sade.valintalaskenta.domain.dto.JarjestyskriteerihistoriaDTO;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import java.util.List;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;

@Tag(name = "/jonosijahistoria", description = "Resurssi jonosijahistoriatiedon hakemiseen")
@RequestMapping("/jonosijahistoria")
public interface JonosijaHistoriaResource {
  @Operation(summary = "Hakee jonosijahistoriat valintatapajono OID:n ja hakemus OID:n perusteella")
  @GetMapping(
      value = "/{valintatapajonoOid}/{hakemusOid}",
      produces = MediaType.APPLICATION_JSON_VALUE)
  List<JarjestyskriteerihistoriaDTO> listJonosijaHistoria(
      @Parameter(name = "Valintatapajono OID", required = true) @PathVariable("valintatapajonoOid")
          String valintatapajonoOid,
      @Parameter(name = "Hakemus OID", required = true) @PathVariable("hakemusOid")
          String hakemusOid);
}
