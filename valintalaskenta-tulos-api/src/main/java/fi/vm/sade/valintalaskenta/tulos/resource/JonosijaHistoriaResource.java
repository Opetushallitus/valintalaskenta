package fi.vm.sade.valintalaskenta.tulos.resource;

import fi.vm.sade.valintalaskenta.domain.dto.JarjestyskriteerihistoriaDTO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

import java.util.List;

@Api(value = "/jonosijahistoria", description = "Resurssi jonosijahistoriatiedon hakemiseen")
public interface JonosijaHistoriaResource {
  @ApiOperation(
      value = "Hakee jonosijahistoriat valintatapajono OID:n ja hakemus OID:n perusteella",
      response = JarjestyskriteerihistoriaDTO.class)
  @GetMapping(value = "{valintatapajonoOid}/{hakemusOid}", produces = MediaType.APPLICATION_JSON_VALUE)
  List<JarjestyskriteerihistoriaDTO> listJonosijaHistoria(
          @ApiParam(value = "Valintatapajono OID", required = true) @PathVariable("valintatapajonoOid")
          String valintatapajonoOid,
      @ApiParam(value = "Hakemus OID", required = true) @PathVariable("hakemusOid") String hakemusOid);
}
