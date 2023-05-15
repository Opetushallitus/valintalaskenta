package fi.vm.sade.valintalaskenta.tulos.resource.impl;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.READ_UPDATE_CRUD;

import fi.vm.sade.valintalaskenta.domain.dto.JarjestyskriteerihistoriaDTO;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import fi.vm.sade.valintalaskenta.tulos.resource.JonosijaHistoriaResource;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

@Controller
@PreAuthorize("isAuthenticated()")
@Api(value = "/jonosijahistoria", description = "Resurssi jonosijahistoriatiedon hakemiseen")
public class JonosijaHistoriaResourceImpl implements JonosijaHistoriaResource {
  protected static final Logger logger =
      LoggerFactory.getLogger(JonosijaHistoriaResourceImpl.class);

  @Autowired private ValintalaskentaTulosService tulosService;

  @Autowired private ValintalaskentaModelMapper modelMapper;

  @PreAuthorize(READ_UPDATE_CRUD)
  @ApiOperation(
      value = "Hakee jonosijahistoriat valintatapajono OID:n ja hakemus OID:n perusteella",
      response = JarjestyskriteerihistoriaDTO.class)
  @GetMapping(value = "{valintatapajonoOid}/{hakemusOid}", produces = MediaType.APPLICATION_JSON_VALUE)
  public List<JarjestyskriteerihistoriaDTO> listJonosijaHistoria(
      @ApiParam(value = "Valintatapajono OID", required = true) @PathVariable("valintatapajonoOid")
          String valintatapajonoOid,
      @ApiParam(value = "Hakemus OID", required = true) @PathVariable("hakemusOid")
          String hakemusOid) {
    return modelMapper.mapList(
        tulosService.haeJonosijaHistoria(valintatapajonoOid, hakemusOid),
        JarjestyskriteerihistoriaDTO.class);
  }
}
