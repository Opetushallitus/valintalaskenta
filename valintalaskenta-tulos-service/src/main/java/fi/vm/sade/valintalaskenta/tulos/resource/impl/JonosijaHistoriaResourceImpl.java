package fi.vm.sade.valintalaskenta.tulos.resource.impl;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.READ_UPDATE_CRUD;

import fi.vm.sade.valintalaskenta.domain.dto.JarjestyskriteerihistoriaDTO;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import fi.vm.sade.valintalaskenta.tulos.resource.JonosijaHistoriaResource;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import java.util.List;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
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
@Tag(name = "/jonosijahistoria", description = "Resurssi jonosijahistoriatiedon hakemiseen")
public class JonosijaHistoriaResourceImpl implements JonosijaHistoriaResource {
  protected static final Logger logger =
      LoggerFactory.getLogger(JonosijaHistoriaResourceImpl.class);
  private final ValintalaskentaTulosService tulosService;
  private final ValintalaskentaModelMapper modelMapper;

  @Autowired
  public JonosijaHistoriaResourceImpl(
      final ValintalaskentaTulosService tulosService,
      final ValintalaskentaModelMapper modelMapper) {
    this.tulosService = tulosService;
    this.modelMapper = modelMapper;
  }

  @PreAuthorize(READ_UPDATE_CRUD)
  @Operation(
      summary = "Hakee jonosijahistoriat valintatapajono OID:n ja hakemus OID:n perusteella")
  @GetMapping(
      value = "{valintatapajonoOid}/{hakemusOid}",
      produces = MediaType.APPLICATION_JSON_VALUE)
  public List<JarjestyskriteerihistoriaDTO> listJonosijaHistoria(
      @Parameter(name = "Valintatapajono OID", required = true) @PathVariable("valintatapajonoOid")
          final String valintatapajonoOid,
      @Parameter(name = "Hakemus OID", required = true) @PathVariable("hakemusOid")
          final String hakemusOid) {
    return modelMapper.mapList(
        tulosService.haeJonosijaHistoria(valintatapajonoOid, hakemusOid),
        JarjestyskriteerihistoriaDTO.class);
  }
}
