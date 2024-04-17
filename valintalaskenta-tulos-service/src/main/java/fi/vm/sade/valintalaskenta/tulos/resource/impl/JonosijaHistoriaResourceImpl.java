package fi.vm.sade.valintalaskenta.tulos.resource.impl;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.READ_UPDATE_CRUD;

import fi.vm.sade.valintalaskenta.domain.dto.JarjestyskriteerihistoriaDTO;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import fi.vm.sade.valintalaskenta.tulos.service.JarjestyskriteerihistoriaService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@PreAuthorize("isAuthenticated()")
@Tag(
    name = "/resources/jonosijahistoria",
    description = "Resurssi jonosijahistoriatiedon hakemiseen")
@RequestMapping(value = "/resources/jonosijahistoria")
public class JonosijaHistoriaResourceImpl {
  protected static final Logger logger =
      LoggerFactory.getLogger(JonosijaHistoriaResourceImpl.class);
  private final JarjestyskriteerihistoriaService historiaService;
  private final ValintalaskentaModelMapper modelMapper;

  @Autowired
  public JonosijaHistoriaResourceImpl(
      final JarjestyskriteerihistoriaService historiaService,
      final ValintalaskentaModelMapper modelMapper) {
    this.historiaService = historiaService;
    this.modelMapper = modelMapper;
  }

  @PreAuthorize(READ_UPDATE_CRUD)
  @Operation(summary = "Hakee jonosijahistoriat valintatapajono OID:n ja hakemus OID:n perusteella")
  @GetMapping(
      value = "{valintatapajonoOid}/{hakemusOid}",
      produces = MediaType.APPLICATION_JSON_VALUE)
  public List<JarjestyskriteerihistoriaDTO> listJonosijaHistoria(
      @Parameter(name = "valintatapajonoOid", required = true) @PathVariable("valintatapajonoOid")
          final String valintatapajonoOid,
      @Parameter(name = "hakemusOid", required = true) @PathVariable("hakemusOid")
          final String hakemusOid) {
    return modelMapper.mapList(
        historiaService.findByValintatapajonoAndHakemusOid(valintatapajonoOid, hakemusOid),
        JarjestyskriteerihistoriaDTO.class);
  }
}
