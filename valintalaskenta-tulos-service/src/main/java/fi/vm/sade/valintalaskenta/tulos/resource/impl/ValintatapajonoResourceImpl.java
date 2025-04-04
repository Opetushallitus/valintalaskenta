package fi.vm.sade.valintalaskenta.tulos.resource.impl;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.READ_UPDATE_CRUD;
import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.UPDATE_CRUD;

import fi.vm.sade.auditlog.Changes;
import fi.vm.sade.auditlog.User;
import fi.vm.sade.service.valintaperusteet.dto.ValintatapajonoDTO;
import fi.vm.sade.valinta.sharedutils.ValintaResource;
import fi.vm.sade.valinta.sharedutils.ValintaperusteetOperation;
import fi.vm.sade.valintalaskenta.domain.dto.JonoDto;
import fi.vm.sade.valintalaskenta.domain.dto.MuokattuJonosijaArvoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.MuokattuJonosijaDTO;
import fi.vm.sade.valintalaskenta.domain.valinta.MuokattuJonosija;
import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;
import fi.vm.sade.valintalaskenta.tulos.LaskentaAudit;
import fi.vm.sade.valintalaskenta.tulos.logging.LaskentaAuditLog;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletRequest;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

@RestController
@PreAuthorize("isAuthenticated()")
@Tag(
    name = "/resources/valintatapajono",
    description = "Resurssi valintatapajonon jonosijojen muokkaamiseen manuaalisesti")
@RequestMapping("/resources/valintatapajono")
public class ValintatapajonoResourceImpl {
  private final ValintalaskentaTulosService tulosService;
  private final ValintalaskentaModelMapper modelMapper;
  private final LaskentaAuditLog auditLog;

  @Autowired
  public ValintatapajonoResourceImpl(
      final ValintalaskentaTulosService tulosService,
      final ValintalaskentaModelMapper modelMapper,
      final LaskentaAuditLog auditLog) {
    this.tulosService = tulosService;
    this.modelMapper = modelMapper;
    this.auditLog = auditLog;
  }

  @PreAuthorize(UPDATE_CRUD)
  @Operation(summary = "Muokkaa jonosijaa")
  @PostMapping(
      value = "/{valintatapajonoOid}/{hakemusOid}/{jarjestyskriteeriPrioriteetti}/jonosija",
      produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  public ResponseEntity<MuokattuJonosijaDTO> muutaJonosija(
      @Parameter(name = "valintatapajonoOid", required = true) @PathVariable("valintatapajonoOid")
          final String valintatapajonoOid,
      @Parameter(name = "hakemusOid", required = true) @PathVariable("hakemusOid")
          final String hakemusOid,
      @Parameter(name = "jarjestyskriteeriPrioriteetti", required = true)
          @PathVariable("jarjestyskriteeriPrioriteetti")
          final Integer jarjestyskriteeriPrioriteetti,
      @RequestBody final MuokattuJonosijaArvoDTO arvo,
      final HttpServletRequest request) {
    User user = auditLog.getUser(request);

    MuokattuJonosija muokattuJonosija =
        tulosService.muutaJarjestyskriteeri(
            valintatapajonoOid, hakemusOid, jarjestyskriteeriPrioriteetti, arvo, user);
    if (muokattuJonosija != null) {
      MuokattuJonosijaDTO map = modelMapper.map(muokattuJonosija, MuokattuJonosijaDTO.class);
      return ResponseEntity.accepted().body(map);
    } else {
      return ResponseEntity.notFound().build();
    }
  }

  @PreAuthorize(UPDATE_CRUD)
  @Operation(summary = "Muokkaa jonosijaa")
  @DeleteMapping(
      value = "/{valintatapajonoOid}/{hakemusOid}/{jarjestyskriteeriPrioriteetti}/jonosija",
      produces = MediaType.APPLICATION_JSON_VALUE)
  public ResponseEntity<MuokattuJonosijaDTO> poistaMuokattuJonosija(
      @Parameter(name = "valintatapajonoOid", required = true) @PathVariable("valintatapajonoOid")
          final String valintatapajonoOid,
      @Parameter(name = "hakemusOid", required = true) @PathVariable("hakemusOid")
          final String hakemusOid,
      @Parameter(name = "jarjestyskriteeriPrioriteetti", required = true)
          @PathVariable("jarjestyskriteeriPrioriteetti")
          final Integer jarjestyskriteeriPrioriteetti,
      final HttpServletRequest request) {
    User user = auditLog.getUser(request);

    MuokattuJonosija muokattuJonosija =
        tulosService.poistaMuokattuJonosija(
            valintatapajonoOid, hakemusOid, jarjestyskriteeriPrioriteetti, user);
    if (muokattuJonosija != null) {
      MuokattuJonosijaDTO map = modelMapper.map(muokattuJonosija, MuokattuJonosijaDTO.class);
      return ResponseEntity.accepted().body(map);
    } else {
      return ResponseEntity.notFound().build();
    }
  }

  @PreAuthorize(UPDATE_CRUD)
  @Operation(summary = "Tallentaa/muokkaa valintatapajonoa")
  @PutMapping(
      value = "/{valintatapajonoOid}/valmissijoiteltavaksi",
      produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  public ResponseEntity<ValintatapajonoDTO> muokkaaSijotteluStatusta(
      @Parameter(name = "valintatapajonoOid", required = true) @PathVariable("valintatapajonoOid")
          final String valintatapajonoOid,
      @Parameter(name = "status", required = true) @RequestParam("status")
          final boolean valmisSijoiteltavaksi,
      @RequestBody final ValintatapajonoDTO valintatapajono,
      final HttpServletRequest request) {
    User user = auditLog.getUser(request);

    Map<String, String> additionalAuditFields = new HashMap<>();
    additionalAuditFields.put("valmissijoiteltavaksi", String.valueOf(valmisSijoiteltavaksi));
    auditLog.log(
        LaskentaAudit.AUDIT,
        auditLog.getUser(request),
        ValintaperusteetOperation.VALINTATAPAJONO_PAIVITYS,
        ValintaResource.VALINTATAPAJONO,
        valintatapajono.getOid(),
        Changes.addedDto(valintatapajono),
        additionalAuditFields);

    Optional<Valintatapajono> dto =
        tulosService.paivitaValmisSijoiteltavaksi(valintatapajonoOid, valmisSijoiteltavaksi);
    return dto.map(
            jono -> ResponseEntity.accepted().body(modelMapper.map(jono, ValintatapajonoDTO.class)))
        .orElse(ResponseEntity.notFound().build());
  }

  @PreAuthorize(READ_UPDATE_CRUD)
  @GetMapping(
      value = "/{valintatapajonoOid}/valmissijoiteltavaksi",
      produces = MediaType.APPLICATION_JSON_VALUE)
  public ResponseEntity<Map<String, Boolean>> haeSijoitteluStatus(
      @Parameter(name = "valintatapajonoOid", required = true) @PathVariable("valintatapajonoOid")
          String oid) {
    HashMap<String, Boolean> object = new HashMap<>();
    object.put("value", tulosService.haeSijoitteluStatus(oid));
    return ResponseEntity.accepted().body(object);
  }

  @PreAuthorize(READ_UPDATE_CRUD)
  @GetMapping(value = "/jonotsijoittelussa/{hakuOid}", produces = MediaType.APPLICATION_JSON_VALUE)
  public List<JonoDto> jonotSijoittelussa(@PathVariable("hakuOid") String hakuOid) {
    return tulosService.haeJonotSijoittelussa(hakuOid);
  }
}
