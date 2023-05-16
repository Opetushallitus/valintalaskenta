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
import fi.vm.sade.valintalaskenta.tulos.resource.ValintatapajonoResource;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;
import javax.servlet.http.HttpServletRequest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;

@Controller
@PreAuthorize("isAuthenticated()")
@Api(
    value = "/valintatapajono",
    description = "Resurssi valintatapajonon jonosijojen muokkaamiseen manuaalisesti")
@RequestMapping("/valintatapajono")
public class ValintatapajonoResourceImpl implements ValintatapajonoResource {
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
  @ApiOperation(value = "Muokkaa jonosijaa", response = MuokattuJonosijaDTO.class)
  @PostMapping(
      value = "/{valintatapajonoOid}/{hakemusOid}/{jarjestyskriteeriPrioriteetti}/jonosija",
      produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  public ResponseEntity<Object> muutaJonosija(
      @ApiParam(value = "Valintatapajonon OID", required = true) @PathVariable("valintatapajonoOid")
          final String valintatapajonoOid,
      @ApiParam(value = "Hakemus OID", required = true) @PathVariable("hakemusOid")
          final String hakemusOid,
      @ApiParam(value = "Muokattavan järjestyskriteerin prioriteetti", required = true)
          @PathVariable("jarjestyskriteeriPrioriteetti")
          final Integer jarjestyskriteeriPrioriteetti,
      @ApiParam(value = "Järjestyskriteerin uusi arvo", required = true) @RequestBody
          final MuokattuJonosijaArvoDTO arvo,
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
  @ApiOperation(value = "Muokkaa jonosijaa", response = MuokattuJonosijaDTO.class)
  @DeleteMapping(
      value = "/{valintatapajonoOid}/{hakemusOid}/{jarjestyskriteeriPrioriteetti}/jonosija",
      produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  public ResponseEntity<Object> poistaMuokattuJonosija(
      @ApiParam(value = "Valintatapajonon OID", required = true) @PathVariable("valintatapajonoOid")
          final String valintatapajonoOid,
      @ApiParam(value = "Hakemus OID", required = true) @PathVariable("hakemusOid")
          final String hakemusOid,
      @ApiParam(value = "Muokattavan järjestyskriteerin prioriteetti", required = true)
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
  @ApiOperation(value = "Tallentaa/muokkaa valintatapajonoa", response = ValintatapajonoDTO.class)
  @PutMapping(
      value = "/{valintatapajonoOid}/valmissijoiteltavaksi",
      produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  public ResponseEntity<ValintatapajonoDTO> muokkaaSijotteluStatusta(
      @ApiParam(value = "Valintatapajonon OID", required = true) @PathVariable("valintatapajonoOid")
          final String valintatapajonoOid,
      @ApiParam(value = "Sijoittelustatus", required = true) @RequestParam("status")
          final boolean status,
      @ApiParam(value = "Valintatapajono", required = true)
          final ValintatapajonoDTO valintatapajono,
      final HttpServletRequest request) {
    User user = auditLog.getUser(request);

    Consumer<Valintatapajono> valintatapajonoMuokkausFunktio =
        jono -> {
          // Käyttöliittymä kutsuu
          // ValintaperusteetResourceV2::updateAutomaattinenSijoitteluunSiirto(valintatapajonoOid,
          // status, request)
          jono.setAloituspaikat(valintatapajono.getAloituspaikat());
          jono.setEiVarasijatayttoa(valintatapajono.getEiVarasijatayttoa());
          jono.setKaikkiEhdonTayttavatHyvaksytaan(
              valintatapajono.getKaikkiEhdonTayttavatHyvaksytaan());
          jono.setKaytetaanValintalaskentaa(valintatapajono.getKaytetaanValintalaskentaa());
          jono.setNimi(valintatapajono.getNimi());
          jono.setPoissaOlevaTaytto(valintatapajono.getPoissaOlevaTaytto());
          jono.setPrioriteetti(valintatapajono.getPrioriteetti());
          jono.setSiirretaanSijoitteluun(valintatapajono.getSiirretaanSijoitteluun());
          jono.setValmisSijoiteltavaksi(status);
        };

    Map<String, String> additionalAuditFields = new HashMap<>();
    additionalAuditFields.put("valmissijoiteltavaksi", String.valueOf(status));
    auditLog.log(
        LaskentaAudit.AUDIT,
        auditLog.getUser(request),
        ValintaperusteetOperation.VALINTATAPAJONO_PAIVITYS,
        ValintaResource.VALINTATAPAJONO,
        valintatapajono.getOid(),
        Changes.addedDto(valintatapajono),
        additionalAuditFields);

    Optional<Valintatapajono> dto =
        tulosService.muokkaaValintatapajonoa(
            valintatapajonoOid, valintatapajonoMuokkausFunktio, user);
    return dto.map(
            jono -> ResponseEntity.accepted().body(modelMapper.map(jono, ValintatapajonoDTO.class)))
        .orElse(ResponseEntity.notFound().build());
  }

  @PreAuthorize(READ_UPDATE_CRUD)
  @GetMapping(
      value = "/{valintatapajonoOid}/valmissijoiteltavaksi",
      produces = MediaType.APPLICATION_JSON_VALUE)
  public ResponseEntity<Object> haeSijoitteluStatus(
      @ApiParam(value = "Valintatapajonon OID", required = true) @PathVariable("valintatapajonoOid")
          String oid) {
    HashMap object = new HashMap();
    object.put("value", tulosService.haeSijoitteluStatus(oid));
    return ResponseEntity.accepted().body(object);
  }

  @PreAuthorize(READ_UPDATE_CRUD)
  @GetMapping(value = "/jonotsijoittelussa/{hakuOid}", produces = MediaType.APPLICATION_JSON_VALUE)
  public List<JonoDto> jonotSijoittelussa(@PathVariable("hakuOid") String hakuOid) {
    return tulosService.haeJonotSijoittelussa(hakuOid);
  }
}
