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
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.core.Response;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

@RestController
@PreAuthorize("isAuthenticated()")
@Tag(
    name = "/resources/valintatapajono",
    description = "Resurssi valintatapajonon jonosijojen muokkaamiseen manuaalisesti")
@RequestMapping("/resources/valintatapajono")
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
  @Operation(summary = "Muokkaa jonosijaa")
  @PostMapping(
      value = "/{valintatapajonoOid}/{hakemusOid}/{jarjestyskriteeriPrioriteetti}/jonosija",
      produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  public Response muutaJonosija(
      @Parameter(name = "Valintatapajonon OID", required = true) @PathVariable("valintatapajonoOid")
          final String valintatapajonoOid,
      @Parameter(name = "Hakemus OID", required = true) @PathVariable("hakemusOid")
          final String hakemusOid,
      @Parameter(name = "Muokattavan järjestyskriteerin prioriteetti", required = true)
          @PathVariable("jarjestyskriteeriPrioriteetti")
          final Integer jarjestyskriteeriPrioriteetti,
      @Parameter(name = "Järjestyskriteerin uusi arvo", required = true) @RequestBody
          final MuokattuJonosijaArvoDTO arvo,
      final HttpServletRequest request) {
    User user = auditLog.getUser(request);

    MuokattuJonosija muokattuJonosija =
        tulosService.muutaJarjestyskriteeri(
            valintatapajonoOid, hakemusOid, jarjestyskriteeriPrioriteetti, arvo, user);
    if (muokattuJonosija != null) {
      MuokattuJonosijaDTO map = modelMapper.map(muokattuJonosija, MuokattuJonosijaDTO.class);
      return Response.status(Response.Status.ACCEPTED).entity(map).build();
    } else {
      return Response.status(Response.Status.NOT_FOUND).build();
    }
  }

  @PreAuthorize(UPDATE_CRUD)
  @Operation(summary = "Muokkaa jonosijaa")
  @DeleteMapping(
      value = "/{valintatapajonoOid}/{hakemusOid}/{jarjestyskriteeriPrioriteetti}/jonosija",
      produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  public Response poistaMuokattuJonosija(
      @Parameter(name = "Valintatapajonon OID", required = true) @PathVariable("valintatapajonoOid")
          final String valintatapajonoOid,
      @Parameter(name = "Hakemus OID", required = true) @PathVariable("hakemusOid")
          final String hakemusOid,
      @Parameter(name = "Muokattavan järjestyskriteerin prioriteetti", required = true)
          @PathVariable("jarjestyskriteeriPrioriteetti")
          final Integer jarjestyskriteeriPrioriteetti,
      final HttpServletRequest request) {
    User user = auditLog.getUser(request);

    MuokattuJonosija muokattuJonosija =
        tulosService.poistaMuokattuJonosija(
            valintatapajonoOid, hakemusOid, jarjestyskriteeriPrioriteetti, user);
    if (muokattuJonosija != null) {
      MuokattuJonosijaDTO map = modelMapper.map(muokattuJonosija, MuokattuJonosijaDTO.class);
      return Response.status(Response.Status.ACCEPTED).entity(map).build();
    } else {
      return Response.status(Response.Status.NOT_FOUND).build();
    }
  }

  @PreAuthorize(UPDATE_CRUD)
  @Operation(summary = "Tallentaa/muokkaa valintatapajonoa")
  @PutMapping(
      value = "/{valintatapajonoOid}/valmissijoiteltavaksi",
      produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  public Response muokkaaSijotteluStatusta(
      @Parameter(name = "Valintatapajonon OID", required = true) @PathVariable("valintatapajonoOid")
          final String valintatapajonoOid,
      @Parameter(name = "Sijoittelustatus", required = true) @RequestParam("status")
          final boolean status,
      @Parameter(name = "Valintatapajono", required = true)
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
            jono ->
                Response.status(Response.Status.ACCEPTED)
                    .entity(modelMapper.map(jono, ValintatapajonoDTO.class))
                    .build())
        .orElse(Response.status(Response.Status.NOT_FOUND).build());
  }

  @PreAuthorize(READ_UPDATE_CRUD)
  @GetMapping(
      value = "/{valintatapajonoOid}/valmissijoiteltavaksi",
      produces = MediaType.APPLICATION_JSON_VALUE)
  public Response haeSijoitteluStatus(
      @Parameter(name = "Valintatapajonon OID", required = true) @PathVariable("valintatapajonoOid")
          String oid) {
    HashMap object = new HashMap();
    object.put("value", tulosService.haeSijoitteluStatus(oid));
    return Response.status(Response.Status.ACCEPTED).entity(object).build();
  }

  @PreAuthorize(READ_UPDATE_CRUD)
  @GetMapping(value = "/jonotsijoittelussa/{hakuOid}", produces = MediaType.APPLICATION_JSON_VALUE)
  public List<JonoDto> jonotSijoittelussa(@PathVariable("hakuOid") String hakuOid) {
    return tulosService.haeJonotSijoittelussa(hakuOid);
  }
}
