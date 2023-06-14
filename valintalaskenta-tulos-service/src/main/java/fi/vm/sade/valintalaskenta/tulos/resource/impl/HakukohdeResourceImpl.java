package fi.vm.sade.valintalaskenta.tulos.resource.impl;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.READ_UPDATE_CRUD;
import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.ROLE_VALINTOJENTOTEUTTAMINEN_TULOSTENTUONTI;

import fi.vm.sade.auditlog.Changes;
import fi.vm.sade.auditlog.User;
import fi.vm.sade.javautils.opintopolku_spring_security.Authorizer;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.resource.ValintaperusteetResource;
import fi.vm.sade.valinta.sharedutils.ValintaResource;
import fi.vm.sade.valinta.sharedutils.ValintaperusteetOperation;
import fi.vm.sade.valintalaskenta.domain.dto.HakijaryhmaDTO;
import fi.vm.sade.valintalaskenta.domain.dto.ValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.ValintatietoValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.tulos.LaskentaAudit;
import fi.vm.sade.valintalaskenta.tulos.logging.LaskentaAuditLog;
import fi.vm.sade.valintalaskenta.tulos.resource.HakukohdeResource;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import javax.servlet.http.HttpServletRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

@RestController
@PreAuthorize("isAuthenticated()")
@Tag(name = "/resources/hakukohde", description = "Resurssi tulosten hakemiseen hakukohteittain")
@RequestMapping(value = "/resources/hakukohde")
public class HakukohdeResourceImpl implements HakukohdeResource {
  protected static final Logger LOGGER = LoggerFactory.getLogger(HakukohdeResourceImpl.class);
  private final ValintalaskentaTulosService tulosService;
  private final Authorizer authorizer;
  private final ValintaperusteetResource valintaperusteetResource;
  private final LaskentaAuditLog auditLog;

  @Autowired
  public HakukohdeResourceImpl(
      final LaskentaAuditLog auditLog,
      final ValintalaskentaTulosService tulosService,
      final Authorizer authorizer,
      final ValintaperusteetResource valintaperusteetResource) {
    this.auditLog = auditLog;
    this.tulosService = tulosService;
    this.authorizer = authorizer;
    this.valintaperusteetResource = valintaperusteetResource;
  }

  @PreAuthorize(READ_UPDATE_CRUD)
  @Operation(summary = "Hakee hakukohteen valinnan vaiheiden tulokset")
  @GetMapping(value = "/{hakukohdeoid}/valinnanvaihe", produces = MediaType.APPLICATION_JSON_VALUE)
  public List<ValintatietoValinnanvaiheDTO> hakukohde(
      @Parameter(name = "Hakukohteen OID", required = true) @PathVariable("hakukohdeoid")
          final String hakukohdeoid) {
    try {
      return tulosService.haeValinnanvaiheetHakukohteelle(hakukohdeoid);
    } catch (Exception e) {
      LOGGER.error("Valintalaskennan tulosten haku hakukohteelle {} epäonnistui!", hakukohdeoid, e);
      throw e;
    }
  }

  @PreAuthorize(READ_UPDATE_CRUD)
  @Operation(summary = "Lisää tuloksia valinnanvaiheelle")
  @PostMapping(
      value = "/{hakukohdeoid}/valinnanvaihe",
      produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  public ResponseEntity<Object> lisaaTuloksia(
      @Parameter(name = "Hakukohteen OID", required = true) @PathVariable("hakukohdeoid")
          final String hakukohdeoid,
      @Parameter(name = "Tarjoaja OID", required = true) @RequestParam("tarjoajaOid")
          final String tarjoajaOid,
      @Parameter(name = "Muokattava valinnanvaihe", required = true) @RequestBody
          final ValinnanvaiheDTO vaihe,
      final HttpServletRequest request) {
    try {
      authorizer.checkOrganisationAccess(tarjoajaOid, ROLE_VALINTOJENTOTEUTTAMINEN_TULOSTENTUONTI);

      List<ValintaperusteetDTO> valintaperusteet =
          valintaperusteetResource.haeValintaperusteet(hakukohdeoid, null);
      if (vaihe.empty()) {
        Map<String, String> message = new HashMap<>();
        LOGGER.warn(
            String.format(
                "Saatiin tyhjä data käyttöliittymältä; ei tallenneta. "
                    + "tarjoajaOid = %s, hakukohdeOid = %s , syöte: %s",
                tarjoajaOid, hakukohdeoid, vaihe));
        message.put("error", "Saatiin tyhjä data käyttöliittymältä; ei tallenneta.");
        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(message);
      }

      Optional<ValintaperusteetDTO> valinnanVaiheValintaperusteissa =
          valintaperusteet.stream()
              .filter(
                  valintaperuste ->
                      valintaperuste
                          .getValinnanVaihe()
                          .getValinnanVaiheOid()
                          .equals(vaihe.getValinnanvaiheoid()))
              .findAny();

      if (!valinnanVaiheValintaperusteissa.isPresent()) {
        LOGGER.error(
            "Päivitettävää valinnanvaihetta ei löytynyt valintaperusteista, hakukohde {}, valinnanvaihe {}",
            hakukohdeoid,
            vaihe.getValinnanvaiheoid());
        return ResponseEntity.internalServerError().build();
      }
    } catch (Exception e) {
      LOGGER.error(
          "Valintatapajonon pisteitä ei saatu päivitettyä hakukohteelle " + hakukohdeoid, e);
      return ResponseEntity.internalServerError().build();
    }
    User user = auditLog.getUser(request);
    ValinnanvaiheDTO valinnanvaihe = tulosService.lisaaTuloksia(vaihe, hakukohdeoid, tarjoajaOid);
    auditLog(hakukohdeoid, vaihe, user);
    return ResponseEntity.accepted().body(valinnanvaihe);
  }

  private void auditLog(String hakukohdeoid, ValinnanvaiheDTO vaihe, User user) {
    vaihe
        .getValintatapajonot()
        .forEach(
            v -> {
              v.getJonosijat()
                  .forEach(
                      h -> {
                        Map<String, String> additionalAuditFields = new HashMap<>();
                        additionalAuditFields.put("hakemusOid", h.getHakemusOid());
                        additionalAuditFields.put("hakijaOid", h.getHakijaOid());
                        additionalAuditFields.put("jonosija", Integer.toString(h.getJonosija()));
                        additionalAuditFields.put("valintatapajonoOid", v.getValintatapajonooid());
                        additionalAuditFields.put("hakukohdeOid", hakukohdeoid);
                        auditLog.log(
                            LaskentaAudit.AUDIT,
                            user,
                            ValintaperusteetOperation.VALINNANVAIHE_TUONTI_KAYTTOLIITTYMA,
                            ValintaResource.VALINNANVAIHE,
                            vaihe.getValinnanvaiheoid(),
                            Changes.addedDto(vaihe),
                            additionalAuditFields);
                      });
            });
  }

  @Operation(summary = "Hakee hakukohteen hakijaryhmien tulokset")
  @GetMapping(value = "/{hakukohdeoid}/hakijaryhma", produces = MediaType.APPLICATION_JSON_VALUE)
  public List<HakijaryhmaDTO> hakijaryhmat(
      @Parameter(name = "Hakukohteen OID", required = true) @PathVariable("hakukohdeoid")
          final String hakukohdeoid) {
    try {
      return tulosService.haeHakijaryhmatHakukohteelle(hakukohdeoid);
    } catch (Exception e) {
      LOGGER.error("Hakijaryhmien tulosten haku hakukohteelle {} epäonnistui!", hakukohdeoid, e);
      throw e;
    }
  }
}
