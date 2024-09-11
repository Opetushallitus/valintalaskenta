package fi.vm.sade.valinta.kooste.external.resource.harkinnanvaraisuus;

import fi.vm.sade.auditlog.Changes;
import fi.vm.sade.valinta.kooste.AuthorizationUtil;
import fi.vm.sade.valinta.kooste.KoosteAudit;
import fi.vm.sade.valinta.kooste.external.resource.harkinnanvaraisuus.dto.HakemuksenHarkinnanvaraisuus;
import fi.vm.sade.valinta.sharedutils.AuditLog;
import fi.vm.sade.valinta.sharedutils.ValintaResource;
import fi.vm.sade.valinta.sharedutils.ValintaperusteetOperation;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletRequest;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.context.request.async.DeferredResult;

@RestController("HarkinnanvaraisuusResource")
@RequestMapping(value = "/resources/harkinnanvaraisuus")
@PreAuthorize("isAuthenticated()")
@Tag(name = "/harkinnanvaraisuus", description = "Hakemusten harkinnanvaraisuustiedot")
public class HarkinnanvaraisuusResource {
  private static final Logger LOG = LoggerFactory.getLogger(HarkinnanvaraisuusResource.class);

  @Autowired private HarkinnanvaraisuusAsyncResource harkinnanvaraisuusAsyncResource;

  @PreAuthorize(
      "hasAnyRole('ROLE_APP_HAKEMUS_READ_UPDATE', 'ROLE_APP_HAKEMUS_READ', 'ROLE_APP_HAKEMUS_CRUD', 'ROLE_APP_HAKEMUS_LISATIETORU', 'ROLE_APP_HAKEMUS_LISATIETOCRUD')")
  @PostMapping(
      value = "/hakemuksille",
      consumes = MediaType.APPLICATION_JSON_VALUE,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(
      summary = "Hakemusten harkinnanvaraisuustiedot",
      responses = {
        @ApiResponse(
            responseCode = "OK",
            content =
                @Content(schema = @Schema(implementation = HakemuksenHarkinnanvaraisuus[].class)))
      })
  public DeferredResult<List<HakemuksenHarkinnanvaraisuus>> hakemustenHarkinnanvaraisuustiedot(
      @RequestBody List<String> hakemusOids, HttpServletRequest request) {

    DeferredResult<List<HakemuksenHarkinnanvaraisuus>> deferredResult =
        new DeferredResult<>(60 * 60 * 1000l);

    String targetOids = String.join(",", hakemusOids);
    AuditLog.log(
        KoosteAudit.AUDIT,
        AuthorizationUtil.createAuditSession(request).asAuditUser(),
        ValintaperusteetOperation.HAKEMUS, // fixme, sharedutils
        ValintaResource.HAKEMUKSET, // fixme, sharedutils
        targetOids,
        Changes.EMPTY,
        Collections.emptyMap());

    harkinnanvaraisuusAsyncResource
        .getHarkinnanvaraisuudetForHakemukses(hakemusOids)
        .thenApply(result -> deferredResult.setResult(result))
        .exceptionally(
            e -> {
              LOG.error("Hakemusten harkinnanvaraisuustietojen haku epäonnistui: ", e);
              deferredResult.setErrorResult(e);
              return null;
            });

    return deferredResult;
  }

  @PreAuthorize(
      "hasAnyRole('ROLE_APP_HAKEMUS_READ_UPDATE', 'ROLE_APP_HAKEMUS_READ', 'ROLE_APP_HAKEMUS_CRUD', 'ROLE_APP_HAKEMUS_LISATIETORU', 'ROLE_APP_HAKEMUS_LISATIETOCRUD')")
  @PostMapping(
      value = "/atarutiedoille",
      consumes = MediaType.APPLICATION_JSON_VALUE,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(
      summary = "Synkatut harkinnanvaraisuudet atarutiedoille",
      responses = {
        @ApiResponse(
            responseCode = "OK",
            content =
                @Content(schema = @Schema(implementation = HakemuksenHarkinnanvaraisuus[].class)))
      })
  public DeferredResult<List<HakemuksenHarkinnanvaraisuus>>
      hakemustenHarkinnanvaraisuustiedotAtarutiedoille(
          @RequestBody List<HakemuksenHarkinnanvaraisuus> atarutiedot, HttpServletRequest request) {

    DeferredResult<List<HakemuksenHarkinnanvaraisuus>> deferredResult =
        new DeferredResult<>(60 * 60 * 1000l);

    String targetOids =
        atarutiedot.stream()
            .map(HakemuksenHarkinnanvaraisuus::getHakemusOid)
            .collect(Collectors.joining(","));
    AuditLog.log(
        KoosteAudit.AUDIT,
        AuthorizationUtil.createAuditSession(request).asAuditUser(),
        ValintaperusteetOperation.HAKEMUS, // fixme, sharedutils
        ValintaResource.HAKEMUKSET, // fixme, sharedutils
        targetOids,
        Changes.EMPTY,
        Collections.emptyMap());

    harkinnanvaraisuusAsyncResource
        .getSyncedHarkinnanvaraisuudes(atarutiedot)
        .thenApply(result -> deferredResult.setResult(result))
        .exceptionally(
            e -> {
              LOG.error("Hakemusten harkinnanvaraisuustietojen haku epäonnistui: ", e);
              deferredResult.setErrorResult(e);
              return null;
            });

    return deferredResult;
  }
}
