package fi.vm.sade.valintalaskenta.tulos.resource.impl;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.READ_UPDATE_CRUD;

import fi.vm.sade.valintalaskenta.domain.dto.HakukohdeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.MinimalJonoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeOsallistuminenDTO;
import fi.vm.sade.valintalaskenta.tulos.resource.HakuResource;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import java.util.List;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
@PreAuthorize("isAuthenticated()")
@Tag(
    name = "/haku",
    description =
        "Resurssi haun valintalaskennan virhetilanteiden ja hakukohtaisten tietojen hakemiseen")
@RequestMapping(value = "/haku")
public class HakuResourceImpl implements HakuResource {
  protected static final Logger LOGGER = LoggerFactory.getLogger(HakuResourceImpl.class);
  private final ValintalaskentaTulosService tulosService;

  @Autowired
  public HakuResourceImpl(final ValintalaskentaTulosService tulosService) {
    this.tulosService = tulosService;
  }

  @PreAuthorize(READ_UPDATE_CRUD)
  @Operation(
      summary = "Hakee haun valintalaskennan virhetilanteet OID:n perusteella")
  @GetMapping(value = "/{hakuOid}/virheet", produces = MediaType.APPLICATION_JSON_VALUE)
  public List<HakukohdeDTO> virheet(@PathVariable("hakuOid") final String hakuOid) {
    return tulosService.haeVirheetHaulle(hakuOid);
  }

  @PreAuthorize(READ_UPDATE_CRUD)
  @GetMapping(value = "/{hakuOid}/valintakoevirheet", produces = MediaType.APPLICATION_JSON_VALUE)
  public List<ValintakoeOsallistuminenDTO> valintakoevirheet(
      @PathVariable("hakuOid") final String hakuOid) {
    return tulosService.haeValintakoevirheetHaulle(hakuOid);
  }

  /**
   * ODW needs to load hakukohde valinnanvaihees but calling the api separatedly for each hakukohde
   * is too slow. Added an API which returns only minimal information for haku and contains a list
   * of valintatulos from those valinnanvaihe, which are siirretty sijoitteluun.
   */
  @PreAuthorize(READ_UPDATE_CRUD)
  @GetMapping(
      value = "/ilmanvalintalaskentaasijoitteluun",
      produces = MediaType.APPLICATION_JSON_VALUE)
  public List<MinimalJonoDTO> jonotSijoitteluun() {
    return tulosService.haeSijoittelunKayttamatJonotIlmanValintalaskentaa();
  }
}
