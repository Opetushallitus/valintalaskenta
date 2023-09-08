package fi.vm.sade.valintalaskenta.tulos.resource.impl;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.READ_UPDATE_CRUD;

import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.HakemusOsallistuminenDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.HakuDTO;
import fi.vm.sade.valintalaskenta.tulos.service.impl.ValintatietoService;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

@RestController
@PreAuthorize("isAuthenticated()")
@RequestMapping("/resources/valintatieto")
public class ValintatietoResourceImpl {
  @Autowired private ValintatietoService valintatietoService;

  @PreAuthorize(READ_UPDATE_CRUD)
  @PostMapping(
      value = "/hakukohde/{hakukohdeOid}",
      produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  public List<HakemusOsallistuminenDTO> haeValintatiedotHakukohteelle(
      @PathVariable("hakukohdeOid") final String hakukohdeOid,
      @RequestBody final List<String> valintakoeTunnisteet) {
    return valintatietoService.haeValintatiedotHakukohteelle(valintakoeTunnisteet, hakukohdeOid);
  }

  @PreAuthorize(READ_UPDATE_CRUD)
  @GetMapping(value = "/haku/{hakuOid}", produces = MediaType.APPLICATION_JSON_VALUE)
  public HakuDTO haeValintatiedot(@PathVariable("hakuOid") final String hakuOid) {
    return valintatietoService.haeValintatiedot(hakuOid);
  }
}
