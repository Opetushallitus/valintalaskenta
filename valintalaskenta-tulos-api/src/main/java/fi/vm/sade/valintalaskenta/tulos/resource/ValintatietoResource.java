package fi.vm.sade.valintalaskenta.tulos.resource;

import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.HakemusOsallistuminenDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.HakuDTO;
import java.util.List;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;

@RequestMapping("/valintatieto")
public interface ValintatietoResource {
  @PostMapping(
      value = "/hakukohde/{hakukohdeOid}",
      produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  List<HakemusOsallistuminenDTO> haeValintatiedotHakukohteelle(
      @PathVariable("hakukohdeOid") final String hakukohdeOid,
      @RequestBody final List<String> valintakoeTunnisteet);

  @GetMapping(value = "/haku/{hakuOid}", produces = MediaType.APPLICATION_JSON_VALUE)
  HakuDTO haeValintatiedot(@PathVariable("hakuOid") final String hakuOid);
}
