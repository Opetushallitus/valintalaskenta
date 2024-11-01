package fi.vm.sade.valintalaskenta.tulos.context;

import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import java.util.ArrayList;
import java.util.List;

import fi.vm.sade.valintalaskenta.tulos.resource.external.ValintaperusteetResource;

public class ValintaperusteetResourceImpl implements ValintaperusteetResource {

  @Override
  public List<ValintaperusteetDTO> haeValintaperusteet(
      String hakukohdeOid, Integer valinnanVaiheJarjestysluku) {
    return new ArrayList<>();
  }
}
