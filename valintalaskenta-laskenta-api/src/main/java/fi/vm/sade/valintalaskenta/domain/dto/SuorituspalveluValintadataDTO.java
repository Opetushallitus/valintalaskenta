package fi.vm.sade.valintalaskenta.domain.dto;

import java.util.List;

public class SuorituspalveluValintadataDTO {

  List<HakemusDTO> valintaHakemukset;

  public SuorituspalveluValintadataDTO(List<HakemusDTO> valintaHakemukset) {
    this.valintaHakemukset = valintaHakemukset;
  }

  public List<HakemusDTO> getValintaHakemukset() {
    return valintaHakemukset;
  }
}
