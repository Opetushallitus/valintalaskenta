package fi.vm.sade.valintalaskenta.domain.dto;

import java.util.List;
import java.util.Optional;

public class SuorituspalveluValintadataDTO {

  List<HakemusDTO> valintaHakemukset;

  public SuorituspalveluValintadataDTO(List<HakemusDTO> valintaHakemukset) {
    this.valintaHakemukset = valintaHakemukset;
  }

  public void vertaile(List<HakemusDTO> koostepalveluSureHakemukset) {
    koostepalveluSureHakemukset.forEach(
        sureHakemus -> {
          Optional<HakemusDTO> supaHakemus =
              valintaHakemukset.stream()
                  .filter(vh -> vh.getHakemusoid().equals(sureHakemus.getHakemusoid()))
                  .findFirst();
          if (supaHakemus.isPresent()) {
            // ...
          }
        });
  }

  public List<HakemusDTO> getValintaHakemukset() {
    return valintaHakemukset;
  }
}
