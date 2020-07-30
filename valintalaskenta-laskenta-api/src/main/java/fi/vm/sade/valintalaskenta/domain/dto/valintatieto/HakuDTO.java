package fi.vm.sade.valintalaskenta.domain.dto.valintatieto;

import static java.lang.Boolean.TRUE;
import static java.util.stream.Collectors.toSet;

import fi.vm.sade.valintalaskenta.domain.dto.HakukohdeDTO;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public class HakuDTO {
  private String hakuOid;
  private List<HakukohdeDTO> hakukohteet;

  public void setHakuOid(String hakuOid) {
    this.hakuOid = hakuOid;
  }

  public String getHakuOid() {
    return hakuOid;
  }

  public List<HakukohdeDTO> getHakukohteet() {
    if (hakukohteet == null) {
      hakukohteet = new ArrayList<>();
    }
    return hakukohteet;
  }

  public void setHakukohteet(List<HakukohdeDTO> hakukohteet) {
    this.hakukohteet = hakukohteet;
  }

  public Set<String> filtteroiOiditHakukohteilleJoillaOnAktiivisiaJonoja() {
    return hakukohteet.stream()
        // Joku valinnanvaihe jossa aktiivinen jono
        .filter(
            hakukohde ->
                hakukohde.getValinnanvaihe().stream()
                    .anyMatch(
                        v ->
                            v.getValintatapajonot().stream()
                                .anyMatch(j -> TRUE.equals(j.getAktiivinen()))))
        .map(HakukohdeDTO::getOid)
        .collect(toSet());
  }
}
