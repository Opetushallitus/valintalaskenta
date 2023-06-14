package fi.vm.sade.valintalaskenta.domain.dto.valintakoe;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.ArrayList;
import java.util.List;

@Schema(name = "valintalaskenta.HakutoiveDTO", description = "Hakutoive")
public class HakutoiveDTO {

  @Schema(description = "Hakutoiveen OID", required = true)
  private String hakukohdeOid;

  @Schema(description = "Valintakoevalinnan vaiheet")
  private List<ValintakoeValinnanvaiheDTO> valinnanVaiheet =
      new ArrayList<ValintakoeValinnanvaiheDTO>();

  public String getHakukohdeOid() {
    return hakukohdeOid;
  }

  public void setHakukohdeOid(String hakukohdeOid) {
    this.hakukohdeOid = hakukohdeOid;
  }

  public List<ValintakoeValinnanvaiheDTO> getValinnanVaiheet() {
    return valinnanVaiheet;
  }

  public void setValinnanVaiheet(List<ValintakoeValinnanvaiheDTO> valinnanVaiheet) {
    this.valinnanVaiheet = valinnanVaiheet;
  }
}
