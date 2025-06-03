package fi.vm.sade.valintalaskenta.domain.dto.valintatieto;

import fi.vm.sade.valintalaskenta.domain.dto.ValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeDTO;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.Date;
import java.util.List;

public class ValintatietoValinnanvaiheDTO extends ValinnanvaiheDTO {
  public ValintatietoValinnanvaiheDTO() {}

  public ValintatietoValinnanvaiheDTO(
      int jarjestysnumero,
      String valinnanvaiheoid,
      String hakuOid,
      String nimi,
      Date createdAt,
      List<ValintatietoValintatapajonoDTO> valintatapajonot,
      List<ValintakoeDTO> valintakokeet,
      String hakukohdeOid) {
    super(
        jarjestysnumero,
        valinnanvaiheoid,
        hakuOid,
        nimi,
        createdAt,
        valintatapajonot,
        valintakokeet);
    this.valinnanvaihe = jarjestysnumero;
    this.hakukohdeOid = hakukohdeOid;
  }

  private int valinnanvaihe;

  private String hakukohdeOid;

  @Schema(hidden = true, requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  private Date lastModified;

  public void setValinnanvaihe(int valinnanvaihe) {
    this.valinnanvaihe = valinnanvaihe;
  }

  public void setLastModified(Date lastModified) {
    this.lastModified = lastModified;
  }

  public int getValinnanvaihe() {
    return valinnanvaihe;
  }

  public Date getLastModified() {
    return lastModified;
  }

  public String getHakukohdeOid() {
    return hakukohdeOid;
  }

  public void setHakukohdeOid(String hakukohdeOid) {
    this.hakukohdeOid = hakukohdeOid;
  }
}
