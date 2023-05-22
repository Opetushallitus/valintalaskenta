package fi.vm.sade.valintalaskenta.domain.dto;

import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.ValintatietoValintatapajonoDTO;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

@Schema(name = "valintalaskenta.domain.dto.ValinnanvaiheDTO", description = "Valinnan vaihe")
public class ValinnanvaiheDTO {

  @Schema(name = "Järjestysnumero", required = true)
  private int jarjestysnumero;

  @Schema(name = "Valinnan vaiheen OID", required = true)
  private String valinnanvaiheoid;

  @Schema(name = "Haun OID", required = true)
  private String hakuOid;

  @Schema(name = "Valinnan vaiheen nimi")
  private String nimi;

  @Schema(name = "Luomisajankohta", required = true)
  private Date createdAt;

  @Schema(name = "Valintatapajonot", required = true)
  private List<ValintatietoValintatapajonoDTO> valintatapajonot =
      new ArrayList<ValintatietoValintatapajonoDTO>();

  @Schema(name = "Valintakokeet")
  private List<ValintakoeDTO> valintakokeet = new ArrayList<ValintakoeDTO>();

  public ValinnanvaiheDTO() {}

  public ValinnanvaiheDTO(
      int jarjestysnumero,
      String valinnanvaiheoid,
      String hakuOid,
      String nimi,
      Date createdAt,
      List<ValintatietoValintatapajonoDTO> valintatapajonot,
      List<ValintakoeDTO> valintakokeet) {
    this.jarjestysnumero = jarjestysnumero;
    this.valinnanvaiheoid = valinnanvaiheoid;
    this.hakuOid = hakuOid;
    this.nimi = nimi;
    this.createdAt = createdAt;
    this.valintatapajonot = valintatapajonot;
    this.valintakokeet = valintakokeet;
  }

  public List<ValintatietoValintatapajonoDTO> getValintatapajonot() {
    return valintatapajonot;
  }

  public String getValinnanvaiheoid() {
    return valinnanvaiheoid;
  }

  public void setValinnanvaiheoid(String valinnanvaiheoid) {
    this.valinnanvaiheoid = valinnanvaiheoid;
  }

  public void setValintatapajonot(List<ValintatietoValintatapajonoDTO> valintatapajonot) {
    this.valintatapajonot = valintatapajonot;
  }

  public int getJarjestysnumero() {
    return jarjestysnumero;
  }

  public void setJarjestysnumero(int jarjestysnumero) {
    this.jarjestysnumero = jarjestysnumero;
  }

  public Date getCreatedAt() {
    return createdAt;
  }

  public void setCreatedAt(Date createdAt) {
    this.createdAt = createdAt;
  }

  public String getNimi() {
    return nimi;
  }

  public void setNimi(String nimi) {
    this.nimi = nimi;
  }

  public List<ValintakoeDTO> getValintakokeet() {
    return valintakokeet;
  }

  public void setValintakokeet(List<ValintakoeDTO> valintakokeet) {
    this.valintakokeet = valintakokeet;
  }

  public String getHakuOid() {
    return hakuOid;
  }

  public void setHakuOid(String hakuOid) {
    this.hakuOid = hakuOid;
  }

  public boolean empty() {
    if (valintatapajonot == null || valintatapajonot.isEmpty()) {
      return true;
    }
    return valintatapajonot.get(0).empty();
  }

  @Override
  public String toString() {
    return "ValinnanvaiheDTO{"
        + "jarjestysnumero="
        + jarjestysnumero
        + ", valinnanvaiheoid='"
        + valinnanvaiheoid
        + '\''
        + ", hakuOid='"
        + hakuOid
        + '\''
        + ", nimi='"
        + nimi
        + '\''
        + ", createdAt="
        + createdAt
        + ", valintatapajonot="
        + valintatapajonot
        + ", valintakokeet="
        + valintakokeet
        + '}';
  }
}
