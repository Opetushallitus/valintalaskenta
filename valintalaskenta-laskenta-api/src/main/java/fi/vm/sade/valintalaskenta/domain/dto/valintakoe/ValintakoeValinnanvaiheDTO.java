package fi.vm.sade.valintalaskenta.domain.dto.valintakoe;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

@Schema(name = "ValintakoeValinnanvaiheDTO", description = "Valintakoevalinnanvaihe")
public class ValintakoeValinnanvaiheDTO {
  @Schema(description = "OID", required = true)
  private String valinnanVaiheOid;

  @Schema(description = "Vaiheen järjestysluku", required = true)
  private Integer valinnanVaiheJarjestysluku;

  @Schema(description = "Valintakokeet")
  private List<ValintakoeDTO> valintakokeet = new ArrayList<ValintakoeDTO>();

  @Schema(hidden = true)
  private Date lastModified;

  public String getValinnanVaiheOid() {
    return valinnanVaiheOid;
  }

  public void setValinnanVaiheOid(String valinnanVaiheOid) {
    this.valinnanVaiheOid = valinnanVaiheOid;
  }

  public Integer getValinnanVaiheJarjestysluku() {
    return valinnanVaiheJarjestysluku;
  }

  public void setValinnanVaiheJarjestysluku(Integer valinnanVaiheJarjestysluku) {
    this.valinnanVaiheJarjestysluku = valinnanVaiheJarjestysluku;
  }

  public List<ValintakoeDTO> getValintakokeet() {
    return valintakokeet;
  }

  public void setValintakokeet(List<ValintakoeDTO> valintakokeet) {
    this.valintakokeet = valintakokeet;
  }

  public void setLastModified(Date lastModified) {
    this.lastModified = lastModified;
  }

  public Date getLastModified() {
    return lastModified;
  }
}
