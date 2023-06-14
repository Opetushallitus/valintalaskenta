package fi.vm.sade.valintalaskenta.domain.dto.valintakoe;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.ArrayList;
import java.util.List;

@Schema(name = "ValintakoeValinnanvaiheDTO", description = "Valintakoevalinnanvaihe")
public class ValintakoeValinnanvaiheDTO {
  @Schema(title = "OID", required = true)
  private String valinnanVaiheOid;

  @Schema(title = "Vaiheen järjestysluku", required = true)
  private Integer valinnanVaiheJarjestysluku;

  @Schema(title = "Valintakokeet")
  private List<ValintakoeDTO> valintakokeet = new ArrayList<ValintakoeDTO>();

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
}
