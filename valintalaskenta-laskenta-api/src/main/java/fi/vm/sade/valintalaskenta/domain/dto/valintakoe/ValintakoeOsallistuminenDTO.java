package fi.vm.sade.valintalaskenta.domain.dto.valintakoe;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/** User: wuoti Date: 29.8.2013 Time: 8.28 */
@Schema(
    name = "valintalaskenta.domain.dto.valintakoe.ValintakoeOsallistuminenDTO",
    description = "Valintakoeosallistuminen")
public class ValintakoeOsallistuminenDTO {

  @Schema(title = "Haku OID", required = true)
  private String hakuOid;

  @Schema(title = "Hakemus OID", required = true)
  private String hakemusOid;

  @Schema(title = "Hakija OID")
  private String hakijaOid;

  @Schema(title = "Etunimi")
  private String etunimi;

  @Schema(title = "Sukunimi")
  private String sukunimi;

  @Schema(title = "Luontiajankohta")
  private Date createdAt;

  @Schema(title = "Hakutoiveet")
  private List<HakutoiveDTO> hakutoiveet = new ArrayList<HakutoiveDTO>();

  public String getHakuOid() {
    return hakuOid;
  }

  public void setHakuOid(String hakuOid) {
    this.hakuOid = hakuOid;
  }

  public String getHakemusOid() {
    return hakemusOid;
  }

  public void setHakemusOid(String hakemusOid) {
    this.hakemusOid = hakemusOid;
  }

  public String getHakijaOid() {
    return hakijaOid;
  }

  public void setHakijaOid(String hakijaOid) {
    this.hakijaOid = hakijaOid;
  }

  public String getEtunimi() {
    return etunimi;
  }

  public void setEtunimi(String etunimi) {
    this.etunimi = etunimi;
  }

  public String getSukunimi() {
    return sukunimi;
  }

  public void setSukunimi(String sukunimi) {
    this.sukunimi = sukunimi;
  }

  public Date getCreatedAt() {
    return createdAt;
  }

  public void setCreatedAt(Date createdAt) {
    this.createdAt = createdAt;
  }

  public List<HakutoiveDTO> getHakutoiveet() {
    return hakutoiveet;
  }

  public void setHakutoiveet(List<HakutoiveDTO> hakutoiveet) {
    this.hakutoiveet = hakutoiveet;
  }
}
