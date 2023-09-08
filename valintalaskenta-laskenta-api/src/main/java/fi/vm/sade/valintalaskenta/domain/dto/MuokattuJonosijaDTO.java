package fi.vm.sade.valintalaskenta.domain.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.ArrayList;
import java.util.List;

@Schema(name = "MuokattuJonosijaDTO", description = "Muokattu jonosija")
public class MuokattuJonosijaDTO {

  @Schema(description = "Hakukohde OID", required = true)
  private String hakukohdeOid;

  @Schema(description = "Haku OID", required = true)
  private String hakuOid;

  @Schema(description = "Valintatapajono OID", required = true)
  private String valintatapajonoOid;

  @Schema(description = "Hakukohde OID", required = true)
  private String hakemusOid;

  @Schema(description = "Prioriteetti", required = true)
  private Integer prioriteetti; // hakutoive

  @Schema(description = "Harkinnanvaraisuus")
  private Boolean harkinnanvarainen;

  @Schema(description = "Järjestyskriteeritulokset", required = true)
  private List<JarjestyskriteeritulosDTO> jarjestyskriteerit =
      new ArrayList<JarjestyskriteeritulosDTO>();

  @Schema(description = "Lokiviestit", required = true)
  private List<LogEntryDTO> logEntries = new ArrayList<LogEntryDTO>();

  public String getHakukohdeOid() {
    return hakukohdeOid;
  }

  public void setHakukohdeOid(String hakukohdeOid) {
    this.hakukohdeOid = hakukohdeOid;
  }

  public String getHakuOid() {
    return hakuOid;
  }

  public void setHakuOid(String hakuOid) {
    this.hakuOid = hakuOid;
  }

  public String getValintatapajonoOid() {
    return valintatapajonoOid;
  }

  public void setValintatapajonoOid(String valintatapajonoOid) {
    this.valintatapajonoOid = valintatapajonoOid;
  }

  public String getHakemusOid() {
    return hakemusOid;
  }

  public void setHakemusOid(String hakemusOid) {
    this.hakemusOid = hakemusOid;
  }

  public Integer getPrioriteetti() {
    return prioriteetti;
  }

  public void setPrioriteetti(Integer prioriteetti) {
    this.prioriteetti = prioriteetti;
  }

  public Boolean getHarkinnanvarainen() {
    return harkinnanvarainen;
  }

  public void setHarkinnanvarainen(Boolean harkinnanvarainen) {
    this.harkinnanvarainen = harkinnanvarainen;
  }

  public List<JarjestyskriteeritulosDTO> getJarjestyskriteerit() {
    return jarjestyskriteerit;
  }

  public void setJarjestyskriteerit(List<JarjestyskriteeritulosDTO> jarjestyskriteerit) {
    this.jarjestyskriteerit = jarjestyskriteerit;
  }

  public List<LogEntryDTO> getLogEntries() {
    return logEntries;
  }

  public void setLogEntries(List<LogEntryDTO> logEntries) {
    this.logEntries = logEntries;
  }
}
