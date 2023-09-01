package fi.vm.sade.valintalaskenta.domain.valinta;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.*;

@Entity
@Table(name = "muokattujonosija")
public class MuokattuJonosija {

  @Id
  @GeneratedValue(strategy = GenerationType.AUTO)
  private Long id;

  //@Indexed(unique = false, dropDups = false)
  private String hakukohdeOid;

  //@Indexed(unique = false, dropDups = false)
  private String hakuOid;

  // @Indexed(unique = false, dropDups = false)
  private String valintatapajonoOid;

  private String hakemusOid;

  private Integer prioriteetti; // hakutoive

  @Embedded
  private List<Jarjestyskriteeritulos> jarjestyskriteerit = new ArrayList<Jarjestyskriteeritulos>();

  @Embedded private List<LogEntry> logEntries = new ArrayList<LogEntry>();

  public List<Jarjestyskriteeritulos> getJarjestyskriteerit() {
    return jarjestyskriteerit;
  }

  public void setJarjestyskriteerit(List<Jarjestyskriteeritulos> jarjestyskriteerit) {
    this.jarjestyskriteerit = jarjestyskriteerit;
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

  public String getHakuOid() {
    return hakuOid;
  }

  public void setHakuOid(String hakuOid) {
    this.hakuOid = hakuOid;
  }

  public String getHakukohdeOid() {
    return hakukohdeOid;
  }

  public void setHakukohdeOid(String hakukohdeOid) {
    this.hakukohdeOid = hakukohdeOid;
  }

  public Integer getPrioriteetti() {
    return prioriteetti;
  }

  public void setPrioriteetti(Integer prioriteetti) {
    this.prioriteetti = prioriteetti;
  }

  public List<LogEntry> getLogEntries() {
    return logEntries;
  }

  public void setLogEntries(List<LogEntry> logEntries) {
    this.logEntries = logEntries;
  }
}
