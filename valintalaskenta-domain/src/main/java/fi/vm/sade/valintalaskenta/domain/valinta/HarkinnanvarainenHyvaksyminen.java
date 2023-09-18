package fi.vm.sade.valintalaskenta.domain.valinta;

import javax.persistence.*;
import java.util.UUID;

@Entity
@Table(name = "HarkinnanvarainenHyvaksyminen")
public class HarkinnanvarainenHyvaksyminen {
  @Id
  @GeneratedValue(strategy = GenerationType.AUTO)
  private UUID id;

  @Column
  private HarkinnanvaraisuusTila harkinnanvaraisuusTila;

  @Column
  private String hakukohdeOid;

  @Column
  private String hakemusOid;

  @Column
  private String hakuOid;

  public String getHakemusOid() {
    return hakemusOid;
  }

  public void setHakemusOid(String hakemusOid) {
    this.hakemusOid = hakemusOid;
  }

  public String getHakukohdeOid() {
    return hakukohdeOid;
  }

  public void setHakukohdeOid(String hakukohdeOid) {
    this.hakukohdeOid = hakukohdeOid;
  }

  public HarkinnanvaraisuusTila getHarkinnanvaraisuusTila() {
    return harkinnanvaraisuusTila;
  }

  public void setHarkinnanvaraisuusTila(HarkinnanvaraisuusTila harkinnanvaraisuusTila) {
    this.harkinnanvaraisuusTila = harkinnanvaraisuusTila;
  }

  public String getHakuOid() {
    return hakuOid;
  }

  public void setHakuOid(String hakuOid) {
    this.hakuOid = hakuOid;
  }

  public UUID getId() {
    return id;
  }

  public void setId(UUID id) {
    this.id = id;
  }
}
