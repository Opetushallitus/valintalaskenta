package fi.vm.sade.valintalaskenta.domain.valinta;

import org.springframework.data.annotation.Id;

import java.util.UUID;

public class HarkinnanvarainenHyvaksyminen {
  @Id
  private UUID id;

  private HarkinnanvaraisuusTila harkinnanvaraisuusTila;

  private String hakukohdeOid;
  private String hakemusOid;

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
