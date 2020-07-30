package fi.vm.sade.valintalaskenta.domain.valinta;

import org.bson.types.ObjectId;
import org.mongodb.morphia.annotations.Entity;
import org.mongodb.morphia.annotations.Id;

@Entity("HarkinnanvarainenHyvaksyminen")
public class HarkinnanvarainenHyvaksyminen {
  @Id private ObjectId id;

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
}
