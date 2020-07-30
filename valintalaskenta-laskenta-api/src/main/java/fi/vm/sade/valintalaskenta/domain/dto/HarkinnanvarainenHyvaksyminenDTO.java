package fi.vm.sade.valintalaskenta.domain.dto;

import fi.vm.sade.valintalaskenta.domain.valinta.HarkinnanvaraisuusTila;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

@ApiModel(
    value = "HarkinnanvarainenHyvaksyminenDTO",
    description = "Harkinnanvarainen hyväksyminen")
public class HarkinnanvarainenHyvaksyminenDTO {

  @ApiModelProperty(value = "Tila", required = true)
  private HarkinnanvaraisuusTila harkinnanvaraisuusTila;

  @ApiModelProperty(value = "Hakukohde OID", required = true)
  private String hakukohdeOid;

  @ApiModelProperty(value = "Hakemus OID", required = true)
  private String hakemusOid;

  @ApiModelProperty(value = "Haku OID", required = true)
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
