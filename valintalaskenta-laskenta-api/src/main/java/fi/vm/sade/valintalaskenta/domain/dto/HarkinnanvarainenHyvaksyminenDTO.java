package fi.vm.sade.valintalaskenta.domain.dto;

import fi.vm.sade.valintalaskenta.domain.valinta.HarkinnanvaraisuusTila;
import io.swagger.v3.oas.annotations.media.Schema;

@Schema(
    name = "HarkinnanvarainenHyvaksyminenDTO",
    description = "Harkinnanvarainen hyväksyminen")
public class HarkinnanvarainenHyvaksyminenDTO {

  @Schema(name = "Tila", required = true)
  private HarkinnanvaraisuusTila harkinnanvaraisuusTila;

  @Schema(name = "Hakukohde OID", required = true)
  private String hakukohdeOid;

  @Schema(name = "Hakemus OID", required = true)
  private String hakemusOid;

  @Schema(name = "Haku OID", required = true)
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
