package fi.vm.sade.valintalaskenta.laskenta.resource.dto.seuranta;

import java.util.List;

public class HakukohdeDto {
  private final String hakukohdeOid;
  private final String organisaatioOid;
  private final HakukohdeTila tila;
  private final List<IlmoitusDto> ilmoitukset;

  public HakukohdeDto() {
    this.hakukohdeOid = null;
    this.tila = null;
    this.ilmoitukset = null;
    this.organisaatioOid = null;
  }

  public HakukohdeDto(String hakukohdeOid, String organisaatioOid) {
    this.hakukohdeOid = hakukohdeOid;
    this.organisaatioOid = organisaatioOid;
    this.tila = null;
    this.ilmoitukset = null;
  }

  public HakukohdeDto(
      String hakukohdeOid,
      String organisaatioOid,
      HakukohdeTila tila,
      List<IlmoitusDto> ilmoitukset) {
    this.organisaatioOid = organisaatioOid;
    this.hakukohdeOid = hakukohdeOid;
    this.tila = tila;
    this.ilmoitukset = ilmoitukset;
  }

  public String getOrganisaatioOid() {
    return organisaatioOid;
  }

  public HakukohdeTila getTila() {
    return tila;
  }

  public List<IlmoitusDto> getIlmoitukset() {
    return ilmoitukset;
  }

  public String getHakukohdeOid() {
    return hakukohdeOid;
  }
}
