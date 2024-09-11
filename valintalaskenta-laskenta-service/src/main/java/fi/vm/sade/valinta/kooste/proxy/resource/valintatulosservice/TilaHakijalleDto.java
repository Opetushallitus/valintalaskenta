package fi.vm.sade.valinta.kooste.proxy.resource.valintatulosservice;

import fi.vm.sade.sijoittelu.domain.ValintatuloksenTila;

public class TilaHakijalleDto {
  private String hakemusOid;
  private String hakukohdeOid;
  private String valintatapajonoOid;
  private ValintatuloksenTila tilaHakijalle;

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

  public String getValintatapajonoOid() {
    return valintatapajonoOid;
  }

  public void setValintatapajonoOid(String valintatapajonoOid) {
    this.valintatapajonoOid = valintatapajonoOid;
  }

  public ValintatuloksenTila getTilaHakijalle() {
    return tilaHakijalle;
  }

  public void setTilaHakijalle(ValintatuloksenTila tilaHakijalle) {
    this.tilaHakijalle = tilaHakijalle;
  }
}
