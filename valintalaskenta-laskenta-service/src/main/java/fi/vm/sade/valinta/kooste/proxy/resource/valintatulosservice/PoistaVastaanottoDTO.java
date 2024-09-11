package fi.vm.sade.valinta.kooste.proxy.resource.valintatulosservice;

import fi.vm.sade.sijoittelu.domain.Valintatulos;

public class PoistaVastaanottoDTO {
  private String henkiloOid;
  private String hakukohdeOid;
  private String ilmoittaja;

  public String getHenkiloOid() {
    return henkiloOid;
  }

  public void setHenkiloOid(String henkiloOid) {
    this.henkiloOid = henkiloOid;
  }

  public String getHakukohdeOid() {
    return hakukohdeOid;
  }

  public void setHakukohdeOid(String hakukohdeOid) {
    this.hakukohdeOid = hakukohdeOid;
  }

  public String getIlmoittaja() {
    return ilmoittaja;
  }

  public void setIlmoittaja(String ilmoittaja) {
    this.ilmoittaja = ilmoittaja;
  }

  public static PoistaVastaanottoDTO of(Valintatulos valintatulos, String muokkaaja) {
    PoistaVastaanottoDTO p = new PoistaVastaanottoDTO();
    p.setHenkiloOid(valintatulos.getHakijaOid());
    p.setHakukohdeOid(valintatulos.getHakukohdeOid());
    p.setIlmoittaja(muokkaaja);
    return p;
  }
}
