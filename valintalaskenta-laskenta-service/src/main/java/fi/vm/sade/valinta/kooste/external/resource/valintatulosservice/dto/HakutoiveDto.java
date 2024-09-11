package fi.vm.sade.valinta.kooste.external.resource.valintatulosservice.dto;

import fi.vm.sade.sijoittelu.tulos.dto.ValintatuloksenTila;

public class HakutoiveDto {
  private String hakukohdeOid;
  private String tarjoajaOid;
  private String valintatapajonoOid;
  private Valintatila valintatila;
  private ValintatuloksenTila vastaanottotila;
  private IlmoittautumistilaDto ilmoittautumistila;

  public IlmoittautumistilaDto getIlmoittautumistila() {
    return ilmoittautumistila;
  }

  public void setIlmoittautumistila(IlmoittautumistilaDto ilmoittautumistila) {
    this.ilmoittautumistila = ilmoittautumistila;
  }

  public Valintatila getValintatila() {
    return valintatila;
  }

  public void setValintatila(Valintatila valintatila) {
    this.valintatila = valintatila;
  }

  public void setVastaanottotila(ValintatuloksenTila vastaanottotila) {
    this.vastaanottotila = vastaanottotila;
  }

  public ValintatuloksenTila getVastaanottotila() {
    return vastaanottotila;
  }

  public String getHakukohdeOid() {
    return hakukohdeOid;
  }

  public String getTarjoajaOid() {
    return tarjoajaOid;
  }

  public String getValintatapajonoOid() {
    return valintatapajonoOid;
  }

  public void setHakukohdeOid(String hakukohdeOid) {
    this.hakukohdeOid = hakukohdeOid;
  }

  public void setTarjoajaOid(String tarjoajaOid) {
    this.tarjoajaOid = tarjoajaOid;
  }

  public void setValintatapajonoOid(String valintatapajonoOid) {
    this.valintatapajonoOid = valintatapajonoOid;
  }
}
