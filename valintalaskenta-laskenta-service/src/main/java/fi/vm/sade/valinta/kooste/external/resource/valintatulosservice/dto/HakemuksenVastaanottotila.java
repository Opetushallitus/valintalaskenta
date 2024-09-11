package fi.vm.sade.valinta.kooste.external.resource.valintatulosservice.dto;

import fi.vm.sade.sijoittelu.domain.ValintatuloksenTila;

public class HakemuksenVastaanottotila {

  private String hakemusOid;
  private ValintatuloksenTila vastaanottotila;
  private String valintatapajonoOid;

  public String getHakemusOid() {
    return hakemusOid;
  }

  public String getValintatapajonoOid() {
    return valintatapajonoOid;
  }

  public ValintatuloksenTila getVastaanottotila() {
    return vastaanottotila;
  }
}
