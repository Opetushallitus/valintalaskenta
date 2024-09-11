package fi.vm.sade.valinta.kooste.external.resource.sijoittelu;

import com.fasterxml.jackson.annotation.JsonInclude;

@JsonInclude(JsonInclude.Include.NON_NULL)
public class ValintatulosUpdateStatus {
  public int status;
  public String message;
  public String valintatapajonoOid;
  public String hakemusOid;

  public ValintatulosUpdateStatus() {}

  public ValintatulosUpdateStatus(
      int status, String message, String valintatapajonoOid, String hakemusOid) {
    this.status = status;
    this.message = message;
    this.valintatapajonoOid = valintatapajonoOid;
    this.hakemusOid = hakemusOid;
  }

  @Override
  public String toString() {
    return "status:"
        + status
        + " message:"
        + message
        + " valintatapajonoOid:"
        + valintatapajonoOid
        + " hakemusOid:"
        + hakemusOid;
  }
}
