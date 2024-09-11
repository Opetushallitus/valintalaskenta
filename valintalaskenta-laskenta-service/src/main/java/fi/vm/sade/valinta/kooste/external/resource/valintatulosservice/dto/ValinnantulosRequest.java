package fi.vm.sade.valinta.kooste.external.resource.valintatulosservice.dto;

import java.util.List;

public class ValinnantulosRequest {
  private AuditSession auditSession;
  private List<Valinnantulos> valinnantulokset;

  public ValinnantulosRequest(AuditSession auditSession, List<Valinnantulos> valinnantulokset) {
    this.auditSession = auditSession;
    this.valinnantulokset = valinnantulokset;
  }

  public AuditSession getAuditSession() {
    return auditSession;
  }

  public void setAuditSession(AuditSession auditSession) {
    this.auditSession = auditSession;
  }

  public List<Valinnantulos> getValinnantulokset() {
    return valinnantulokset;
  }

  public void setValinnantulokset(List<Valinnantulos> valinnantulokset) {
    this.valinnantulokset = valinnantulokset;
  }
}
