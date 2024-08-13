package fi.vm.sade.valintalaskenta.domain.siirtotiedosto;

import java.util.UUID;

public class Poistettu {
  private UUID id;
  private UUID parentId;
  private String tunniste;
  private boolean isDeletedItself = true;

  public Poistettu() {}

  public Poistettu(UUID id, UUID parentId, String tunniste) {
    this.id = id;
    this.parentId = parentId;
    this.tunniste = tunniste;
  }

  public UUID getId() {
    return id;
  }

  public void setId(UUID id) {
    this.id = id;
  }

  public UUID getParentId() {
    return parentId;
  }

  public void setParentId(UUID parentId) {
    this.parentId = parentId;
  }

  public String getTunniste() {
    return tunniste;
  }

  public void setTunniste(String tunniste) {
    this.tunniste = tunniste;
  }

  public boolean isDeletedItself() {
    return isDeletedItself;
  }

  public Poistettu setDeletedItself(boolean deletedItself) {
    isDeletedItself = deletedItself;
    return this;
  }
}
