package fi.vm.sade.valintalaskenta.domain.siirtotiedosto;

import java.util.UUID;

public class Poistettu {
  private UUID id;
  private UUID parentId;
  private String tunniste;
  private boolean isDeletedItself = true;

  public Poistettu() {}

  public Poistettu(Object[] raw) {
    this.id = raw[0] != null ? UUID.fromString(raw[0].toString()) : null;
    this.parentId = raw[1] != null ? UUID.fromString(raw[1].toString()) : null;
    this.tunniste = raw[2] != null ? raw[2].toString() : null;
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
