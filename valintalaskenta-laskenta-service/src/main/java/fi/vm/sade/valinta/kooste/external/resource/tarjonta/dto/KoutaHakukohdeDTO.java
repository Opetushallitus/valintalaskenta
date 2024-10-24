package fi.vm.sade.valinta.kooste.external.resource.tarjonta.dto;

public class KoutaHakukohdeDTO {
  public final String oid;

  private KoutaHakukohdeDTO() {
    this.oid = null;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;

    KoutaHakukohdeDTO that = (KoutaHakukohdeDTO) o;

    return oid.equals(that.oid);
  }

  @Override
  public int hashCode() {
    return oid.hashCode();
  }
}
