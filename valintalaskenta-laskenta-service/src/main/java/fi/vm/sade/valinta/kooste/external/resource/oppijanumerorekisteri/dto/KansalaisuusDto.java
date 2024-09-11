package fi.vm.sade.valinta.kooste.external.resource.oppijanumerorekisteri.dto;

import java.io.Serializable;

public class KansalaisuusDto implements Serializable {
  private static final long serialVersionUID = -1616181528688301217L;

  private String kansalaisuusKoodi;

  public KansalaisuusDto() {}

  public KansalaisuusDto(String kansalaisuusKoodi) {
    this.kansalaisuusKoodi = kansalaisuusKoodi;
  }

  public String getKansalaisuusKoodi() {
    return kansalaisuusKoodi;
  }

  public void setKansalaisuusKoodi(String kansalaisuusKoodi) {
    this.kansalaisuusKoodi = kansalaisuusKoodi;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;

    KansalaisuusDto that = (KansalaisuusDto) o;

    return kansalaisuusKoodi.equals(that.kansalaisuusKoodi);
  }

  @Override
  public int hashCode() {
    return kansalaisuusKoodi.hashCode();
  }
}
