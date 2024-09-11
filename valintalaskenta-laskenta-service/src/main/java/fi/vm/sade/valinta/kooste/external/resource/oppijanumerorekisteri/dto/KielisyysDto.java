package fi.vm.sade.valinta.kooste.external.resource.oppijanumerorekisteri.dto;

import java.io.Serializable;

public class KielisyysDto implements Serializable {
  private static final long serialVersionUID = 7217945009330980201L;

  private String kieliKoodi;
  private String kieliTyyppi;

  public KielisyysDto() {}

  public KielisyysDto(String kieliKoodi) {
    this.kieliKoodi = kieliKoodi;
  }

  public KielisyysDto(String kieliKoodi, String kieliTyyppi) {
    this.kieliKoodi = kieliKoodi;
    this.kieliTyyppi = kieliTyyppi;
  }

  public String getKieliKoodi() {
    return kieliKoodi;
  }

  public void setKieliKoodi(String kieliKoodi) {
    this.kieliKoodi = kieliKoodi;
  }

  public String getKieliTyyppi() {
    return kieliTyyppi;
  }

  public void setKieliTyyppi(String kieliTyyppi) {
    this.kieliTyyppi = kieliTyyppi;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;

    KielisyysDto that = (KielisyysDto) o;

    return kieliKoodi.equals(that.kieliKoodi);
  }

  @Override
  public int hashCode() {
    return kieliKoodi.hashCode();
  }
}
