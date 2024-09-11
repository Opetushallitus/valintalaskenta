package fi.vm.sade.valinta.kooste.external.resource.kouta.dto;

import java.math.BigDecimal;

public class PainotettuArvosanaDTO {
  private String koodiUri;
  private BigDecimal painokerroin;

  public String getKoodiUri() {
    return koodiUri;
  }

  public void setKoodiUri(String koodiUri) {
    this.koodiUri = koodiUri;
  }

  public BigDecimal getPainokerroin() {
    return painokerroin;
  }

  public void setPainokerroin(BigDecimal painokerroin) {
    this.painokerroin = painokerroin;
  }
}
