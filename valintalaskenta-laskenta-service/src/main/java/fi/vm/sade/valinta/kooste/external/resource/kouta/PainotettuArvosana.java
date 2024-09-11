package fi.vm.sade.valinta.kooste.external.resource.kouta;

import fi.vm.sade.valinta.kooste.external.resource.kouta.dto.PainotettuArvosanaDTO;
import java.math.BigDecimal;

public class PainotettuArvosana {
  public final String koodiUri;
  public final BigDecimal painokerroin;

  public PainotettuArvosana(String koodiUri, BigDecimal painokerroin) {
    this.koodiUri = koodiUri;
    this.painokerroin = painokerroin;
  }

  public PainotettuArvosana(PainotettuArvosanaDTO dto) {
    this.koodiUri = dto.getKoodiUri();
    this.painokerroin = dto.getPainokerroin();
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;

    PainotettuArvosana that = (PainotettuArvosana) o;

    return koodiUri.equals(that.koodiUri);
  }

  @Override
  public int hashCode() {
    return koodiUri.hashCode();
  }
}
