package fi.vm.sade.valintalaskenta.domain.dto.seuranta;

import java.util.Arrays;
import java.util.Date;
import java.util.List;

public class IlmoitusDto {
  private IlmoitusTyyppi tyyppi;
  private String otsikko;
  private long paivamaara;
  private List<String> data;

  public IlmoitusDto() {
    // Dummy constructor for JSON parser
  }

  public IlmoitusDto(IlmoitusTyyppi tyyppi, String otsikko) {
    this.tyyppi = tyyppi;
    this.otsikko = otsikko;
    this.data = null;
    this.paivamaara = new Date().getTime();
  }

  public IlmoitusDto(IlmoitusTyyppi tyyppi, String otsikko, List<String> data) {
    this.tyyppi = tyyppi;
    this.otsikko = otsikko;
    this.data = data;
    this.paivamaara = new Date().getTime();
  }

  public IlmoitusDto(IlmoitusTyyppi tyyppi, String otsikko, List<String> data, long paivamaara) {
    this.tyyppi = tyyppi;
    this.otsikko = otsikko;
    this.data = data;
    this.paivamaara = paivamaara;
  }

  public long getPaivamaara() {
    return paivamaara;
  }

  public List<String> getData() {
    return data;
  }

  public String getOtsikko() {
    return otsikko;
  }

  public IlmoitusTyyppi getTyyppi() {
    return tyyppi;
  }

  public static IlmoitusDto virheilmoitus(String virhe, String... dataa) {
    return new IlmoitusDto(IlmoitusTyyppi.VIRHE, virhe, Arrays.asList(dataa));
  }

  public static IlmoitusDto ilmoitus(String ilmoitus, String... dataa) {
    return new IlmoitusDto(IlmoitusTyyppi.ILMOITUS, ilmoitus, Arrays.asList(dataa));
  }

  @Override
  public String toString() {
    return "IlmoitusDto{"
        + "tyyppi="
        + tyyppi
        + ", otsikko='"
        + otsikko
        + '\''
        + ", paivamaara="
        + paivamaara
        + ", data="
        + data
        + '}';
  }
}
