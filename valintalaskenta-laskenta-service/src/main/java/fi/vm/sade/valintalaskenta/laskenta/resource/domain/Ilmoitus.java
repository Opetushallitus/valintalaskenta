package fi.vm.sade.valintalaskenta.laskenta.resource.domain;

import fi.vm.sade.valintalaskenta.domain.dto.seuranta.IlmoitusDto;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.IlmoitusTyyppi;
import java.util.Date;
import java.util.List;

public class Ilmoitus {

  private final IlmoitusTyyppi tyyppi;
  private final String otsikko;
  private final Date paivamaara;
  private final List<String> data;

  public Ilmoitus() {
    this.tyyppi = null;
    this.otsikko = null;
    this.paivamaara = null;
    this.data = null;
  }

  public Ilmoitus(IlmoitusTyyppi tyyppi, String otsikko, List<String> data) {
    this.tyyppi = tyyppi;
    this.otsikko = otsikko;
    this.data = data;
    this.paivamaara = new Date();
  }

  public IlmoitusDto asDto() {
    return new IlmoitusDto(
        tyyppi, otsikko, data, paivamaara == null ? new Date().getTime() : paivamaara.getTime());
  }

  public List<String> getData() {
    return data;
  }

  public String getOtsikko() {
    return otsikko;
  }

  public Date getPaivamaara() {
    return paivamaara;
  }
}
