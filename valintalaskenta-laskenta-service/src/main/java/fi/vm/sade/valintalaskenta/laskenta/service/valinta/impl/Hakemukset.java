package fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl;

import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakemus;
import java.util.ArrayList;
import java.util.List;

/** User: wuoti Date: 4.9.2013 Time: 16.27 */
public class Hakemukset {
  public List<HakemusWrapper> hakemukset = new ArrayList<HakemusWrapper>();
  public List<Hakemus> laskentahakemukset = new ArrayList<Hakemus>();

  public List<HakemusWrapper> getHakemukset() {
    return hakemukset;
  }

  public void setHakemukset(List<HakemusWrapper> hakemukset) {
    this.hakemukset = hakemukset;
  }

  public List<Hakemus> getLaskentahakemukset() {
    return laskentahakemukset;
  }

  public void setLaskentahakemukset(List<Hakemus> laskentahakemukset) {
    this.laskentahakemukset = laskentahakemukset;
  }
}
