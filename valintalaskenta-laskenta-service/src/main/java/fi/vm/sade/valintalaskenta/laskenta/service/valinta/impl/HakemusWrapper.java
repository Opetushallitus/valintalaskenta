package fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl;

import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakemus;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;

/** User: kkammone Date: 16.5.2013 Time: 12:03 */
public class HakemusWrapper {

  private Hakemus laskentahakemus;

  private boolean harkinnanvaraisuus = false;

  private int hakutoiveprioriteetti;

  public HakemusDTO getHakemusDTO() {
    return hakemusDTO;
  }

  public void setHakemusDTO(HakemusDTO hakemusDTO) {
    this.hakemusDTO = hakemusDTO;
  }

  public HakemusDTO hakemusDTO;

  public Hakemus getLaskentahakemus() {
    return laskentahakemus;
  }

  public void setLaskentahakemus(Hakemus laskentahakemus) {
    this.laskentahakemus = laskentahakemus;
  }

  public boolean isHarkinnanvaraisuus() {
    return harkinnanvaraisuus;
  }

  public void setHarkinnanvaraisuus(boolean harkinnanvaraisuus) {
    this.harkinnanvaraisuus = harkinnanvaraisuus;
  }

  public int getHakutoiveprioriteetti() {
    return hakutoiveprioriteetti;
  }

  public void setHakutoiveprioriteetti(int hakutoiveprioriteetti) {
    this.hakutoiveprioriteetti = hakutoiveprioriteetti;
  }
}
