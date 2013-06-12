package fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl;

import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakemus;

/**
 * Created with IntelliJ IDEA.
 * User: kkammone
 * Date: 16.5.2013
 * Time: 12:03
 * To change this template use File | Settings | File Templates.
 */
public class HakemusWrapper {

    private HakemusTyyppi hakemusTyyppi;

    private Hakemus laskentahakemus;

    private boolean harkinnanvaraisuus = false;

    public HakemusTyyppi getHakemusTyyppi() {
        return hakemusTyyppi;
    }

    public void setHakemusTyyppi(HakemusTyyppi hakemusTyyppi) {
        this.hakemusTyyppi = hakemusTyyppi;
    }

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
}
