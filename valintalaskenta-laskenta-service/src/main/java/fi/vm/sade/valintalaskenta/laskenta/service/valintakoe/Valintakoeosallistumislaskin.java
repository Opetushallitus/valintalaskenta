package fi.vm.sade.valintalaskenta.laskenta.service.valintakoe;

import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.FunktiokutsuTyyppi;

/**
 * User: wuoti
 * Date: 6.5.2013
 * Time: 8.50
 */
public interface Valintakoeosallistumislaskin {

    boolean laskeOsallistuminenYhdelleHakukohteelle(String hakukohdeOid, HakemusTyyppi hakemus,
                                                    FunktiokutsuTyyppi kaava);
}
