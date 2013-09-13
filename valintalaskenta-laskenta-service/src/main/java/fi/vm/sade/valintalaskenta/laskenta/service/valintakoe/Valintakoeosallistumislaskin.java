package fi.vm.sade.valintalaskenta.laskenta.service.valintakoe;

import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakukohde;
import fi.vm.sade.service.valintaperusteet.schema.FunktiokutsuTyyppi;
import fi.vm.sade.valintalaskenta.domain.valintakoe.OsallistuminenTulos;

/**
 * User: wuoti
 * Date: 6.5.2013
 * Time: 8.50
 */
public interface Valintakoeosallistumislaskin {

    OsallistuminenTulos laskeOsallistuminenYhdelleHakukohteelle(Hakukohde hakukohde, HakemusTyyppi hakemus,
                                                                FunktiokutsuTyyppi kaava);
}
