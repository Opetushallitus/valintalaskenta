package fi.vm.sade.valintalaskenta.laskenta.service.valintakoe;

import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakemus;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakukohde;
import fi.vm.sade.service.valintaperusteet.model.Funktiokutsu;
import fi.vm.sade.valintalaskenta.domain.valintakoe.OsallistuminenTulos;

/**
 * User: wuoti
 * Date: 6.5.2013
 * Time: 8.50
 */
public interface Valintakoeosallistumislaskin {

    OsallistuminenTulos laskeOsallistuminenYhdelleHakukohteelleRest(Hakukohde hakukohde, Hakemus hakemus,
                                                                Funktiokutsu kaava);
}
