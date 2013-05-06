package fi.vm.sade.valintalaskenta.laskenta.service.impl;

import fi.vm.sade.kaava.Laskentadomainkonvertteri;
import fi.vm.sade.service.valintaperusteet.laskenta.Lukuarvofunktio;
import fi.vm.sade.service.valintaperusteet.laskenta.Totuusarvofunktio;
import fi.vm.sade.service.valintaperusteet.model.Funktiokutsu;
import org.springframework.stereotype.Component;

/**
 * User: wuoti
 * Date: 6.5.2013
 * Time: 9.07
 */
@Component
public class LaskendomainkonvertteriWrapper {

    public Totuusarvofunktio muodostaTotuusarvolasku(Funktiokutsu funktiokutsu) {
        return Laskentadomainkonvertteri.muodostaTotuusarvolasku(funktiokutsu);
    }

    public Lukuarvofunktio muodostaLukuarvolasku(Funktiokutsu funktiokutsu) {
        return Laskentadomainkonvertteri.muodostaLukuarvolasku(funktiokutsu);
    }
}
