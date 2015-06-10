package fi.vm.sade.valintalaskenta.laskenta.service.impl;

import fi.vm.sade.kaava.Laskentadomainkonvertteri;
import fi.vm.sade.service.valintaperusteet.laskenta.Lukuarvofunktio;
import fi.vm.sade.service.valintaperusteet.laskenta.Totuusarvofunktio;
import fi.vm.sade.service.valintaperusteet.model.Funktiokutsu;
import org.springframework.stereotype.Component;

@Component
public class LaskentadomainkonvertteriWrapper {

    public Totuusarvofunktio muodostaTotuusarvolasku(Funktiokutsu funktiokutsu) {
        return Laskentadomainkonvertteri.muodostaTotuusarvolasku(funktiokutsu);
    }

    public Lukuarvofunktio muodostaLukuarvolasku(Funktiokutsu funktiokutsu) {
        return Laskentadomainkonvertteri.muodostaLukuarvolasku(funktiokutsu);
    }
}
