package fi.vm.sade.valintalaskenta.laskenta.service.valinta;

import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteetTyyppi;

import java.util.List;

/**
 * 
 * @author Jussi Jartamo
 * 
 *         Konvertoi WSDL tyypit laskentaa varten
 * 
 */
public interface ValintalaskentaSuorittajaService {

    /**
     * 
     * @param hakemukset
     * @param valintaperusteet
     */
    void suoritaLaskenta(List<HakemusTyyppi> hakemukset, List<ValintaperusteetTyyppi> valintaperusteet);

}
