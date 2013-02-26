package fi.vm.sade.service.valintalaskenta.service;

import java.util.List;

import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteetTyyppi;

/**
 * 
 * @author Jussi Jartamo
 * 
 *         Konvertoi WSDL tyypit laskentaa varten
 * 
 */
public interface ValintalaskentaSuorittajaService {

    public void suoritaLaskenta(List<HakemusTyyppi> hakemukset, List<ValintaperusteetTyyppi> valintaperusteet);

}
