package fi.vm.sade.valintalaskenta.laskenta.service.valintakoe;

import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteetTyyppi;

import java.util.List;

/**
 * User: wuoti
 * Date: 2.5.2013
 * Time: 9.15
 */
public interface ValintakoelaskentaSuorittajaService {

    void laske(HakemusTyyppi hakemus,  List<ValintaperusteetTyyppi> valintaperusteet);
}