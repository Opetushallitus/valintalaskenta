package fi.vm.sade.valintalaskenta.laskenta.service.valinta;

import fi.vm.sade.service.valintaperusteet.laskenta.Lukuarvofunktio;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakemus;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakukohde;
import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.HakemusWrapper;

import java.util.List;
import java.util.Map;

/**
 * User: wuoti
 * Date: 5.9.2013
 * Time: 9.44
 */
public interface HakemuslaskinService {
    void suoritaLaskentaHakemukselle(Hakukohde hakukohde, HakemusWrapper laskettavaHakemus,
                                     List<Hakemus> kaikkiHakemukset,
                                     Lukuarvofunktio lukuarvofunktio,
                                     int jkPrioriteetti,
                                     Valinnanvaihe edellinenVaihe,
                                     Map<String, Jonosija> jonosijatHakemusOidinMukaan);
}
