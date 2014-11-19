package fi.vm.sade.valintalaskenta.laskenta.service.valinta;

import fi.vm.sade.service.valintaperusteet.laskenta.Lukuarvofunktio;
import fi.vm.sade.service.valintaperusteet.laskenta.Totuusarvofunktio;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakemus;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakukohde;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.HakemusWrapper;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.JonosijaJaSyotetytArvot;

import java.util.List;
import java.util.Map;

/**
 * User: wuoti
 * Date: 5.9.2013
 * Time: 9.44
 */
public interface HakemuslaskinService {

    void suoritaHakijaryhmaLaskentaHakemukselle(Hakukohde hakukohde, HakemusWrapper laskettavaHakemus,
                                     List<Hakemus> kaikkiHakemukset,
                                     Lukuarvofunktio lukuarvofunktio,
                                     Map<String, JonosijaJaSyotetytArvot> jonosijatHakemusOidinMukaan);

    void suoritaHakijaryhmaLaskentaHakemukselle(Hakukohde hakukohde, HakemusWrapper laskettavaHakemus,
                                                List<Hakemus> kaikkiHakemukset,
                                                Totuusarvofunktio totuusarvofunktio,
                                                Map<String, JonosijaJaSyotetytArvot> jonosijatHakemusOidinMukaan);


    void suoritaLaskentaHakemukselle(Hakukohde hakukohde, HakemusWrapper laskettavaHakemus,
                                     List<Hakemus> kaikkiHakemukset,
                                     Lukuarvofunktio lukuarvofunktio,
                                     int jkPrioriteetti,
                                     Valinnanvaihe edellinenVaihe,
                                     Map<String, JonosijaJaSyotetytArvot> jonosijatHakemusOidinMukaan,
                                     String jkNimi, int jarjestysnumero);

    void suoritaLaskentaHakemukselle(Hakukohde hakukohde, HakemusWrapper laskettavaHakemus,
                                     List<Hakemus> kaikkiHakemukset,
                                     Totuusarvofunktio lukuarvofunktio,
                                     int jkPrioriteetti,
                                     Valinnanvaihe edellinenVaihe,
                                     Map<String, JonosijaJaSyotetytArvot> jonosijatHakemusOidinMukaan,
                                     String jkNimi, int jarjestysnumero);
}
