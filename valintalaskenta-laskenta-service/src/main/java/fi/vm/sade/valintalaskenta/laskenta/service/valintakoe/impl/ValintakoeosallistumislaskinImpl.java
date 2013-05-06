package fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.impl;

import fi.vm.sade.kaava.Laskentadomainkonvertteri;
import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.valintaperusteet.laskenta.api.LaskentaService;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Laskentatulos;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Tila;
import fi.vm.sade.service.valintaperusteet.model.Funktiokutsu;
import fi.vm.sade.service.valintaperusteet.schema.FunktiokutsuTyyppi;
import fi.vm.sade.valintalaskenta.laskenta.service.exception.LaskentaVaarantyyppisellaFunktiollaException;
import fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion.FunktioKutsuTyyppiToFunktioKutsuConverter;
import fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion.HakemusTyyppiToHakemusConverter;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.Valintakoeosallistumislaskin;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * User: wuoti
 * Date: 6.5.2013
 * Time: 9.02
 */
@Service
public class ValintakoeosallistumislaskinImpl implements Valintakoeosallistumislaskin {

    @Autowired
    private FunktioKutsuTyyppiToFunktioKutsuConverter funktiokutsuConverter;

    @Autowired
    private LaskentaService laskentaService;

    @Autowired
    private HakemusTyyppiToHakemusConverter hakemusConverter;

    @Override
    public boolean laskeOsallistuminenYhdelleHakukohteelle(String hakukohdeOid, HakemusTyyppi hakemus,
                                                           FunktiokutsuTyyppi kaava) {
        Funktiokutsu funktiokutsu = funktiokutsuConverter.convert(kaava);

        switch (funktiokutsu.getFunktionimi().getTyyppi()) {
            case TOTUUSARVOFUNKTIO:
                Laskentatulos<Boolean> tulos = laskentaService.suoritaLasku(hakukohdeOid,
                        hakemusConverter.convert(hakemus),
                        Laskentadomainkonvertteri.muodostaTotuusarvolasku(funktiokutsu));

                // Jos tulosta ei ole saatu laskettua (ts. sitä ei ole) tai jos tuloksen tila on hylätty, voidaan
                // olettaa, että henkilön pitää osallistua valintakokeeseen
                if (tulos.getTulos() == null || Tila.Tilatyyppi.HYLATTY.equals(tulos.getTila().getTilatyyppi())) {
                    return true;
                } else {
                    // muussa tapauksessa palautetaan laskettu tulos
                    return tulos.getTulos();
                }

            default:
                throw new LaskentaVaarantyyppisellaFunktiollaException("Palvelu hyväksyy vain totuusarvofunktioita!");
        }
    }
}
