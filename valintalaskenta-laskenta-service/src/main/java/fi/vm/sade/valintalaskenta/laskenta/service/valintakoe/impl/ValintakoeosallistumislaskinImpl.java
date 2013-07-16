package fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.valintaperusteet.laskenta.api.LaskentaService;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Laskentatulos;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Tila;
import fi.vm.sade.service.valintaperusteet.model.Funktiokutsu;
import fi.vm.sade.service.valintaperusteet.schema.FunktiokutsuTyyppi;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;
import fi.vm.sade.valintalaskenta.laskenta.service.exception.LaskentaVaarantyyppisellaFunktiollaException;
import fi.vm.sade.valintalaskenta.laskenta.service.impl.LaskentadomainkonvertteriWrapper;
import fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion.FunktioKutsuTyyppiToFunktioKutsuConverter;
import fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion.HakemusTyyppiToHakemusConverter;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.Valintakoeosallistumislaskin;

/**
 * User: wuoti Date: 6.5.2013 Time: 9.02
 */
@Service
public class ValintakoeosallistumislaskinImpl implements Valintakoeosallistumislaskin {

    @Autowired
    private FunktioKutsuTyyppiToFunktioKutsuConverter funktiokutsuConverter;

    @Autowired
    private LaskentaService laskentaService;

    @Autowired
    private HakemusTyyppiToHakemusConverter hakemusConverter;

    @Autowired
    private LaskentadomainkonvertteriWrapper laskentadomainkonvertteriWrapper;

    @Override
    public Osallistuminen laskeOsallistuminenYhdelleHakukohteelle(String hakukohdeOid, HakemusTyyppi hakemus,
            FunktiokutsuTyyppi kaava) {
        Funktiokutsu funktiokutsu = funktiokutsuConverter.convert(kaava);

        switch (funktiokutsu.getFunktionimi().getTyyppi()) {
        case TOTUUSARVOFUNKTIO:
            Laskentatulos<Boolean> tulos = laskentaService.suoritaLasku(hakukohdeOid,
                    hakemusConverter.convert(hakemus),
                    laskentadomainkonvertteriWrapper.muodostaTotuusarvolasku(funktiokutsu), new StringBuffer());

            // Jos tulosta ei ole saatu laskettua (ts. sitä ei ole) tai jos
            // tuloksen tila on hylätty, voidaan
            // olettaa, että henkilön pitää osallistua valintakokeeseen
            if (tulos.getTulos() == null || Tila.Tilatyyppi.HYLATTY.equals(tulos.getTila().getTilatyyppi())) {
                return Osallistuminen.OSALLISTUU;
            } else {
                return tulos.getTulos() ? Osallistuminen.OSALLISTUU : Osallistuminen.EI_OSALLISTU;
            }

        default:
            throw new LaskentaVaarantyyppisellaFunktiollaException("Palvelu hyväksyy vain totuusarvofunktioita!");
        }
    }
}
