package fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.impl;

import java.math.BigDecimal;
import java.util.Map;

import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakemus;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakukohde;
import fi.vm.sade.service.valintaperusteet.laskenta.api.LaskentaService;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Laskentatulos;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Hylattytila;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Tila;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Virhetila;
import fi.vm.sade.service.valintaperusteet.model.Funktiokutsu;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.OsallistuminenTulos;
import fi.vm.sade.valintalaskenta.laskenta.service.exception.LaskentaVaarantyyppisellaFunktiollaException;
import fi.vm.sade.valintalaskenta.laskenta.service.impl.LaskentadomainkonvertteriWrapper;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.Valintakoeosallistumislaskin;

/**
 * User: wuoti Date: 6.5.2013 Time: 9.02
 */
@Service
public class ValintakoeosallistumislaskinImpl implements Valintakoeosallistumislaskin {

	private final LaskentaService laskentaService;
	private final LaskentadomainkonvertteriWrapper laskentadomainkonvertteriWrapper;

    @Autowired
    public ValintakoeosallistumislaskinImpl(LaskentaService laskentaService, LaskentadomainkonvertteriWrapper laskentadomainkonvertteriWrapper) {
        this.laskentaService = laskentaService;
        this.laskentadomainkonvertteriWrapper = laskentadomainkonvertteriWrapper;
    }


    private OsallistuminenTulos muodostaTulos(Laskentatulos tulos) {
        Osallistuminen osallistuminen = resolveOsallistuminen(tulos);

        OsallistuminenTulos osallistuminenTulos = new OsallistuminenTulos();
        osallistuminenTulos.setOsallistuminen(osallistuminen);
        osallistuminenTulos.setLaskentaTila(tulos.getTila().getTilatyyppi()
                .name());
        if(tulos.getTulos() instanceof Boolean) {
            osallistuminenTulos.setLaskentaTulos((Boolean)tulos.getTulos());
        } else {
            if(tulos.getTulos() == null) {
                osallistuminenTulos.setLaskentaTulos(false);
            } else {
                osallistuminenTulos.setLaskentaTulos(true);
            }
        }


        Map<String, String> kuvaus = null;
        String tekninen = null;
        if (tulos.getTila() instanceof Hylattytila) {
            kuvaus = ((Hylattytila) tulos.getTila()).getKuvaus();
            tekninen = ((Hylattytila) tulos.getTila()).getTekninenKuvaus();
        } else if (tulos.getTila() instanceof Virhetila) {
            kuvaus = ((Virhetila) tulos.getTila()).getKuvaus();
        }
        osallistuminenTulos.setKuvaus(kuvaus);
        osallistuminenTulos.setTekninenKuvaus(tekninen);
        return osallistuminenTulos;
    }

    private Osallistuminen resolveOsallistuminen(Laskentatulos tulos) {
        // Jos tulosta ei ole saatu laskettua (ts. sitä ei ole) tai jos
        // tuloksen tila on hylätty, voidaan
        // olettaa, että henkilön pitää osallistua valintakokeeseen
        if (Tila.Tilatyyppi.VIRHE.equals(tulos.getTila().getTilatyyppi())) {
            // Palautetaan virhe, jos laskenta palautti virheen
            return Osallistuminen.VIRHE;
        } else if (tulos.getTulos() == null
                || Tila.Tilatyyppi.HYLATTY.equals(tulos.getTila().getTilatyyppi())) {
            return Osallistuminen.OSALLISTUU;
        } else {
            if (tulos.getTulos() instanceof Boolean) {
                if ((Boolean) tulos.getTulos()) {
                    return Osallistuminen.OSALLISTUU;
                }
                else {
                    return Osallistuminen.EI_OSALLISTU;
                }
            } else {
                if (tulos.getTulos() == null) {
                    return Osallistuminen.OSALLISTUU;
                }
                else {
                    return Osallistuminen.EI_OSALLISTU;
                }
            }
        }
    }

    @Override
    public OsallistuminenTulos laskeOsallistuminenYhdelleHakukohteelle(Hakukohde hakukohde, Hakemus hakemus, Funktiokutsu kaava) {
        switch (kaava.getFunktionimi().getTyyppi()) {
            case TOTUUSARVOFUNKTIO:
                Laskentatulos<Boolean> tulos = laskentaService
                        .suoritaValintakoelaskenta(hakukohde, hakemus, laskentadomainkonvertteriWrapper
                                .muodostaTotuusarvolasku(kaava));
                return muodostaTulos(tulos);

            case LUKUARVOFUNKTIO:
                Laskentatulos<BigDecimal> lukuarvotulos = laskentaService
                        .suoritaValintakoelaskenta(hakukohde, hakemus, laskentadomainkonvertteriWrapper
                                .muodostaLukuarvolasku(kaava));
                return muodostaTulos(lukuarvotulos);

            default:
                throw new LaskentaVaarantyyppisellaFunktiollaException(
                        "Palvelu hyväksyy vain totuusarvo- ja lukuarvofunktioita!");
        }
    }
}
