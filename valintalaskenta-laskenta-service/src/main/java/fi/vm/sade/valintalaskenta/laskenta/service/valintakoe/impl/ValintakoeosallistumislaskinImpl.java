package fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.impl;

import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakukohde;
import fi.vm.sade.service.valintaperusteet.laskenta.api.LaskentaService;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Laskentatulos;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Hylattytila;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Tila;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Virhetila;
import fi.vm.sade.service.valintaperusteet.model.Funktiokutsu;
import fi.vm.sade.service.valintaperusteet.schema.FunktiokutsuTyyppi;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.OsallistuminenTulos;
import fi.vm.sade.valintalaskenta.laskenta.service.exception.LaskentaVaarantyyppisellaFunktiollaException;
import fi.vm.sade.valintalaskenta.laskenta.service.impl.LaskentadomainkonvertteriWrapper;
import fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion.FunktioKutsuTyyppiToFunktioKutsuConverter;
import fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion.HakemusTyyppiToHakemusConverter;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.Valintakoeosallistumislaskin;

/**
 * User: wuoti Date: 6.5.2013 Time: 9.02
 */
@Service
public class ValintakoeosallistumislaskinImpl implements
		Valintakoeosallistumislaskin {

	@Autowired
	private FunktioKutsuTyyppiToFunktioKutsuConverter funktiokutsuConverter;

	@Autowired
	private LaskentaService laskentaService;

	@Autowired
	private HakemusTyyppiToHakemusConverter hakemusConverter;

	@Autowired
	private LaskentadomainkonvertteriWrapper laskentadomainkonvertteriWrapper;

	@Override
	public OsallistuminenTulos laskeOsallistuminenYhdelleHakukohteelle(
			Hakukohde hakukohde, HakemusTyyppi hakemus, FunktiokutsuTyyppi kaava) {
		Funktiokutsu funktiokutsu = funktiokutsuConverter.convert(kaava);

		switch (funktiokutsu.getFunktionimi().getTyyppi()) {
		case TOTUUSARVOFUNKTIO:
			Laskentatulos<Boolean> tulos = laskentaService
					.suoritaValintakoelaskenta(hakukohde, hakemusConverter
							.convert(hakemus), laskentadomainkonvertteriWrapper
							.muodostaTotuusarvolasku(funktiokutsu));

			// Jos tulosta ei ole saatu laskettua (ts. sitä ei ole) tai jos
			// tuloksen tila on hylätty, voidaan
			// olettaa, että henkilön pitää osallistua valintakokeeseen
			Osallistuminen osallistuminen = null;
			if (Tila.Tilatyyppi.VIRHE.equals(tulos.getTila().getTilatyyppi())) {
				// Palautetaan virhe, jos laskenta palautti virheen
				osallistuminen = Osallistuminen.VIRHE;
			} else if (tulos.getTulos() == null
					|| Tila.Tilatyyppi.HYLATTY.equals(tulos.getTila()
							.getTilatyyppi())) {
				osallistuminen = Osallistuminen.OSALLISTUU;
			} else {
				osallistuminen = tulos.getTulos() ? Osallistuminen.OSALLISTUU
						: Osallistuminen.EI_OSALLISTU;
			}

			OsallistuminenTulos osallistuminenTulos = new OsallistuminenTulos();
			osallistuminenTulos.setOsallistuminen(osallistuminen);
			osallistuminenTulos.setLaskentaTila(tulos.getTila().getTilatyyppi()
					.name());
			osallistuminenTulos.setLaskentaTulos(tulos.getTulos());

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

		default:
			throw new LaskentaVaarantyyppisellaFunktiollaException(
					"Palvelu hyväksyy vain totuusarvofunktioita!");
		}
	}
}
