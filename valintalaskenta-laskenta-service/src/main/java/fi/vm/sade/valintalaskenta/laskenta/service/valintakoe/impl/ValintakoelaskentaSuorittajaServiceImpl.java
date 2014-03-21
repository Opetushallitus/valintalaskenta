package fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.hakemus.schema.HakukohdeTyyppi;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakukohde;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Hyvaksyttavissatila;
import fi.vm.sade.service.valintaperusteet.schema.HakukohteenValintaperusteTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintakoeTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintakoeValinnanVaiheTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteetTyyppi;
import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Hakutoive;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.OsallistuminenTulos;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Valintakoe;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeValinnanvaihe;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValinnanvaiheDAO;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValintakoeOsallistuminenDAO;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.EdellinenValinnanvaiheKasittelija;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.TilaJaSelite;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.ValintakoelaskentaSuorittajaService;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.Valintakoeosallistumislaskin;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.impl.util.HakukohdeValintakoeData;

/**
 * User: wuoti Date: 2.5.2013 Time: 9.16
 */
@Service
public class ValintakoelaskentaSuorittajaServiceImpl implements
		ValintakoelaskentaSuorittajaService {

    private final String r = "\\{\\{([A-Za-z0–9\\-_]+)\\.([A-Za-z0–9\\-_]+)\\}\\}";
    private final Pattern pattern = Pattern.compile(r);

	private static final Logger LOG = LoggerFactory
			.getLogger(ValintakoelaskentaSuorittajaServiceImpl.class);

	@Autowired
	private ValintakoeOsallistuminenDAO valintakoeOsallistuminenDAO;

	@Autowired
	private Valintakoeosallistumislaskin valintakoeosallistumislaskin;

	@Autowired
	private ValinnanvaiheDAO valinnanvaiheDAO;

	@Autowired
	private EdellinenValinnanvaiheKasittelija edellinenValinnanvaiheKasittelija;

	private String haeTunniste(String mustache,
			Map<String, String> hakukohteenValintaperusteet) {

		final Matcher m = pattern.matcher(mustache);

		String avain = null;
		while (m.find()) {
			if (!m.group(1).isEmpty() && m.group(1).contentEquals("hakukohde")
					&& !m.group(2).isEmpty()) {
				avain = m.group(2);
			}
		}
		if (avain == null) {
			return mustache;
		} else {
			String arvo = hakukohteenValintaperusteet.get(avain);
			return arvo;
		}

	}

	@Override
	public void laske(HakemusTyyppi hakemus,
			List<ValintaperusteetTyyppi> valintaperusteet) {

		LOG.info("Laskentaan valintakoeosallistumiset hakemukselle {}",
				hakemus.getHakemusOid());

		final Map<String, HakukohdeTyyppi> hakutoiveetByOid = luoHakutoiveMap(hakemus
				.getHakutoive());
		Map<String, List<HakukohdeValintakoeData>> valintakoeData = new HashMap<String, List<HakukohdeValintakoeData>>();

		for (ValintaperusteetTyyppi vp : valintaperusteet) {
			Map<String, String> hakukohteenValintaperusteet = muodostaHakukohteenValintaperusteetMap(vp
					.getHakukohteenValintaperuste());

			if (hakutoiveetByOid.containsKey(vp.getHakukohdeOid())
					&& vp.getValinnanVaihe() instanceof ValintakoeValinnanVaiheTyyppi) {
				ValintakoeValinnanVaiheTyyppi vaihe = (ValintakoeValinnanVaiheTyyppi) vp
						.getValinnanVaihe();

				for (ValintakoeTyyppi koe : vaihe.getValintakoe()) {
					String tunniste = haeTunniste(koe.getTunniste(),
							hakukohteenValintaperusteet);
					if (tunniste == null) {
						LOG.error(
								"Valintakokoeen tunnistetta ei pystytty määrittelemään. HakukohdeOid: {} - ValintakoeOid: {}",
								vp.getHakukohdeOid(), koe.getOid());
						continue;
					}

					// Haetaan tätä valinnan vaihetta edeltävä varsinainen
					// valinnan vaihe, jos sellainen on olemassa
					Valinnanvaihe edellinenVaihe = valinnanvaiheDAO
							.haeEdeltavaValinnanvaihe(vp.getHakuOid(),
									vp.getHakukohdeOid(),
									vaihe.getValinnanVaiheJarjestysluku());

					// jos edellistä varsinaista valinnan vaihetta ei ole
					// olemassa ja järjestysnumero > 0,
					// tarkistetaaan löytyykö edellistä valintakoevaihetta vai
					// heitetäänö virhe
					if (edellinenVaihe == null
							&& vaihe.getValinnanVaiheJarjestysluku() > 0) {
						ValintakoeOsallistuminen edellinenOsallistuminen = valintakoeOsallistuminenDAO
								.haeEdeltavaValinnanvaihe(vp.getHakuOid(),
										vp.getHakukohdeOid(),
										vaihe.getValinnanVaiheJarjestysluku());
						if (edellinenOsallistuminen == null) {
							LOG.warn("Valinnanvaiheen järjestysnumero on suurempi kuin 0, mutta edellistä valinnanvaihetta ei löytynyt");
							continue;
						}
					}

					// Haetaan viimeisin varsinainen valinnan vaihe, jos
					// sellainen on olemassa (tämä saattaa olla sama kuin
					// edeltävä vaihe)
					Valinnanvaihe viimeisinValinnanVaihe = null;
					if (vaihe.getValinnanVaiheJarjestysluku() > 0) {
						if (edellinenVaihe != null
								&& edellinenVaihe.getJarjestysnumero() == vaihe
										.getValinnanVaiheJarjestysluku() - 1) {
							viimeisinValinnanVaihe = edellinenVaihe;
						} else {
							viimeisinValinnanVaihe = valinnanvaiheDAO
									.haeViimeisinValinnanvaihe(
											vp.getHakuOid(),
											vp.getHakukohdeOid(),
											vaihe.getValinnanVaiheJarjestysluku());
						}
					}

					OsallistuminenTulos osallistuminen = null;
					if (viimeisinValinnanVaihe != null) {
						TilaJaSelite tilaJaSelite = edellinenValinnanvaiheKasittelija
								.tilaEdellisenValinnanvaiheenMukaan(
										hakemus.getHakemusOid(),
										new Hyvaksyttavissatila(),
										viimeisinValinnanVaihe);
						if (tilaJaSelite.getTila().equals(
								JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA)) {
							osallistuminen = valintakoeosallistumislaskin
									.laskeOsallistuminenYhdelleHakukohteelle(
											new Hakukohde(vp.getHakukohdeOid(),
													hakukohteenValintaperusteet),
											hakemus, koe.getFunktiokutsu());
						} else {
							osallistuminen = new OsallistuminenTulos();
							osallistuminen.setKuvaus(tilaJaSelite.getSelite());
                            osallistuminen.setTekninenKuvaus(tilaJaSelite.getTekninenSelite());
							osallistuminen.setLaskentaTila(tilaJaSelite
									.getTila().toString());
							osallistuminen
									.setOsallistuminen(Osallistuminen.EI_OSALLISTU);
						}
					} else {
						osallistuminen = valintakoeosallistumislaskin
								.laskeOsallistuminenYhdelleHakukohteelle(
										new Hakukohde(vp.getHakukohdeOid(),
												hakukohteenValintaperusteet),
										hakemus, koe.getFunktiokutsu());
					}

					HakukohdeValintakoeData data = new HakukohdeValintakoeData();
					data.setHakuOid(vp.getHakuOid());
					data.setHakukohdeOid(vp.getHakukohdeOid());
					data.setOsallistuminenTulos(osallistuminen);
					data.setValinnanVaiheJarjestysNro(vaihe
							.getValinnanVaiheJarjestysluku());
					data.setValinnanVaiheOid(vaihe.getValinnanVaiheOid());
					data.setValintakoeOid(koe.getOid());
					data.setValintakoeTunniste(tunniste);
					data.setNimi(koe.getNimi());

					if (!valintakoeData.containsKey(tunniste)) {
						valintakoeData.put(tunniste,
								new ArrayList<HakukohdeValintakoeData>());
					}

					valintakoeData.get(tunniste).add(data);
				}
			}
		}

		Map<String, ValintakoeOsallistuminen> osallistumisetByHaku = new HashMap<String, ValintakoeOsallistuminen>();
		for (Map.Entry<String, List<HakukohdeValintakoeData>> entry : valintakoeData
				.entrySet()) {
			List<HakukohdeValintakoeData> kokeet = entry.getValue();

			asetaOsallistumisetKokeisiin(kokeet, hakutoiveetByOid);
			for (HakukohdeValintakoeData c : kokeet) {
				LOG.info(
						"Hakukohde: {}, valintakoe: {}",
						new Object[] { c.getHakukohdeOid(),
								c.getValintakoeOid() });

				if (!osallistumisetByHaku.containsKey(c.getHakuOid())) {
					osallistumisetByHaku.put(c.getHakuOid(),
							luoValintakoeOsallistuminen(c, hakemus));
				}

				ValintakoeOsallistuminen osallistuminen = osallistumisetByHaku
						.get(c.getHakuOid());
				haeTaiLuoHakutoive(osallistuminen, c);
			}
		}

		for (ValintakoeOsallistuminen osallistuminen : osallistumisetByHaku
				.values()) {
			valintakoeOsallistuminenDAO.createOrUpdate(osallistuminen);
		}
	}

	private Map<String, String> muodostaHakukohteenValintaperusteetMap(
			List<HakukohteenValintaperusteTyyppi> hakukohteenValintaperuste) {
		Map<String, String> map = new HashMap<String, String>();

		for (HakukohteenValintaperusteTyyppi vp : hakukohteenValintaperuste) {
			map.put(vp.getTunniste(), vp.getArvo());
		}

		return map;
	}

	protected void asetaOsallistumisetKokeisiin(
			List<HakukohdeValintakoeData> kokeet,
			final Map<String, HakukohdeTyyppi> hakukohteetByOid) {

		// Käydään hakijan hakutoiveet läpi prioriteetin mukaan ja asetetaan
		// kullekin hakukohteelle
		// valintakoekohtainen osallistumistieto

		Collections.sort(kokeet, new Comparator<HakukohdeValintakoeData>() {
			@Override
			public int compare(HakukohdeValintakoeData o1,
					HakukohdeValintakoeData o2) {
				return hakukohteetByOid.get(o1.getHakukohdeOid())
						.getPrioriteetti()
						- hakukohteetByOid.get(o2.getHakukohdeOid())
								.getPrioriteetti();
			}
		});

		// Jos hakija osallistuu korkeamman prioriteetin hakuktoiveen
		// valintakokeeseen, hakija ei osallistu
		// pienemmällä prioriteetilla oleviin samoihin valintakokeisiin.
		boolean osallistuminenLoydetty = false;
		for (HakukohdeValintakoeData d : kokeet) {
			if (!osallistuminenLoydetty
					&& Osallistuminen.OSALLISTUU.equals(d
							.getOsallistuminenTulos().getOsallistuminen())) {
				osallistuminenLoydetty = true;
				continue;
			}

			if (!d.getOsallistuminenTulos().getOsallistuminen()
					.equals(Osallistuminen.VIRHE)) {
				d.getOsallistuminenTulos().setOsallistuminen(
						Osallistuminen.EI_OSALLISTU);
			}
		}
	}

	protected ValintakoeOsallistuminen luoValintakoeOsallistuminen(
			HakukohdeValintakoeData data, HakemusTyyppi hakemus) {
		ValintakoeOsallistuminen osallistuminen = valintakoeOsallistuminenDAO
				.readByHakuOidAndHakemusOid(data.getHakuOid(),
						hakemus.getHakemusOid());

		if (osallistuminen == null) {
			osallistuminen = new ValintakoeOsallistuminen();
		} else {
			osallistuminen.getHakutoiveet().clear();
		}

		osallistuminen.setHakuOid(data.getHakuOid());
		osallistuminen.setHakemusOid(hakemus.getHakemusOid());
		osallistuminen.setHakijaOid(hakemus.getHakijaOid());
		osallistuminen.setSukunimi(hakemus.getHakijanSukunimi());
		osallistuminen.setEtunimi(hakemus.getHakijanEtunimi());

		return osallistuminen;
	}

	protected void haeTaiLuoHakutoive(ValintakoeOsallistuminen osallistuminen,
			HakukohdeValintakoeData data) {
		Hakutoive toive = null;
		for (Hakutoive t : osallistuminen.getHakutoiveet()) {
			if (data.getHakukohdeOid().equals(t.getHakukohdeOid())) {
				toive = t;
				break;
			}
		}

		if (toive == null) {
			toive = new Hakutoive();
			osallistuminen.getHakutoiveet().add(toive);
		}
		toive.setHakukohdeOid(data.getHakukohdeOid());

		haeTaiLuoValinnanVaihe(toive, data);
	}

	protected void haeTaiLuoValinnanVaihe(Hakutoive hakutoive,
			HakukohdeValintakoeData data) {
		ValintakoeValinnanvaihe vaihe = null;

		for (ValintakoeValinnanvaihe v : hakutoive.getValinnanVaiheet()) {
			if (data.getValinnanVaiheOid().equals(v.getValinnanVaiheOid())) {
				vaihe = v;
				break;
			}
		}

		if (vaihe == null) {
			vaihe = new ValintakoeValinnanvaihe();
			hakutoive.getValinnanVaiheet().add(vaihe);
		}
		vaihe.setValinnanVaiheOid(data.getValinnanVaiheOid());
		vaihe.setValinnanVaiheJarjestysluku(data.getValinnanVaiheJarjestysNro());

		haeTaiLuoValintakoe(vaihe, data);
	}

	protected void haeTaiLuoValintakoe(ValintakoeValinnanvaihe valinnanVaihe,
			HakukohdeValintakoeData data) {
		Valintakoe koe = null;
		for (Valintakoe k : valinnanVaihe.getValintakokeet()) {
			if (data.getValintakoeTunniste().equals(k.getValintakoeTunniste())) {
				koe = k;
				break;
			}
		}

		if (koe == null) {
			koe = new Valintakoe();
			valinnanVaihe.getValintakokeet().add(koe);
		}

		koe.setOsallistuminenTulos(data.getOsallistuminenTulos());
		koe.setValintakoeOid(data.getValintakoeOid());
		koe.setNimi(data.getNimi());
		koe.setValintakoeTunniste(data.getValintakoeTunniste());
	}

	protected Map<String, HakukohdeTyyppi> luoHakutoiveMap(
			List<HakukohdeTyyppi> hakutoiveet) {
		Map<String, HakukohdeTyyppi> toiveetMap = new HashMap<String, HakukohdeTyyppi>();
		for (HakukohdeTyyppi hk : hakutoiveet) {
			toiveetMap.put(hk.getHakukohdeOid(), hk);
		}

		return toiveetMap;
	}
}