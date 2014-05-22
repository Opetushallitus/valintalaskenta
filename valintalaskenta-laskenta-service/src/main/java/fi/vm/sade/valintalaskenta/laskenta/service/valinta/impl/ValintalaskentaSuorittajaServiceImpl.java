package fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl;

import java.util.*;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import fi.vm.sade.kaava.Laskentadomainkonvertteri;
import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.hakemus.schema.HakukohdeTyyppi;
import fi.vm.sade.service.valintaperusteet.dto.model.Funktiotyyppi;
import fi.vm.sade.service.valintaperusteet.laskenta.Lukuarvofunktio;
import fi.vm.sade.service.valintaperusteet.laskenta.api.FunktioTulos;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakemus;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakukohde;
import fi.vm.sade.service.valintaperusteet.laskenta.api.SyotettyArvo;
import fi.vm.sade.service.valintaperusteet.model.Funktiokutsu;
import fi.vm.sade.service.valintaperusteet.schema.HakukohteenValintaperusteTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.JarjestyskriteeriTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.TavallinenValinnanVaiheTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteetTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintatapajonoJarjestyskriteereillaTyyppi;
import fi.vm.sade.service.valintaperusteet.service.validointi.virhe.LaskentakaavaEiOleValidiException;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.Tasasijasaanto;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.laskenta.dao.JarjestyskriteerihistoriaDAO;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValinnanvaiheDAO;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValintakoeOsallistuminenDAO;
import fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion.FunktioKutsuTyyppiToFunktioKutsuConverter;
import fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion.HakemusTyyppiToHakemusConverter;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.HakemuslaskinService;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.ValintalaskentaSuorittajaService;

/**
 * @author Jussi Jartamo
 */
@Service
public class ValintalaskentaSuorittajaServiceImpl implements
		ValintalaskentaSuorittajaService {

	private static final Logger LOG = LoggerFactory
			.getLogger(ValintalaskentaSuorittajaServiceImpl.class);

	@Autowired
	private HakemusTyyppiToHakemusConverter hakemusConverter;

	@Autowired
	private FunktioKutsuTyyppiToFunktioKutsuConverter funktiokutsuConverter;

	@Autowired
	private ValinnanvaiheDAO valinnanvaiheDAO;

	@Autowired
	private JarjestyskriteerihistoriaDAO jarjestyskriteerihistoriaDAO;

	@Autowired
	private HakemuslaskinService hakemuslaskinService;

	@Autowired
	private ValintakoeOsallistuminenDAO valintakoeOsallistuminenDAO;

	@Override
	public void suoritaLaskenta(List<HakemusTyyppi> kaikkiHakemukset,
			List<ValintaperusteetTyyppi> valintaperusteet) {

		Map<String, Hakemukset> hakemuksetHakukohteittain = jarjestaHakemuksetHakukohteittain(kaikkiHakemukset);

		// Järjestetään valintaperusteet valinnan vaiheiden järjestysnumeron
		// mukaan
		Collections.sort(valintaperusteet,
				new Comparator<ValintaperusteetTyyppi>() {
					@Override
					public int compare(ValintaperusteetTyyppi o1,
							ValintaperusteetTyyppi o2) {
						return o1.getValinnanVaihe()
								.getValinnanVaiheJarjestysluku()
								- o2.getValinnanVaihe()
										.getValinnanVaiheJarjestysluku();
					}
				});

		for (ValintaperusteetTyyppi vp : valintaperusteet) {
			String hakuOid = vp.getHakuOid();
			String hakukohdeOid = vp.getHakukohdeOid();
			String tarjoajaOid = vp.getTarjoajaOid();

			if (!hakemuksetHakukohteittain.containsKey(hakukohdeOid)) {
				LOG.info(
						"Hakukohteelle {} ei ole yhtään hakemusta. Hypätään yli.",
						hakukohdeOid);
				continue;
			}

			List<HakemusWrapper> hakemukset = hakemuksetHakukohteittain.get(
					hakukohdeOid).getHakemukset();
			List<Hakemus> laskentahakemukset = hakemuksetHakukohteittain.get(
					hakukohdeOid).getLaskentahakemukset();
			if (hakemukset == null
					|| hakemukset.isEmpty()
					|| !(vp.getValinnanVaihe() instanceof TavallinenValinnanVaiheTyyppi)) {
				continue;
			}

			Map<String, String> hakukohteenValintaperusteet = muodostaHakukohteenValintaperusteetMap(vp
					.getHakukohteenValintaperuste());

			TavallinenValinnanVaiheTyyppi vaihe = (TavallinenValinnanVaiheTyyppi) vp
					.getValinnanVaihe();

			final String valinnanvaiheOid = vaihe.getValinnanVaiheOid();
			final int jarjestysnumero = vaihe.getValinnanVaiheJarjestysluku();

			LOG.info(
					"Haku {}, hakukohde {}, valinnanvaihe {} - jarjestysnumero {}",
					new Object[] { hakuOid, hakukohdeOid, valinnanvaiheOid,
							jarjestysnumero });
			Valinnanvaihe edellinenVaihe = valinnanvaiheDAO
					.haeEdeltavaValinnanvaihe(hakuOid, hakukohdeOid,
							jarjestysnumero);

			// jos edellinenVaihe == null ja järjestysluku > 0 tarkistetaaan
			// löytyykö edellistä valintakoevaihetta vai heitetäänö virhe
			if (edellinenVaihe == null && jarjestysnumero > 0) {
				ValintakoeOsallistuminen edellinenOsallistuminen = valintakoeOsallistuminenDAO
						.haeEdeltavaValinnanvaihe(hakuOid, hakukohdeOid,
								jarjestysnumero);
				if (edellinenOsallistuminen == null) {
					LOG.warn("Valinnanvaiheen järjestysnumero on suurempi kuin 0, mutta edellistä valinnanvaihetta ei löytynyt");
					continue;
				}
			}

			Valinnanvaihe viimeisinVaihe = null;
			if (jarjestysnumero > 0) {
				if (edellinenVaihe != null
						&& edellinenVaihe.getJarjestysnumero() == jarjestysnumero - 1) {
					viimeisinVaihe = edellinenVaihe;
				} else {
					viimeisinVaihe = valinnanvaiheDAO
							.haeViimeisinValinnanvaihe(hakuOid, hakukohdeOid,
									jarjestysnumero);
				}
			}

			Valinnanvaihe valinnanvaihe = haeTaiLuoValinnanvaihe(valinnanvaiheOid, hakuOid, hakukohdeOid, jarjestysnumero);
			valinnanvaihe.setHakukohdeOid(hakukohdeOid);
			valinnanvaihe.setHakuOid(hakuOid);
			valinnanvaihe.setJarjestysnumero(jarjestysnumero);
			valinnanvaihe.setValinnanvaiheOid(valinnanvaiheOid);
			valinnanvaihe.setTarjoajaOid(tarjoajaOid);
			valinnanvaihe.setNimi(vp.getValinnanVaihe().getNimi());

			for (ValintatapajonoJarjestyskriteereillaTyyppi j : vaihe
					.getValintatapajono()) {
				Valintatapajono jono = new Valintatapajono();
				jono.setAloituspaikat(j.getAloituspaikat());
				jono.setEiVarasijatayttoa(j.isEiVarasijatayttoa());
                jono.setKaikkiEhdonTayttavatHyvaksytaan(j.isKaikkiEhdonTayttavatHyvaksytaan());
                jono.setPoissaOlevaTaytto(j.isPoissaOlevaTaytto());
                jono.setKaytetaanValintalaskentaa(j.isKaytetaanValintalaskentaa());
				jono.setNimi(j.getNimi());
				jono.setPrioriteetti(j.getPrioriteetti());
				jono.setSiirretaanSijoitteluun(j.isSiirretaanSijoitteluun());
				jono.setTasasijasaanto(Tasasijasaanto.valueOf(j
						.getTasasijasaanto().name()));
				jono.setValintatapajonoOid(j.getOid());

				Map<String, JonosijaJaSyotetytArvot> jonosijatHakemusOidinMukaan = new HashMap<String, JonosijaJaSyotetytArvot>();
				for (JarjestyskriteeriTyyppi jk : j.getJarjestyskriteerit()) {
					try {
						Funktiokutsu funktiokutsu = funktiokutsuConverter
								.convert(jk.getFunktiokutsu());
						if (!Funktiotyyppi.LUKUARVOFUNKTIO.equals(funktiokutsu
								.getFunktionimi().getTyyppi())) {
							LOG.error(
									"Valintatapajonon {} prioriteetilla {} olevan järjestyskriteerin laskentakaava "
											+ "ei ole tyypiltään "
											+ Funktiotyyppi.LUKUARVOFUNKTIO
													.name()
											+ ". Laskentaa ei "
											+ "voida suorittaa.", new Object[] {
											j.getOid(), jk.getPrioriteetti() });
							continue;
						}

						Lukuarvofunktio lukuarvofunktio = Laskentadomainkonvertteri
								.muodostaLukuarvolasku(funktiokutsu);
						for (HakemusWrapper hw : hakemukset) {
							LOG.debug("hakemus {}", new Object[] { hw
									.getHakemusTyyppi().getHakemusOid() });

							hakemuslaskinService.suoritaLaskentaHakemukselle(
									new Hakukohde(hakukohdeOid,
											hakukohteenValintaperusteet), hw,
									laskentahakemukset, lukuarvofunktio, jk
											.getPrioriteetti(), viimeisinVaihe,
									jonosijatHakemusOidinMukaan, jk.getNimi());
						}
					} catch (LaskentakaavaEiOleValidiException e) {
						LOG.error(
								"Valintatapajonon {} prioriteetilla {} olevan järjestyskriteerin "
										+ "laskentakaava ei ole validi. Laskentaa ei voida suorittaa.",
								new Object[] { j.getOid(), jk.getPrioriteetti() });
						continue;
					}
				}

				for (JonosijaJaSyotetytArvot js : jonosijatHakemusOidinMukaan
						.values()) {
					Jonosija jonosija = js.getJonosija();
					for (SyotettyArvo a : js.getSyotetytArvot().values()) {
						fi.vm.sade.valintalaskenta.domain.valinta.SyotettyArvo syotettyArvo = new fi.vm.sade.valintalaskenta.domain.valinta.SyotettyArvo();
						syotettyArvo.setArvo(a.getArvo());
						syotettyArvo.setLaskennallinenArvo(a
								.getLaskennallinenArvo());
						syotettyArvo.setOsallistuminen(a.getOsallistuminen()
								.name());
						syotettyArvo.setTunniste(a.getTunniste());
						jonosija.getSyotetytArvot().add(syotettyArvo);
					}
					for (FunktioTulos a : js.getFunktioTulokset().values()) {
						fi.vm.sade.valintalaskenta.domain.valinta.FunktioTulos funktioTulos = new fi.vm.sade.valintalaskenta.domain.valinta.FunktioTulos();
						funktioTulos.setArvo(a.getArvo());
						funktioTulos.setTunniste(a.getTunniste());
						funktioTulos.setNimiFi(a.getNimiFi());
						funktioTulos.setNimiSv(a.getNimiSv());
						funktioTulos.setNimiEn(a.getNimiEn());
						jonosija.getFunktioTulokset().add(funktioTulos);
					}
					jono.getJonosijat().add(jonosija);
				}

				valinnanvaihe.getValintatapajonot().add(jono);
			}

			valinnanvaiheDAO.create(valinnanvaihe);
		}
	}

	private Map<String, String> muodostaHakukohteenValintaperusteetMap(
			List<HakukohteenValintaperusteTyyppi> hakukohteenValintaperuste) {
		Map<String, String> map = new HashMap<String, String>();

		LOG.debug("Hakukohteen valintaperusteet:");
		for (HakukohteenValintaperusteTyyppi vp : hakukohteenValintaperuste) {
			map.put(vp.getTunniste(), vp.getArvo());
		}

		return map;
	}

	private Map<String, Hakemukset> jarjestaHakemuksetHakukohteittain(
			List<HakemusTyyppi> hakemukset) {
		Map<String, Hakemukset> hakukohdeHakemukset = new HashMap<String, Hakemukset>();
		for (HakemusTyyppi hakemus : hakemukset) {
			for (HakukohdeTyyppi hakukohde : hakemus.getHakutoive()) {
				String hakukohdeoid = hakukohde.getHakukohdeOid();
				if (!hakukohdeHakemukset.containsKey(hakukohdeoid)) {
					hakukohdeHakemukset.put(hakukohdeoid, new Hakemukset());
				}
				HakemusWrapper h = new HakemusWrapper();
				h.setHakemusTyyppi(hakemus);
				h.setLaskentahakemus(hakemusConverter.convert(hakemus));

				for (HakukohdeTyyppi hakutoive : hakemus.getHakutoive()) {
					if (hakukohdeoid.equals(hakutoive.getHakukohdeOid())) {
						h.setHakutoiveprioriteetti(hakutoive.getPrioriteetti());
						break;
					}
				}

				hakukohdeHakemukset.get(hakukohdeoid).getHakemukset().add(h);
				hakukohdeHakemukset.get(hakukohdeoid).getLaskentahakemukset()
						.add(h.getLaskentahakemus());
			}
		}
		return hakukohdeHakemukset;
	}

	private Valinnanvaihe haeTaiLuoValinnanvaihe(String valinnanvaiheOid, String hakuOid, String hakukohdeOid, int jarjestysnumero) {
		Valinnanvaihe valinnanvaihe = valinnanvaiheDAO
				.haeValinnanvaihe(valinnanvaiheOid);

        // Tarkistetaan ettei jää haamuvaiheita OVT-7668
        List<Valinnanvaihe> vaiheet = valinnanvaiheDAO.haeValinnanvaiheetJarjestysnumerolla(hakuOid, hakukohdeOid, jarjestysnumero);
        for (Valinnanvaihe vaihe : vaiheet) {
            if(!vaihe.getValinnanvaiheOid().equals(valinnanvaiheOid)) {
                valinnanvaiheDAO.poistaValinnanvaihe(vaihe);
            }
        }

		// Poistetaan vanhat historiat
		if (valinnanvaihe != null) {
            List<Valintatapajono> saastettavat = new ArrayList<Valintatapajono>();
			for (Valintatapajono jono : valinnanvaihe.getValintatapajonot()) {
                if(jono.getKaytetaanValintalaskentaa() || jono.getKaytetaanValintalaskentaa() == null) {
                    for (Jonosija jonosija : jono.getJonosijat()) {
                        for (Jarjestyskriteeritulos tulos : jonosija
                                .getJarjestyskriteeritulokset()) {
                            jarjestyskriteerihistoriaDAO
                                    .delete(tulos.getHistoria());
                        }
                    }
                } else {
                    saastettavat.add(jono);
                }
			}

			valinnanvaihe.getValintatapajonot().clear();
            valinnanvaihe.getValintatapajonot().addAll(saastettavat);
		} else {
			valinnanvaihe = new Valinnanvaihe();
		}

		return valinnanvaihe;
	}
}
