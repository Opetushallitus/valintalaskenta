package fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.impl;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import fi.vm.sade.service.valintaperusteet.dto.HakukohteenValintaperusteDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintakoeDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetValinnanVaiheDTO;
import fi.vm.sade.service.valintaperusteet.dto.model.Koekutsu;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakemus;
import fi.vm.sade.service.valintaperusteet.model.Funktiokutsu;
import fi.vm.sade.valintalaskenta.domain.dto.AvainArvoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakukohdeDTO;
import fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion.HakemusDTOToHakemusConverter;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakukohde;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Hyvaksyttavissatila;
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

    public static final String VALINNANVAIHE_HAKIJAN_VALINTA = "valinnanVaiheHakijanValinta";

    @Autowired
    private HakemusDTOToHakemusConverter hakemusConverter;

	@Autowired
	private ValintakoeOsallistuminenDAO valintakoeOsallistuminenDAO;

	@Autowired
	private Valintakoeosallistumislaskin valintakoeosallistumislaskin;

	@Autowired
	private ValinnanvaiheDAO valinnanvaiheDAO;

	@Autowired
	private EdellinenValinnanvaiheKasittelija edellinenValinnanvaiheKasittelija;

    @Autowired
    private ValintalaskentaModelMapper modelMapper;

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
    public void laske(HakemusDTO hakemus, List<ValintaperusteetDTO> valintaperusteet) {
        LOG.info("Laskentaan valintakoeosallistumiset hakemukselle {}",
                hakemus.getHakemusoid());

        final Map<String, HakukohdeDTO> hakutoiveetByOid = luoHakutoiveMap(hakemus
                .getHakukohteet());
        Map<String, List<HakukohdeValintakoeData>> valintakoeData = new HashMap<String, List<HakukohdeValintakoeData>>();

        for (ValintaperusteetDTO vp : valintaperusteet) {
            Map<String, String> hakukohteenValintaperusteet = muodostaHakukohteenValintaperusteetMap(vp
                    .getHakukohteenValintaperuste());

            if (hakutoiveetByOid.containsKey(vp.getHakukohdeOid())
                    && !vp.getValinnanVaihe().getValintakoe().isEmpty()) {
                ValintaperusteetValinnanVaiheDTO vaihe = vp
                        .getValinnanVaihe();

                for (ValintakoeDTO koe : vaihe.getValintakoe()) {
                    if(koe.getAktiivinen()) {
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

                        OsallistuminenTulos osallistuminen = new OsallistuminenTulos();
                        Hakemus hak = hakemusConverter.convert(hakemus);
                        Funktiokutsu fuk = modelMapper.map(koe.getFunktiokutsu(), Funktiokutsu.class);

                        if (viimeisinValinnanVaihe != null) {
                            TilaJaSelite tilaJaSelite = edellinenValinnanvaiheKasittelija.tilaEdellisenValinnanvaiheenMukaan(
                                    hakemus.getHakemusoid(), new Hyvaksyttavissatila(), viimeisinValinnanVaihe);
                            if (tilaJaSelite.getTila().equals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA)) {
                                if (koe.getKutsunKohde().equals(Koekutsu.HAKIJAN_VALINTA)) {
                                    osallistuminen = createOsallistuminen(tilaJaSelite, Osallistuminen.OSALLISTUU);
                                } else {
                                    osallistuminen = getOsallistuminenTulos(new Hakukohde(vp.getHakukohdeOid(), hakukohteenValintaperusteet), hak, fuk);
                                }
                            } else {
                                osallistuminen = createOsallistuminen(tilaJaSelite, Osallistuminen.EI_OSALLISTU);
                            }
                        } else {
                            if (koe.getKutsunKohde().equals(Koekutsu.HAKIJAN_VALINTA)) {
                                osallistuminen.setOsallistuminen(Osallistuminen.OSALLISTUU);
                            } else {
                                osallistuminen = getOsallistuminenTulos(new Hakukohde(vp.getHakukohdeOid(), hakukohteenValintaperusteet), hak, fuk);
                            }
                        }

                        HakukohdeValintakoeData data = new HakukohdeValintakoeData();
                        data.setHakuOid(vp.getHakuOid());
                        if (koe.getKutsunKohde().equals(Koekutsu.HAKIJAN_VALINTA)) {
                            data.setValinnanVaiheOid(VALINNANVAIHE_HAKIJAN_VALINTA);
                            data.setValinnanVaiheJarjestysNro(100);
                            final Optional<AvainArvoDTO> avainArvo = hakemus.getAvaimet().stream().filter(avainArvoDTO ->
                                    avainArvoDTO.getAvain().equals(koe.getKutsunKohdeAvain())).findFirst();
                            if (avainArvo.isPresent()) {
                                data.setHakukohdeOid(avainArvo.get().getArvo());
                            } else {
                                LOG.error("Hakemukselta {} puuttuu kutsun kohde {}", hakemus.getHakemusoid(), koe.getKutsunKohdeAvain());
                                throw new RuntimeException("Hakemukselta ei löytynyt kutsun kohdetta!");
                            }
                        } else {
                            data.setHakukohdeOid(vp.getHakukohdeOid());
                            data.setLaskettavaHakukohdeOid(vp.getHakukohdeOid());
                            data.setValinnanVaiheJarjestysNro(vaihe.getValinnanVaiheJarjestysluku());
                            data.setValinnanVaiheOid(vaihe.getValinnanVaiheOid());
                        }
                        data.setOsallistuminenTulos(osallistuminen);
                        data.setValintakoeOid(koe.getOid());
                        data.setValintakoeTunniste(tunniste);
                        data.setNimi(koe.getNimi());
                        data.setLahetetaankoKoekutsut(koe.getLahetetaankoKoekutsut());
                        data.setAktiivinen(koe.getAktiivinen());
                        data.setKutsunKohde(koe.getKutsunKohde());
                        data.setKutsunKohdeAvain(koe.getKutsunKohdeAvain());

                        if (!valintakoeData.containsKey(tunniste)) {
                            valintakoeData.put(tunniste,
                                    new ArrayList<>());
                        }

                        valintakoeData.get(tunniste).add(data);
                    }
                }
            }
        }

        Map<String, ValintakoeOsallistuminen> osallistumisetByHaku = new HashMap<String, ValintakoeOsallistuminen>();
        for (Map.Entry<String, List<HakukohdeValintakoeData>> entry : valintakoeData
                .entrySet()) {
            List<HakukohdeValintakoeData> kokeet = entry.getValue();
            List<HakukohdeValintakoeData> olemassaOlevat = new ArrayList<>();

//            asetaOsallistumisetKokeisiin(kokeet, hakutoiveetByOid);
            for (HakukohdeValintakoeData c : kokeet) {
                LOG.info(
                        "Hakukohde: {}, valintakoe: {}",
                        new Object[] { c.getHakukohdeOid(),
                                c.getValintakoeOid() });

                if (!osallistumisetByHaku.containsKey(c.getHakuOid())) {
                    osallistumisetByHaku.put(c.getHakuOid(),
                            luoValintakoeOsallistuminen(c, hakemus, hakutoiveetByOid));
                }

//                ValintakoeOsallistuminen osallistuminen = osallistumisetByHaku
//                        .get(c.getHakuOid());
//
//                haeTaiLuoHakutoive(osallistuminen, c);
            }

            for(HakukohdeValintakoeData c : kokeet) {
                ValintakoeOsallistuminen osallistuminen = osallistumisetByHaku
                        .get(c.getHakuOid());

                osallistuminen.getHakutoiveet().forEach(h -> h.getValinnanVaiheet().forEach(v -> v.getValintakokeet().forEach(koe -> {
                    if(koe.getValintakoeTunniste().equals(c.getValintakoeTunniste()) && !koe.getValintakoeOid().equals(c.getValintakoeOid())) {
                        HakukohdeValintakoeData data = new HakukohdeValintakoeData();
                        data.setHakuOid(c.getHakuOid());
                        data.setHakukohdeOid(h.getHakukohdeOid());
                        data.setOsallistuminenTulos(koe.getOsallistuminenTulos());
                        data.setValinnanVaiheJarjestysNro(v
                                .getValinnanVaiheJarjestysluku());
                        data.setValinnanVaiheOid(v.getValinnanVaiheOid());
                        data.setValintakoeOid(koe.getValintakoeOid());
                        data.setValintakoeTunniste(koe.getValintakoeTunniste());
                        data.setNimi(koe.getNimi());
                        data.setLahetetaankoKoekutsut(koe.isLahetetaankoKoekutsut());
                        data.setAktiivinen(koe.isAktiivinen());
                        data.setKutsunKohde(koe.getKutsunKohde());
                        data.setKutsunKohdeAvain(koe.getKutsunKohdeAvain());
                        olemassaOlevat.add(data);
                    }
                })));
                olemassaOlevat.add(c);
            }

            asetaOsallistumisetKokeisiin(olemassaOlevat, hakutoiveetByOid);

            for (HakukohdeValintakoeData c : olemassaOlevat) {

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

    private OsallistuminenTulos getOsallistuminenTulos(Hakukohde hakukohde, Hakemus convert, Funktiokutsu map) {
        return valintakoeosallistumislaskin
                .laskeOsallistuminenYhdelleHakukohteelle(
                        hakukohde,
                        convert, map);
    }

    private OsallistuminenTulos createOsallistuminen(TilaJaSelite tilaJaSelite, Osallistuminen osallistuu) {
        OsallistuminenTulos osallistuminen = new OsallistuminenTulos();
        osallistuminen.setKuvaus(tilaJaSelite.getSelite());
        osallistuminen.setTekninenKuvaus(tilaJaSelite.getTekninenSelite());
        osallistuminen.setLaskentaTila(tilaJaSelite.getTila().toString());
        osallistuminen.setOsallistuminen(osallistuu);
        return osallistuminen;
    }

    private Map<String, String> muodostaHakukohteenValintaperusteetMap(
            List<HakukohteenValintaperusteDTO> hakukohteenValintaperuste) {
        Map<String, String> map = new HashMap<String, String>();

        for (HakukohteenValintaperusteDTO vp : hakukohteenValintaperuste) {
            map.put(vp.getTunniste(), vp.getArvo());
        }

        return map;
    }

    protected void asetaOsallistumisetKokeisiin(
            List<HakukohdeValintakoeData> kokeet,
            final Map<String, HakukohdeDTO> hakukohteetByOid) {

        // Käydään hakijan hakutoiveet läpi prioriteetin mukaan ja asetetaan
        // kullekin hakukohteelle
        // valintakoekohtainen osallistumistieto
        Collections.sort(kokeet, (o1, o2) ->
                hakukohteetByOid.getOrDefault(o1.getHakukohdeOid(), new HakukohdeDTO() {{setPrioriteetti(Integer.MAX_VALUE); }}).getPrioriteetti()
                - hakukohteetByOid.getOrDefault(o2.getHakukohdeOid(), new HakukohdeDTO() {{setPrioriteetti(Integer.MAX_VALUE); }}).getPrioriteetti());

        // Jos hakija osallistuu korkeamman prioriteetin hakuktoiveen
        // valintakokeeseen, hakija ei osallistu
        // pienemmällä prioriteetilla oleviin samoihin valintakokeisiin.
        boolean osallistuminenLoydetty = false;
        final List<HakukohdeValintakoeData> lasketutKokeet = kokeet.stream().filter(koe -> !koe.getKutsunKohde().equals(Koekutsu.HAKIJAN_VALINTA)).collect(Collectors.toList());
        for (HakukohdeValintakoeData d : lasketutKokeet) {

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
            HakukohdeValintakoeData data, HakemusDTO hakemus,
            Map<String, HakukohdeDTO> hakutoiveetByOid) {
        ValintakoeOsallistuminen osallistuminen = valintakoeOsallistuminenDAO
                .readByHakuOidAndHakemusOid(data.getHakuOid(),
                        hakemus.getHakemusoid());

        if (osallistuminen == null) {
            osallistuminen = new ValintakoeOsallistuminen();
        } else {

            List<Hakutoive> toiveet = osallistuminen.getHakutoiveet()
                    .stream()
                    .map(t -> {
                        if (hakutoiveetByOid.containsKey(t.getHakukohdeOid())) {
                            return t;
                        } else {
                            t.setValinnanVaiheet(getHakijanValintaVaiheet(t));
                            return t;
                        }
                    })
                    .filter(t -> !t.getHakukohdeOid().equals(data.getLaskettavaHakukohdeOid()))
                    .collect(Collectors.toList());

            final Optional<Hakutoive> hakutoive = osallistuminen.getHakutoiveet().stream()
                    .filter(t -> t.getHakukohdeOid().equals(data.getLaskettavaHakukohdeOid()))
                    .map(h -> {
                        h.setValinnanVaiheet(getHakijanValintaVaiheet(h));
                        return h;
                    }).findFirst();

            if (hakutoive.isPresent() && hakutoive.get().getValinnanVaiheet().size() > 0) {
                toiveet.add(hakutoive.get());
            }
            osallistuminen.getHakutoiveet().clear();
            osallistuminen.setHakutoiveet(toiveet);
        }

        osallistuminen.setHakuOid(data.getHakuOid());
        osallistuminen.setHakemusOid(hakemus.getHakemusoid());
        osallistuminen.setHakijaOid(hakemus.getHakijaOid());
        osallistuminen.setSukunimi(hakemus.getSukunimi());
        osallistuminen.setEtunimi(hakemus.getEtunimi());

        return osallistuminen;
    }

    private List<ValintakoeValinnanvaihe> getHakijanValintaVaiheet(Hakutoive t) {
        return t.getValinnanVaiheet().stream()
            .filter(v -> v.getValinnanVaiheOid().equals(VALINNANVAIHE_HAKIJAN_VALINTA))
            .collect(Collectors.toList());
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
		koe.setLahetetaankoKoekutsut(data.isLahetetaankoKoekutsut());
		koe.setAktiivinen(data.isAktiivinen());
        koe.setKutsunKohde(data.getKutsunKohde());
        koe.setKutsunKohdeAvain(data.getKutsunKohdeAvain());
	}

    protected Map<String, HakukohdeDTO> luoHakutoiveMap(
            List<HakukohdeDTO> hakutoiveet) {
        Map<String, HakukohdeDTO> toiveetMap = new HashMap<String, HakukohdeDTO>();
        for (HakukohdeDTO hk : hakutoiveet) {
            toiveetMap.put(hk.getOid(), hk);
        }

        return toiveetMap;
    }
}