package fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl;

import java.util.*;
import java.util.stream.Collectors;

import fi.vm.sade.service.valintaperusteet.dto.*;
import fi.vm.sade.service.valintaperusteet.dto.model.ValinnanVaiheTyyppi;
import fi.vm.sade.service.valintaperusteet.laskenta.Totuusarvofunktio;
import fi.vm.sade.valintalaskenta.domain.dto.*;
import fi.vm.sade.valintalaskenta.domain.valinta.*;
import fi.vm.sade.valintalaskenta.laskenta.dao.HakijaryhmaDAO;
import fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion.HakemusDTOToHakemusConverter;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import fi.vm.sade.kaava.Laskentadomainkonvertteri;
import fi.vm.sade.service.valintaperusteet.dto.model.Funktiotyyppi;
import fi.vm.sade.service.valintaperusteet.laskenta.Lukuarvofunktio;
import fi.vm.sade.service.valintaperusteet.laskenta.api.FunktioTulos;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakemus;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakukohde;
import fi.vm.sade.service.valintaperusteet.laskenta.api.SyotettyArvo;
import fi.vm.sade.service.valintaperusteet.model.Funktiokutsu;
import fi.vm.sade.service.valintaperusteet.service.validointi.virhe.LaskentakaavaEiOleValidiException;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.Tasasijasaanto;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.laskenta.dao.JarjestyskriteerihistoriaDAO;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValinnanvaiheDAO;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValintakoeOsallistuminenDAO;
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
    private HakemusDTOToHakemusConverter hakemusConverter;

	@Autowired
	private ValinnanvaiheDAO valinnanvaiheDAO;

    @Autowired
    private HakijaryhmaDAO hakijaryhmaDAO;

	@Autowired
	private JarjestyskriteerihistoriaDAO jarjestyskriteerihistoriaDAO;

	@Autowired
	private HakemuslaskinService hakemuslaskinService;

	@Autowired
	private ValintakoeOsallistuminenDAO valintakoeOsallistuminenDAO;

    @Autowired
    private ValintalaskentaModelMapper modelMapper;

    @Override
    public void suoritaLaskenta(List<HakemusDTO> kaikkiHakemukset,
                                List<ValintaperusteetDTO> valintaperusteet,
                                List<ValintaperusteetHakijaryhmaDTO> hakijaryhmat,
                                String hakukohdeOid) {

        Map<String, Hakemukset> hakemuksetHakukohteittain = jarjestaHakemuksetHakukohteittain(kaikkiHakemukset);

        // Järjestetään valintaperusteet valinnan vaiheiden järjestysnumeron
        // mukaan
        Collections.sort(valintaperusteet,
                (o1, o2) -> o1.getValinnanVaihe()
                        .getValinnanVaiheJarjestysluku()
                        - o2.getValinnanVaihe()
                        .getValinnanVaiheJarjestysluku());

        for (ValintaperusteetDTO vp : valintaperusteet) {
            String hakuOid = vp.getHakuOid();
            String tarjoajaOid = vp.getTarjoajaOid();

            if (!hakemuksetHakukohteittain.containsKey(hakukohdeOid)) {
                LOG.error(
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
                    || (vp.getValinnanVaihe().getValinnanVaiheTyyppi().equals(ValinnanVaiheTyyppi.VALINTAKOE))) {
                continue;
            }

            Map<String, String> hakukohteenValintaperusteet = muodostaHakukohteenValintaperusteetMap(vp
                    .getHakukohteenValintaperuste());

            ValintaperusteetValinnanVaiheDTO vaihe = vp
                    .getValinnanVaihe();

            final String valinnanvaiheOid = vaihe.getValinnanVaiheOid();
            final int jarjestysnumero = vaihe.getValinnanVaiheJarjestysluku();

            LOG.info(
                    "Haku {}, hakukohde {}, valinnanvaihe {} - jarjestysnumero {}",
                    hakuOid, hakukohdeOid, valinnanvaiheOid,
                    jarjestysnumero);
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

            final Valinnanvaihe viimeisinVaihe;
            if (jarjestysnumero > 0) {
                if (edellinenVaihe != null
                        && edellinenVaihe.getJarjestysnumero() == jarjestysnumero - 1) {
                    viimeisinVaihe = edellinenVaihe;
                } else {
                    viimeisinVaihe = valinnanvaiheDAO
                            .haeViimeisinValinnanvaihe(hakuOid, hakukohdeOid,
                                    jarjestysnumero);
                }
            } else {
                viimeisinVaihe = null;
            }

            Valinnanvaihe valinnanvaihe = haeTaiLuoValinnanvaihe(valinnanvaiheOid, hakuOid, hakukohdeOid, jarjestysnumero);
            valinnanvaihe.setHakukohdeOid(hakukohdeOid);
            valinnanvaihe.setHakuOid(hakuOid);
            valinnanvaihe.setJarjestysnumero(jarjestysnumero);
            valinnanvaihe.setValinnanvaiheOid(valinnanvaiheOid);
            valinnanvaihe.setTarjoajaOid(tarjoajaOid);
            valinnanvaihe.setNimi(vp.getValinnanVaihe().getNimi());

            for (ValintatapajonoJarjestyskriteereillaDTO j : vaihe
                    .getValintatapajono()) {
                Valintatapajono jono = new Valintatapajono();
                jono.setAloituspaikat(j.getAloituspaikat());
                jono.setEiVarasijatayttoa(j.getEiVarasijatayttoa());
                jono.setNimi(j.getNimi());
                jono.setPrioriteetti(j.getPrioriteetti());
                jono.setSiirretaanSijoitteluun(j.getSiirretaanSijoitteluun());
                jono.setKaikkiEhdonTayttavatHyvaksytaan(j.getKaikkiEhdonTayttavatHyvaksytaan());
                jono.setTasasijasaanto(Tasasijasaanto.valueOf(j.getTasasijasaanto()));
                jono.setValintatapajonoOid(j.getOid());
                jono.setValmisSijoiteltavaksi(j.getValmisSijoiteltavaksi());

                Map<String, JonosijaJaSyotetytArvot> jonosijatHakemusOidinMukaan = new HashMap<String, JonosijaJaSyotetytArvot>();
                for (ValintaperusteetJarjestyskriteeriDTO jk : j.getJarjestyskriteerit()) {
                    try {
                        Funktiokutsu funktiokutsu = modelMapper.map(jk.getFunktiokutsu(), Funktiokutsu.class);

                        Optional<Lukuarvofunktio> lukuarvofunktio = Optional.empty();
                        Optional<Totuusarvofunktio> totuusarvofunktio = Optional.empty();

                        if(Funktiotyyppi.LUKUARVOFUNKTIO.equals(funktiokutsu
                                .getFunktionimi().getTyyppi())) {
                            lukuarvofunktio = Optional.ofNullable(Laskentadomainkonvertteri
                                    .muodostaLukuarvolasku(funktiokutsu));
                        } else {
                            totuusarvofunktio = Optional.ofNullable(Laskentadomainkonvertteri
                                    .muodostaTotuusarvolasku(funktiokutsu));
                        }

                        for (HakemusWrapper hw : hakemukset) {
                            LOG.debug("hakemus {}", new Object[] { hw
                                    .getHakemusDTO().getHakemusoid() });

                            if(lukuarvofunktio.isPresent()) {
                                hakemuslaskinService.suoritaLaskentaHakemukselle(
                                        new Hakukohde(hakukohdeOid,
                                                hakukohteenValintaperusteet), hw,
                                        laskentahakemukset, lukuarvofunktio.get(), jk
                                        .getPrioriteetti(), viimeisinVaihe,
                                        jonosijatHakemusOidinMukaan, jk.getNimi(), jarjestysnumero);
                            } else {
                                hakemuslaskinService.suoritaLaskentaHakemukselle(
                                        new Hakukohde(hakukohdeOid,
                                                hakukohteenValintaperusteet), hw,
                                        laskentahakemukset, totuusarvofunktio.get(), jk
                                        .getPrioriteetti(), viimeisinVaihe,
                                        jonosijatHakemusOidinMukaan, jk.getNimi(),jarjestysnumero);
                            }


                        }
                    } catch (LaskentakaavaEiOleValidiException e) {
                        LOG.error(
                                "Hakukohteen {} Valintatapajonon {} prioriteetilla {} olevan järjestyskriteerin "
                                        + "funktiokutsu ei ole validi. Laskentaa ei voida suorittaa.",
                                hakukohdeOid, j.getOid(), jk.getPrioriteetti());
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

        poistaHaamuryhmat(hakijaryhmat, valintaperusteet.get(0).getHakukohdeOid());

        LOG.error("Hakijaryhmien määrä {}", hakijaryhmat.size());
        // Hakijaryhmät
        if(!hakijaryhmat.isEmpty()) {
//            Collections.sort(hakijaryhmat, (h1, h2) -> h1.getPrioriteetti() - h2.getPrioriteetti());
            hakijaryhmat.parallelStream().forEach(h -> {
                if (!hakemuksetHakukohteittain.containsKey(hakukohdeOid)) {
                    LOG.info(
                            "Hakukohteelle {} ei ole yhtään hakemusta. Hypätään yli.",
                            hakukohdeOid);
                    return;
                }

                List<HakemusWrapper> hakemukset = hakemuksetHakukohteittain.get(
                        hakukohdeOid).getHakemukset();
                List<Hakemus> laskentahakemukset = hakemuksetHakukohteittain.get(
                        hakukohdeOid).getLaskentahakemukset();
                if (hakemukset == null
                        || hakemukset.isEmpty()) {
                    return;
                }

                Hakijaryhma hakijaryhma = haeTaiLuoHakijaryhma(h);

                Map<String, JonosijaJaSyotetytArvot> jonosijatHakemusOidinMukaan = new HashMap<String, JonosijaJaSyotetytArvot>();
                try {
                    Funktiokutsu funktiokutsu = modelMapper.map(h.getFunktiokutsu(), Funktiokutsu.class);

                    Optional<Lukuarvofunktio> lukuarvofunktio = Optional.empty();
                    Optional<Totuusarvofunktio> totuusarvofunktio = Optional.empty();

                    if(Funktiotyyppi.LUKUARVOFUNKTIO.equals(funktiokutsu
                            .getFunktionimi().getTyyppi())) {
                        lukuarvofunktio = Optional.ofNullable(Laskentadomainkonvertteri
                                .muodostaLukuarvolasku(funktiokutsu));
                    } else {
                        totuusarvofunktio = Optional.ofNullable(Laskentadomainkonvertteri
                                .muodostaTotuusarvolasku(funktiokutsu));
                    }

                    Map<String, String> hakukohteenValintaperusteet = muodostaHakukohteenValintaperusteetMap(
                            valintaperusteet.get(0).getHakukohteenValintaperuste());

                    for (HakemusWrapper hw : hakemukset) {
                        LOG.debug("hakemus {}", new Object[] { hw
                                .getHakemusDTO().getHakemusoid() });

                        if(lukuarvofunktio.isPresent()) {
                            hakemuslaskinService.suoritaHakijaryhmaLaskentaHakemukselle(
                                    new Hakukohde(hakukohdeOid,
                                            hakukohteenValintaperusteet), hw,
                                    laskentahakemukset, lukuarvofunktio.get(),
                                    jonosijatHakemusOidinMukaan);
                        } else {
                            hakemuslaskinService.suoritaHakijaryhmaLaskentaHakemukselle(
                                    new Hakukohde(hakukohdeOid,
                                            hakukohteenValintaperusteet), hw,
                                    laskentahakemukset, totuusarvofunktio.get(),
                                    jonosijatHakemusOidinMukaan);
                        }


                    }
                } catch (LaskentakaavaEiOleValidiException e) {
                    LOG.error(
                            "Hakukohteen {} Hakijaryhmän {} "
                                    + "funktiokutsu ei ole validi. Laskentaa ei voida suorittaa.",
                            hakukohdeOid, h.getOid());
                    return;
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
                    hakijaryhma.getJonosijat().add(jonosija);
                }
                LOG.info("persistoidaan hakijaryhmä {}", hakijaryhma.getHakijaryhmaOid());
                hakijaryhmaDAO.create(hakijaryhma);

            });
        }
    }

    private Map<String, String> muodostaHakukohteenValintaperusteetMap(
            List<HakukohteenValintaperusteDTO> hakukohteenValintaperuste) {
        Map<String, String> map = new HashMap<String, String>();

        LOG.debug("Hakukohteen valintaperusteet:");
        for (HakukohteenValintaperusteDTO vp : hakukohteenValintaperuste) {
            map.put(vp.getTunniste(), vp.getArvo());
        }

        return map;
    }

    private Map<String, Hakemukset> jarjestaHakemuksetHakukohteittain(
            List<HakemusDTO> hakemukset) {
        Map<String, Hakemukset> hakukohdeHakemukset = new HashMap<String, Hakemukset>();
        for (HakemusDTO hakemus : hakemukset) {
            for (HakukohdeDTO hakukohde : hakemus.getHakukohteet()) {
                String hakukohdeOid = hakukohde.getOid();
                if (!hakukohdeHakemukset.containsKey(hakukohdeOid)) {
                    hakukohdeHakemukset.put(hakukohdeOid, new Hakemukset());
                }
                HakemusWrapper h = new HakemusWrapper();
                h.setHakemusDTO(hakemus);



                h.setLaskentahakemus(hakemusConverter.convert(hakemus));

                for (HakukohdeDTO hakutoive : hakemus.getHakukohteet()) {
                    if (hakukohdeOid.equals(hakutoive.getOid())) {
                        h.setHakutoiveprioriteetti(hakutoive.getPrioriteetti());
                        break;
                    }
                }

                hakukohdeHakemukset.get(hakukohdeOid).getHakemukset().add(h);
                hakukohdeHakemukset.get(hakukohdeOid).getLaskentahakemukset()
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
                if(jono.getKaytetaanValintalaskentaa() == null || jono.getKaytetaanValintalaskentaa()) {
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

    private void poistaHaamuryhmat(List<ValintaperusteetHakijaryhmaDTO> hakijaryhmat, String hakukohdeOid) {
        List<String> oidit = hakijaryhmat.stream().map(ValintaperusteetHakijaryhmaDTO::getOid).collect(Collectors.toList());

        hakijaryhmaDAO.haeHakijaryhmat(hakukohdeOid).stream()
                .filter(h -> oidit.indexOf(h.getHakijaryhmaOid()) == -1).forEach(hakijaryhmaDAO::poistaHakijaryhma);
    }

    private Hakijaryhma haeTaiLuoHakijaryhma(ValintaperusteetHakijaryhmaDTO dto) {
        Hakijaryhma hakijaryhma = hakijaryhmaDAO.haeHakijaryhma(dto.getOid()).map(h -> h).orElse(new Hakijaryhma());
        hakijaryhma.setHakijaryhmaOid(dto.getOid());
        hakijaryhma.setHakukohdeOid(dto.getHakukohdeOid());
        hakijaryhma.setKaytaKaikki(dto.isKaytaKaikki());
        hakijaryhma.setKaytetaanRyhmaanKuuluvia(dto.isKaytetaanRyhmaanKuuluvia());
        hakijaryhma.setKiintio(dto.getKiintio());
        hakijaryhma.setKuvaus(dto.getKuvaus());
        hakijaryhma.setNimi(dto.getNimi());
        hakijaryhma.setPrioriteetti(dto.getPrioriteetti());
        hakijaryhma.setTarkkaKiintio(dto.isTarkkaKiintio());
        hakijaryhma.setValintatapajonoOid(dto.getValintatapajonoOid());

        // Poistetaan vanhat historiat
        for (Jonosija jonosija : hakijaryhma.getJonosijat()) {
            for (Jarjestyskriteeritulos tulos : jonosija
                    .getJarjestyskriteeritulokset()) {
                jarjestyskriteerihistoriaDAO
                        .delete(tulos.getHistoria());
            }
        }

        hakijaryhma.getJonosijat().clear();

        return hakijaryhma;

    }
}
