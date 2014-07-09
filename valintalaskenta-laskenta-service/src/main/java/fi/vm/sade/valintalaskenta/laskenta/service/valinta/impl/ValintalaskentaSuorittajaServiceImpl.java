package fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl;

import java.util.*;

import fi.vm.sade.service.valintaperusteet.dto.*;
import fi.vm.sade.service.valintaperusteet.dto.model.ValinnanVaiheTyyppi;
import fi.vm.sade.valintalaskenta.domain.dto.*;
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
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;
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
	private JarjestyskriteerihistoriaDAO jarjestyskriteerihistoriaDAO;

	@Autowired
	private HakemuslaskinService hakemuslaskinService;

	@Autowired
	private ValintakoeOsallistuminenDAO valintakoeOsallistuminenDAO;

    @Autowired
    private ValintalaskentaModelMapper modelMapper;

    @Override
    public void suoritaLaskentaRest(List<HakemusDTO> kaikkiHakemukset,
                                List<ValintaperusteetDTO> valintaperusteet) {

        Map<String, Hakemukset> hakemuksetHakukohteittain = jarjestaHakemuksetHakukohteittainRest(kaikkiHakemukset);

        // Järjestetään valintaperusteet valinnan vaiheiden järjestysnumeron
        // mukaan
        Collections.sort(valintaperusteet,
                new Comparator<ValintaperusteetDTO>() {
                    @Override
                    public int compare(ValintaperusteetDTO o1,
                                       ValintaperusteetDTO o2) {
                        return o1.getValinnanVaihe()
                                .getValinnanVaiheJarjestysluku()
                                - o2.getValinnanVaihe()
                                .getValinnanVaiheJarjestysluku();
                    }
                });

        for (ValintaperusteetDTO vp : valintaperusteet) {
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
                    || (vp.getValinnanVaihe().getValinnanVaiheTyyppi().equals(ValinnanVaiheTyyppi.VALINTAKOE))) {
                continue;
            }

            Map<String, String> hakukohteenValintaperusteet = muodostaHakukohteenValintaperusteetMapRest(vp
                    .getHakukohteenValintaperuste());

            ValintaperusteetValinnanVaiheDTO vaihe = vp
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

            for (ValintatapajonoJarjestyskriteereillaDTO j : vaihe
                    .getValintatapajono()) {
                Valintatapajono jono = new Valintatapajono();
                jono.setAloituspaikat(j.getAloituspaikat());
                jono.setEiVarasijatayttoa(j.getEiVarasijatayttoa());
                jono.setNimi(j.getNimi());
                jono.setPrioriteetti(j.getPrioriteetti());
                jono.setSiirretaanSijoitteluun(j.getSiirretaanSijoitteluun());
                jono.setTasasijasaanto(Tasasijasaanto.valueOf(j.getTasasijasaanto()));
                jono.setValintatapajonoOid(j.getOid());

                Map<String, JonosijaJaSyotetytArvot> jonosijatHakemusOidinMukaan = new HashMap<String, JonosijaJaSyotetytArvot>();
                for (ValintaperusteetJarjestyskriteeriDTO jk : j.getJarjestyskriteerit()) {
                    try {
                        Funktiokutsu funktiokutsu = modelMapper.map(jk.getFunktiokutsu(), Funktiokutsu.class);

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
                                    .getHakemusDTO().getHakemusoid() });

                            hakemuslaskinService.suoritaLaskentaHakemukselle(
                                    new Hakukohde(hakukohdeOid,
                                            hakukohteenValintaperusteet), hw,
                                    laskentahakemukset, lukuarvofunktio, jk
                                    .getPrioriteetti(), viimeisinVaihe,
                                    jonosijatHakemusOidinMukaan, jk.getNimi());
                        }
                    } catch (LaskentakaavaEiOleValidiException e) {
//                        e.printStackTrace();
                        LOG.error(
                                "Hakukohteen {} Valintatapajonon {} prioriteetilla {} olevan järjestyskriteerin "
                                        + "funktiokutsu ei ole validi. Laskentaa ei voida suorittaa.",
                                new Object[] {hakukohdeOid, j.getOid(), jk.getPrioriteetti()});
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

    private Map<String, String> muodostaHakukohteenValintaperusteetMapRest(
            List<HakukohteenValintaperusteDTO> hakukohteenValintaperuste) {
        Map<String, String> map = new HashMap<String, String>();

        LOG.debug("Hakukohteen valintaperusteet:");
        for (HakukohteenValintaperusteDTO vp : hakukohteenValintaperuste) {
            map.put(vp.getTunniste(), vp.getArvo());
        }

        return map;
    }

    private Map<String, Hakemukset> jarjestaHakemuksetHakukohteittainRest(
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
}
