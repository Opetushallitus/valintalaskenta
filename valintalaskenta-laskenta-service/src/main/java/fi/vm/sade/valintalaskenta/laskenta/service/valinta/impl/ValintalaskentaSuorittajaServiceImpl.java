package fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl;

import fi.vm.sade.kaava.Laskentadomainkonvertteri;
import fi.vm.sade.service.valintaperusteet.dto.HakukohteenValintaperusteDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetHakijaryhmaDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetJarjestyskriteeriDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetValinnanVaiheDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintatapajonoJarjestyskriteereillaDTO;
import fi.vm.sade.service.valintaperusteet.dto.model.Funktiotyyppi;
import fi.vm.sade.service.valintaperusteet.dto.model.ValinnanVaiheTyyppi;
import fi.vm.sade.service.valintaperusteet.laskenta.Lukuarvofunktio;
import fi.vm.sade.service.valintaperusteet.laskenta.Totuusarvofunktio;
import fi.vm.sade.service.valintaperusteet.laskenta.api.FunktioTulos;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakemus;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakukohde;
import fi.vm.sade.service.valintaperusteet.laskenta.api.SyotettyArvo;
import fi.vm.sade.service.valintaperusteet.model.Funktiokutsu;
import fi.vm.sade.service.valintaperusteet.service.validointi.virhe.LaskentakaavaEiOleValidiException;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakukohdeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.Tasasijasaanto;
import fi.vm.sade.valintalaskenta.domain.valinta.Hakijaryhma;
import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.laskenta.dao.HakijaryhmaDAO;
import fi.vm.sade.valintalaskenta.laskenta.dao.JarjestyskriteerihistoriaDAO;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValinnanvaiheDAO;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValintakoeOsallistuminenDAO;
import fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion.HakemusDTOToHakemusConverter;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.HakemuslaskinService;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.ValintalaskentaSuorittajaService;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class ValintalaskentaSuorittajaServiceImpl implements ValintalaskentaSuorittajaService {
    private static final Logger LOG = LoggerFactory.getLogger(ValintalaskentaSuorittajaServiceImpl.class);

    private final HakemusDTOToHakemusConverter hakemusConverter;
    private final ValinnanvaiheDAO valinnanvaiheDAO;
    private final HakijaryhmaDAO hakijaryhmaDAO;
    private final JarjestyskriteerihistoriaDAO jarjestyskriteerihistoriaDAO;
    private final HakemuslaskinService hakemuslaskinService;
    private final ValintakoeOsallistuminenDAO valintakoeOsallistuminenDAO;
    private final ValintalaskentaModelMapper modelMapper;
    private final EdellinenValinnanvaiheKasittelija edellinenValinnanvaiheKasittelija;

    @Autowired
    public ValintalaskentaSuorittajaServiceImpl(HakemusDTOToHakemusConverter hakemusConverter,
                                                ValinnanvaiheDAO valinnanvaiheDAO,
                                                HakijaryhmaDAO hakijaryhmaDAO,
                                                JarjestyskriteerihistoriaDAO jarjestyskriteerihistoriaDAO,
                                                HakemuslaskinService hakemuslaskinService,
                                                ValintakoeOsallistuminenDAO valintakoeOsallistuminenDAO,
                                                ValintalaskentaModelMapper modelMapper,
                                                EdellinenValinnanvaiheKasittelija edellinenValinnanvaiheKasittelija) {
        this.hakemusConverter = hakemusConverter;
        this.valinnanvaiheDAO = valinnanvaiheDAO;
        this.hakijaryhmaDAO = hakijaryhmaDAO;
        this.jarjestyskriteerihistoriaDAO = jarjestyskriteerihistoriaDAO;
        this.hakemuslaskinService = hakemuslaskinService;
        this.valintakoeOsallistuminenDAO = valintakoeOsallistuminenDAO;
        this.modelMapper = modelMapper;
        this.edellinenValinnanvaiheKasittelija = edellinenValinnanvaiheKasittelija;
    }

    @Override
    public void suoritaLaskenta(List<HakemusDTO> kaikkiHakemukset, List<ValintaperusteetDTO> valintaperusteet, List<ValintaperusteetHakijaryhmaDTO> hakijaryhmat, String hakukohdeOid, String uuid, boolean korkeakouluhaku) {
        Map<String, Hakemukset> hakemuksetHakukohteittain = jarjestaHakemuksetHakukohteittain(kaikkiHakemukset);
        jarjestaValinnanVaiheenJarjestysluvunMukaan(valintaperusteet);

        for (ValintaperusteetDTO vp : valintaperusteet) {
            if (!hakemuksetHakukohteittain.containsKey(hakukohdeOid)) {
                LOG.error("(Uuid={}) Hakukohteelle {} ei ole yhtään hakemusta. Hypätään yli.", uuid, hakukohdeOid);
                continue;
            }
            List<HakemusWrapper> hakemukset = hakemuksetHakukohteittain.get(hakukohdeOid).getHakemukset();
            List<Hakemus> laskentahakemukset = hakemuksetHakukohteittain.get(hakukohdeOid).getLaskentahakemukset();
            if (emptyHakemuksetOrValinnanVaiheTyyppiValintakoe(vp, hakemukset)) {
                continue;
            }
            Map<String, String> hakukohteenValintaperusteet = muodostaHakukohteenValintaperusteetMap(vp.getHakukohteenValintaperuste());
            ValintaperusteetValinnanVaiheDTO vaihe = vp.getValinnanVaihe();
            final String valinnanvaiheOid = vaihe.getValinnanVaiheOid();
            final int jarjestysnumero = vaihe.getValinnanVaiheJarjestysluku();
            final String hakuOid = vp.getHakuOid();

            LOG.info("(Uuid={}) Haku {}, hakukohde {}, valinnanvaihe {} - jarjestysnumero {}", uuid, hakuOid, hakukohdeOid, valinnanvaiheOid, jarjestysnumero);
            Valinnanvaihe edellinenVaihe = valinnanvaiheDAO.haeEdeltavaValinnanvaihe(hakuOid, hakukohdeOid, jarjestysnumero);
            if (invalidEdellinenVaihe(hakukohdeOid, uuid, jarjestysnumero, hakuOid, edellinenVaihe)) {
                continue;
            }
            final Valinnanvaihe viimeisinVaihe = getViimeisinValinnanVaihe(hakukohdeOid, jarjestysnumero, hakuOid, edellinenVaihe);
            Valinnanvaihe valinnanvaihe = haeTaiLuoValinnanvaihe(valinnanvaiheOid, hakuOid, hakukohdeOid, jarjestysnumero);
            valinnanvaihe.setHakukohdeOid(hakukohdeOid);
            valinnanvaihe.setHakuOid(hakuOid);
            valinnanvaihe.setJarjestysnumero(jarjestysnumero);
            valinnanvaihe.setValinnanvaiheOid(valinnanvaiheOid);
            valinnanvaihe.setTarjoajaOid(vp.getTarjoajaOid());
            valinnanvaihe.setNimi(vp.getValinnanVaihe().getNimi());

            ValintakoeOsallistuminen edellinenOsallistuminen = valintakoeOsallistuminenDAO.haeEdeltavaValinnanvaihe(hakuOid, hakukohdeOid, jarjestysnumero);

            laskeValintatapajonot(hakukohdeOid, uuid, hakemukset, laskentahakemukset, hakukohteenValintaperusteet, vaihe, jarjestysnumero, edellinenVaihe, viimeisinVaihe, valinnanvaihe, edellinenOsallistuminen, korkeakouluhaku);
            valinnanvaiheDAO.saveOrUpdate(valinnanvaihe);
        }

        poistaHaamuryhmat(hakijaryhmat, valintaperusteet.get(0).getHakukohdeOid());
        LOG.info("(Uuid={}) Hakijaryhmien määrä {} hakukohteessa {}", uuid, hakijaryhmat.size(), hakukohdeOid);
        laskeHakijaryhmat(valintaperusteet, hakijaryhmat, hakukohdeOid, uuid, hakemuksetHakukohteittain, korkeakouluhaku);
    }

    private boolean emptyHakemuksetOrValinnanVaiheTyyppiValintakoe(ValintaperusteetDTO vp, List<HakemusWrapper> hakemukset) {
        return hakemukset == null
                || hakemukset.isEmpty()
                || (vp.getValinnanVaihe().getValinnanVaiheTyyppi().equals(ValinnanVaiheTyyppi.VALINTAKOE));
    }

    private boolean invalidEdellinenVaihe(String hakukohdeOid, String uuid, int jarjestysnumero, String hakuOid, Valinnanvaihe edellinenVaihe) {
        if (edellinenVaihe == null && jarjestysnumero > 0) {
            ValintakoeOsallistuminen edellinenOsallistuminen = valintakoeOsallistuminenDAO.haeEdeltavaValinnanvaihe(hakuOid, hakukohdeOid, jarjestysnumero);
            if (edellinenOsallistuminen == null) {
                LOG.warn("(Uuid={}) Valinnanvaiheen järjestysnumero on suurempi kuin 0, mutta edellistä valinnanvaihetta ei löytynyt", uuid);
                return true;
            }
        }
        return false;
    }

    private Valinnanvaihe getViimeisinValinnanVaihe(String hakukohdeOid, int jarjestysnumero, String hakuOid, Valinnanvaihe edellinenVaihe) {
        final Valinnanvaihe viimeisinVaihe;
        if (jarjestysnumero > 0) {
            if (edellinenVaihe != null && edellinenVaihe.getJarjestysnumero() == jarjestysnumero - 1) {
                viimeisinVaihe = edellinenVaihe;
            } else {
                viimeisinVaihe = valinnanvaiheDAO.haeViimeisinValinnanvaihe(hakuOid, hakukohdeOid, jarjestysnumero);
            }
        } else {
            viimeisinVaihe = null;
        }
        return viimeisinVaihe;
    }

    private void laskeHakijaryhmat(List<ValintaperusteetDTO> valintaperusteet, List<ValintaperusteetHakijaryhmaDTO> hakijaryhmat, String hakukohdeOid, String uuid, Map<String, Hakemukset> hakemuksetHakukohteittain, boolean korkeakouluhaku) {
        if (!hakijaryhmat.isEmpty()) {
            hakijaryhmat.parallelStream().forEach(h -> {
                if (!hakemuksetHakukohteittain.containsKey(hakukohdeOid)) {
                    LOG.info("(Uuid={}) Hakukohteelle {} ei ole yhtään hakemusta. Hypätään yli.", uuid, hakukohdeOid);
                    return;
                }

                List<HakemusWrapper> hakemukset = hakemuksetHakukohteittain.get(hakukohdeOid).getHakemukset();
                List<Hakemus> laskentahakemukset = hakemuksetHakukohteittain.get(hakukohdeOid).getLaskentahakemukset();
                if (hakemukset == null || hakemukset.isEmpty()) {
                    return;
                }

                Hakijaryhma hakijaryhma = haeTaiLuoHakijaryhma(h);

                Map<String, JonosijaJaSyotetytArvot> jonosijatHakemusOidinMukaan = new HashMap<>();
                Map<String, String> hakukohteenValintaperusteet = muodostaHakukohteenValintaperusteetMap(valintaperusteet.get(0).getHakukohteenValintaperuste());

                Funktiokutsu funktiokutsu = modelMapper.map(h.getFunktiokutsu(), Funktiokutsu.class);
                Optional<Lukuarvofunktio> lukuarvofunktio = Optional.empty();
                Optional<Totuusarvofunktio> totuusarvofunktio = Optional.empty();
                try {
                    if (Funktiotyyppi.LUKUARVOFUNKTIO.equals(funktiokutsu.getFunktionimi().getTyyppi())) {
                        lukuarvofunktio = Optional.ofNullable(Laskentadomainkonvertteri.muodostaLukuarvolasku(funktiokutsu));
                    } else {
                        totuusarvofunktio = Optional.ofNullable(Laskentadomainkonvertteri.muodostaTotuusarvolasku(funktiokutsu));
                    }
                } catch (LaskentakaavaEiOleValidiException e) {
                    LOG.error("(Uuid={}) Hakukohteen {} Hakijaryhmän {} funktiokutsu ei ole validi. Laskentaa ei voida suorittaa.", uuid, hakukohdeOid, h.getOid());
                    return;
                }

                for (HakemusWrapper hw : hakemukset) {
                    LOG.debug("hakemus {}", new Object[]{hw.getHakemusDTO().getHakemusoid()});
                    if (lukuarvofunktio.isPresent()) {
                        hakemuslaskinService.suoritaHakijaryhmaLaskentaHakemukselle(
                                new Hakukohde(hakukohdeOid, hakukohteenValintaperusteet, korkeakouluhaku),
                                hw,
                                laskentahakemukset,
                                lukuarvofunktio.get(),
                                jonosijatHakemusOidinMukaan
                        );
                    } else {
                        hakemuslaskinService.suoritaHakijaryhmaLaskentaHakemukselle(
                                new Hakukohde(hakukohdeOid, hakukohteenValintaperusteet, korkeakouluhaku),
                                hw,
                                laskentahakemukset,
                                totuusarvofunktio.get(),
                                jonosijatHakemusOidinMukaan
                        );
                    }
                }

                for (JonosijaJaSyotetytArvot js : jonosijatHakemusOidinMukaan.values()) {
                    hakijaryhma.getJonosijat().add(createJonosija(js));
                }
                LOG.info("(Uuid={}) Persistoidaan hakijaryhmä {}", uuid, hakijaryhma.getHakijaryhmaOid());
                hakijaryhmaDAO.create(hakijaryhma);
            });
        }
    }

    private Jonosija createJonosija(JonosijaJaSyotetytArvot js) {
        Jonosija jonosija = js.getJonosija();
        for (SyotettyArvo a : js.getSyotetytArvot().values()) {
            fi.vm.sade.valintalaskenta.domain.valinta.SyotettyArvo syotettyArvo = new fi.vm.sade.valintalaskenta.domain.valinta.SyotettyArvo();
            syotettyArvo.setArvo(a.getArvo());
            syotettyArvo.setLaskennallinenArvo(a.getLaskennallinenArvo());
            syotettyArvo.setOsallistuminen(a.getOsallistuminen().name());
            syotettyArvo.setTunniste(a.getTunniste());
            syotettyArvo.setTyypinKoodiUri(a.getTyypinKoodiUri());
            syotettyArvo.setTilastoidaan(a.isTilastoidaan());
            jonosija.getSyotetytArvot().add(syotettyArvo);
        }
        for (FunktioTulos a : js.getFunktioTulokset().values()) {
            fi.vm.sade.valintalaskenta.domain.valinta.FunktioTulos funktioTulos = new fi.vm.sade.valintalaskenta.domain.valinta.FunktioTulos();
            funktioTulos.setArvo(a.getArvo());
            funktioTulos.setTunniste(a.getTunniste());
            funktioTulos.setNimiFi(a.getNimiFi());
            funktioTulos.setNimiSv(a.getNimiSv());
            funktioTulos.setNimiEn(a.getNimiEn());
            funktioTulos.setOmaopintopolku(a.isOmaopintopolku());
            jonosija.getFunktioTulokset().add(funktioTulos);
        }
        return jonosija;
    }

    private void laskeValintatapajonot(String hakukohdeOid, String uuid, List<HakemusWrapper> hakemukset, List<Hakemus> laskentahakemukset, Map<String, String> hakukohteenValintaperusteet, ValintaperusteetValinnanVaiheDTO vaihe, int jarjestysnumero, Valinnanvaihe edellinenVaihe, Valinnanvaihe viimeisinVaihe, Valinnanvaihe valinnanvaihe, ValintakoeOsallistuminen edellinenOsallistuminen, boolean korkeakouluhaku) {
        for (ValintatapajonoJarjestyskriteereillaDTO j : vaihe.getValintatapajono()) {
            if (j.getKaytetaanValintalaskentaa() == null || j.getKaytetaanValintalaskentaa()) {
                Valintatapajono jono = createValintatapajono(j);
                Map<String, JonosijaJaSyotetytArvot> jonosijatHakemusOidinMukaan = new HashMap<>();
                for (ValintaperusteetJarjestyskriteeriDTO jk : j.getJarjestyskriteerit()) {
                    Funktiokutsu funktiokutsu = modelMapper.map(jk.getFunktiokutsu(), Funktiokutsu.class);
                    Optional<Lukuarvofunktio> lukuarvofunktio = Optional.empty();
                    Optional<Totuusarvofunktio> totuusarvofunktio = Optional.empty();
                    try {
                        if (Funktiotyyppi.LUKUARVOFUNKTIO.equals(funktiokutsu.getFunktionimi().getTyyppi())) {
                            lukuarvofunktio = Optional.ofNullable(Laskentadomainkonvertteri.muodostaLukuarvolasku(funktiokutsu));
                        } else {
                            totuusarvofunktio = Optional.ofNullable(Laskentadomainkonvertteri.muodostaTotuusarvolasku(funktiokutsu));
                        }
                    } catch (LaskentakaavaEiOleValidiException e) {
                        LOG.error("(Uuid={}) Hakukohteen {} Valintatapajonon {} prioriteetilla {} olevan järjestyskriteerin funktiokutsu ei ole validi. Laskentaa ei voida suorittaa.", uuid, hakukohdeOid, j.getOid(), jk.getPrioriteetti());
                        continue;
                    }

                    for (HakemusWrapper hw : hakemukset) {
                        LOG.debug("hakemus {}", new Object[]{hw.getHakemusDTO().getHakemusoid()});

                        if (lukuarvofunktio.isPresent()) {
                            hakemuslaskinService.suoritaLaskentaHakemukselle(
                                    new Hakukohde(hakukohdeOid,hakukohteenValintaperusteet, korkeakouluhaku),
                                    hw,
                                    laskentahakemukset,
                                    lukuarvofunktio.get(),
                                    jk.getPrioriteetti(),
                                    viimeisinVaihe,
                                    jonosijatHakemusOidinMukaan,
                                    jk.getNimi(),jarjestysnumero, edellinenOsallistuminen
                            );
                        } else {
                            hakemuslaskinService.suoritaLaskentaHakemukselle(
                                    new Hakukohde(hakukohdeOid, hakukohteenValintaperusteet, korkeakouluhaku),
                                    hw,
                                    laskentahakemukset,
                                    totuusarvofunktio.get(),
                                    jk.getPrioriteetti(),
                                    viimeisinVaihe,
                                    jonosijatHakemusOidinMukaan,
                                    jk.getNimi(),
                                    jarjestysnumero, edellinenOsallistuminen
                            );
                        }
                    }
                }

                jono.setJonosijat(jonosijatHakemusOidinMukaan.values().stream()
                        .map(this::createJonosija)
                        .collect(Collectors.toList()));

                if (j.isPoistetaankoHylatyt()) {
                    List<Jonosija> filteroity = jono.getJonosijat().stream()
                            .filter(sija -> {
                                boolean tila = !sija.getJarjestyskriteeritulokset().get(0).getTila().equals(JarjestyskriteerituloksenTila.HYLATTY);
                                TilaJaSelite tilaJaSelite = edellinenValinnanvaiheKasittelija.hakemusHyvaksyttavissaEdellisenValinnanvaiheenMukaan(sija.getHakemusOid(), edellinenVaihe);
                                return tila || tilaJaSelite.getTila().equals(JarjestyskriteerituloksenTila.HYLATTY);
                            })
                            .collect(Collectors.toList());
                    jono.setJonosijat(filteroity);
                }
                valinnanvaihe.getValintatapajonot().add(jono);
            }
        }
    }

    private Valintatapajono createValintatapajono(ValintatapajonoJarjestyskriteereillaDTO j) {
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
        jono.setKaytetaanValintalaskentaa(j.getKaytetaanValintalaskentaa());
        return jono;
    }

    private void jarjestaValinnanVaiheenJarjestysluvunMukaan(List<ValintaperusteetDTO> valintaperusteet) {
        valintaperusteet.sort(Comparator.comparingInt(o -> o.getValinnanVaihe().getValinnanVaiheJarjestysluku()));
    }

    private Map<String, String> muodostaHakukohteenValintaperusteetMap(List<HakukohteenValintaperusteDTO> hakukohteenValintaperuste) {
        Map<String, String> map = new HashMap<>();
        for (HakukohteenValintaperusteDTO vp : hakukohteenValintaperuste) {
            map.put(vp.getTunniste(), vp.getArvo());
        }
        return map;
    }

    private Map<String, Hakemukset> jarjestaHakemuksetHakukohteittain(List<HakemusDTO> hakemukset) {
        Map<String, Hakemukset> hakukohdeHakemukset = new HashMap<>();
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
                hakukohdeHakemukset.get(hakukohdeOid).getLaskentahakemukset().add(h.getLaskentahakemus());
            }
        }
        return hakukohdeHakemukset;
    }

    private Valinnanvaihe haeTaiLuoValinnanvaihe(String valinnanvaiheOid, String hakuOid, String hakukohdeOid, int jarjestysnumero) {
        Valinnanvaihe valinnanvaihe = valinnanvaiheDAO.haeValinnanvaihe(valinnanvaiheOid);

        // Tarkistetaan ettei jää haamuvaiheita OVT-7668
        List<Valinnanvaihe> vaiheet = valinnanvaiheDAO.haeValinnanvaiheetJarjestysnumerolla(hakuOid, hakukohdeOid, jarjestysnumero);
        for (Valinnanvaihe vaihe : vaiheet) {
            if (!vaihe.getValinnanvaiheOid().equals(valinnanvaiheOid)) {
                valinnanvaiheDAO.poistaValinnanvaihe(vaihe);
            }
        }

        if (valinnanvaihe != null) {
            poistaVanhatJonotJaHistoriat(valinnanvaihe);
        } else {
            valinnanvaihe = new Valinnanvaihe();
        }
        return valinnanvaihe;
    }

    private void poistaVanhatJonotJaHistoriat(Valinnanvaihe valinnanvaihe) {
        List<Valintatapajono> saastettavat = new ArrayList<>();
        List<Valintatapajono> poistettavat = new ArrayList<>();
        for (Valintatapajono jono : valinnanvaihe.getValintatapajonot()) {
            if (jono.getKaytetaanValintalaskentaa() == null || jono.getKaytetaanValintalaskentaa()) {
                for (Jonosija jonosija : jono.getJonosijat()) {
                    for (Jarjestyskriteeritulos tulos : jonosija.getJarjestyskriteeritulokset()) {
                        jarjestyskriteerihistoriaDAO.delete(tulos.getHistoria());
                    }
                }
                poistettavat.add(jono);
            } else {
                saastettavat.add(jono);
            }
        }

        valinnanvaihe.getValintatapajonot().clear();
        valinnanvaihe.getValintatapajonot().addAll(saastettavat);
        valinnanvaiheDAO.saveOrUpdate(valinnanvaihe);
        poistettavat.forEach(valinnanvaiheDAO::poistaJono);
    }

    private void poistaHaamuryhmat(List<ValintaperusteetHakijaryhmaDTO> hakijaryhmat, String hakukohdeOid) {
        List<String> oidit = hakijaryhmat.stream().map(ValintaperusteetHakijaryhmaDTO::getOid).collect(Collectors.toList());

        hakijaryhmaDAO.haeHakijaryhmat(hakukohdeOid).stream()
                .filter(h -> oidit.indexOf(h.getHakijaryhmaOid()) == -1)
                .forEach(hakijaryhmaDAO::poistaHakijaryhma);
    }

    private Hakijaryhma haeTaiLuoHakijaryhma(ValintaperusteetHakijaryhmaDTO dto) {
        Hakijaryhma hakijaryhma = hakijaryhmaDAO.haeHakijaryhma(dto.getOid()).orElse(new Hakijaryhma());
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
        if(dto.getHakijaryhmatyyppikoodi() != null) {
            hakijaryhma.setHakijaryhmatyyppikoodiUri(dto.getHakijaryhmatyyppikoodi().getUri());
        }

        poistaVanhatHistoriat(hakijaryhma);
        hakijaryhma.getJonosijat().clear();

        return hakijaryhma;
    }

    private void poistaVanhatHistoriat(Hakijaryhma hakijaryhma) {
        for (Jonosija jonosija : hakijaryhma.getJonosijat()) {
            for (Jarjestyskriteeritulos tulos : jonosija.getJarjestyskriteeritulokset()) {
                jarjestyskriteerihistoriaDAO.delete(tulos.getHistoria());
            }
        }
    }
}
