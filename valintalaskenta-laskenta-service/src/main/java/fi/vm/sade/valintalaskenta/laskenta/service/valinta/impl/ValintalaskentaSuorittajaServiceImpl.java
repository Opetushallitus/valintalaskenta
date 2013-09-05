package fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl;

import fi.vm.sade.kaava.Laskentadomainkonvertteri;
import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.hakemus.schema.HakukohdeTyyppi;
import fi.vm.sade.service.valintaperusteet.laskenta.Lukuarvofunktio;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakemus;
import fi.vm.sade.service.valintaperusteet.laskenta.api.LaskentaService;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Laskentatulos;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Hylattytila;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Tila;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Virhetila;
import fi.vm.sade.service.valintaperusteet.model.Funktiokutsu;
import fi.vm.sade.service.valintaperusteet.schema.JarjestyskriteeriTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.TavallinenValinnanVaiheTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteetTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintatapajonoJarjestyskriteereillaTyyppi;
import fi.vm.sade.service.valintaperusteet.service.validointi.virhe.LaskentakaavaEiOleValidiException;
import fi.vm.sade.valintalaskenta.domain.valinta.*;
import fi.vm.sade.valintalaskenta.laskenta.dao.JarjestyskriteerihistoriaDAO;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValinnanvaiheDAO;
import fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion.FunktioKutsuTyyppiToFunktioKutsuConverter;
import fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion.HakemusTyyppiToHakemusConverter;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.ValintalaskentaSuorittajaService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author Jussi Jartamo
 */
@Service
public class ValintalaskentaSuorittajaServiceImpl implements ValintalaskentaSuorittajaService {

    private static final Logger LOG = LoggerFactory.getLogger(ValintalaskentaSuorittajaServiceImpl.class);

    @Autowired
    private LaskentaService laskentaService;

    @Autowired
    private HakemusTyyppiToHakemusConverter hakemusConverter;

    @Autowired
    private FunktioKutsuTyyppiToFunktioKutsuConverter funktiokutsuConverter;

    @Autowired
    private ValinnanvaiheDAO valinnanvaiheDAO;

    @Autowired
    private JarjestyskriteerihistoriaDAO jarjestyskriteerihistoriaDAO;

    @Override
    public void suoritaLaskenta(List<HakemusTyyppi> kaikkiHakemukset, List<ValintaperusteetTyyppi> valintaperusteet) {

        Map<String, Hakemukset> hakemuksetHakukohteittain = jarjestaHakemuksetHakukohteittain(kaikkiHakemukset);
        for (ValintaperusteetTyyppi vp : valintaperusteet) {
            String hakukohdeOid = vp.getHakukohdeOid();
            String hakuoid = vp.getHakuOid();
            String tarjoajaOid = vp.getTarjoajaOid();

            List<HakemusWrapper> hakemukset = hakemuksetHakukohteittain.get(hakukohdeOid).getHakemukset();
            List<Hakemus> laskentahakemukset = hakemuksetHakukohteittain.get(hakukohdeOid).getLaskentahakemukset();
            if (hakemukset == null || hakemukset.isEmpty() ||
                    !(vp.getValinnanVaihe() instanceof TavallinenValinnanVaiheTyyppi)) {
                continue;
            }

            TavallinenValinnanVaiheTyyppi vaihe = (TavallinenValinnanVaiheTyyppi) vp.getValinnanVaihe();

            final String valinnanvaiheOid = vaihe.getValinnanVaiheOid();
            final int jarjestysnumero = vaihe.getValinnanVaiheJarjestysluku();

            Valinnanvaihe edellinenVaihe = haeEdellinenValinnanvaihe(hakuoid, hakukohdeOid, jarjestysnumero);
            Valinnanvaihe valinnanvaihe = haeTaiLuoValinnanvaihe(valinnanvaiheOid);
            valinnanvaihe.setHakukohdeOid(hakukohdeOid);
            valinnanvaihe.setHakuOid(hakuoid);
            valinnanvaihe.setJarjestysnumero(jarjestysnumero);
            valinnanvaihe.setValinnanvaiheOid(valinnanvaiheOid);
            valinnanvaihe.setTarjoajaOid(tarjoajaOid);

            for (ValintatapajonoJarjestyskriteereillaTyyppi j : vaihe.getValintatapajono()) {
                Valintatapajono jono = new Valintatapajono();
                jono.setAloituspaikat(j.getAloituspaikat());
                jono.setEiVarasijatayttoa(j.isEiVarasijatayttoa());
                jono.setNimi(j.getNimi());
                jono.setPrioriteetti(j.getPrioriteetti());
                jono.setSiirretaanSijoitteluun(j.isSiirretaanSijoitteluun());
                jono.setTasasijasaanto(Tasasijasaanto.valueOf(j.getTasasijasaanto().name()));
                jono.setValintatapajonoOid(j.getOid());

                Map<String, Jonosija> jonosijatHakemusOidinMukaan = new HashMap<String, Jonosija>();
                for (JarjestyskriteeriTyyppi jk : j.getJarjestyskriteerit()) {
                    try {
                        Jarjestyskriteeritulos jktulos = new Jarjestyskriteeritulos();
                        jktulos.setPrioriteetti(jk.getPrioriteetti());

                        Funktiokutsu funktiokutsu = funktiokutsuConverter.convert(jk.getFunktiokutsu());
                        Lukuarvofunktio lukuarvofunktio = Laskentadomainkonvertteri.muodostaLukuarvolasku(funktiokutsu);
                        for (HakemusWrapper hw : hakemukset) {
                            StringBuffer historia = new StringBuffer();
                            Laskentatulos<BigDecimal> tulos = laskentaService.suoritaLasku(hakukohdeOid,
                                    hw.getLaskentahakemus(), laskentahakemukset, lukuarvofunktio, historia);

                            jktulos.setArvo(tulos.getTulos());

                            HakemusTyyppi hakemus = hw.getHakemusTyyppi();
                            EdellinenValinnanvaiheTila edellinenValinnanvaiheTila =
                                    EdellinenValinnanvaiheKasittelija.hakemusHyvaksyttavissa(
                                            hakemus.getHakemusOid(), edellinenVaihe);

                            if (!JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA.equals(edellinenValinnanvaiheTila.getTila())) {
                                jktulos.setTila(edellinenValinnanvaiheTila.getTila());
                                jktulos.setKuvaus(edellinenValinnanvaiheTila.getSelite());
                            } else if (Tila.Tilatyyppi.HYLATTY.equals(tulos.getTila().getTilatyyppi())) {
                                jktulos.setTila(JarjestyskriteerituloksenTila.HYLATTY);
                                if (tulos.getTila() instanceof Hylattytila) {
                                    jktulos.setKuvaus(((Hylattytila) tulos.getTila()).getKuvaus());
                                }
                            } else if (Tila.Tilatyyppi.VIRHE.equals(tulos.getTila().getTilatyyppi())) {
                                jktulos.setTila(JarjestyskriteerituloksenTila.VIRHE);
                                if (tulos.getTila() instanceof Virhetila) {
                                    jktulos.setKuvaus(((Virhetila) tulos.getTila()).getKuvaus());
                                }
                            } else if (Tila.Tilatyyppi.HYVAKSYTTAVISSA.equals(tulos.getTila().getTilatyyppi())) {
                                jktulos.setTila(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA);
                            } else {
                                jktulos.setTila(JarjestyskriteerituloksenTila.MAARITTELEMATON);
                            }

                            if (!jonosijatHakemusOidinMukaan.containsKey(hakemus.getHakemusOid())) {
                                Jonosija jonosija = new Jonosija();
                                jonosija.setEtunimi(hakemus.getHakijanEtunimi());
                                jonosija.setHakemusOid(hakemus.getHakemusOid());
                                jonosija.setHakijaOid(hakemus.getHakijaOid());
                                jonosija.setHakutoiveprioriteetti(hw.getHakutoiveprioriteetti());
                                jonosija.setHarkinnanvarainen(hw.isHarkinnanvaraisuus());
                                jonosija.setSukunimi(hakemus.getHakijanSukunimi());
                                jonosijatHakemusOidinMukaan.put(hakemus.getHakemusOid(), new Jonosija());
                            }

                            Jonosija jonosija = jonosijatHakemusOidinMukaan.get(hakemus.getHakemusOid());
                            jonosija.getJarjestyskriteeritulokset().add(jktulos);

                            Jarjestyskriteerihistoria jkhistoria = new Jarjestyskriteerihistoria();
                            jkhistoria.setHistoria(historia.toString());
                            jarjestyskriteerihistoriaDAO.create(jkhistoria);
                            jktulos.setHistoria(jkhistoria.getId());
                        }
                    } catch (LaskentakaavaEiOleValidiException e) {
                        LOG.error("Valintatapajonon {} prioriteetilla {} olevan järjestyskriteerin " +
                                "laskentakaava ei ole validi. Laskentaa ei voida suorittaa.",
                                new Object[]{j.getOid(), jk.getPrioriteetti()});
                        continue;
                    }
                }

                jono.getJonosijat().addAll(jonosijatHakemusOidinMukaan.values());
            }

            valinnanvaiheDAO.create(valinnanvaihe);
        }
    }

    private Map<String, Hakemukset> jarjestaHakemuksetHakukohteittain(List<HakemusTyyppi> hakemukset) {
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
                hakukohdeHakemukset.get(hakukohdeoid).getLaskentahakemukset().add(h.getLaskentahakemus());
            }
        }
        return hakukohdeHakemukset;
    }

    private Valinnanvaihe haeEdellinenValinnanvaihe(String hakuOid, String hakukohdeOid, int jarjestysnumero) {
        return valinnanvaiheDAO.haeEdellinenValinnanvaihe(hakuOid, hakukohdeOid, jarjestysnumero);
    }

    private Valinnanvaihe haeTaiLuoValinnanvaihe(String valinnanvaiheOid) {
        Valinnanvaihe valinnanvaihe = valinnanvaiheDAO.haeValinnanvaihe(valinnanvaiheOid);

        // Poistetaan vanhat historiat
        if (valinnanvaihe != null) {
            for (Valintatapajono jono : valinnanvaihe.getValintatapajonot()) {
                for (Jonosija jonosija : jono.getJonosijat()) {
                    for (Jarjestyskriteeritulos tulos : jonosija.getJarjestyskriteeritulokset()) {
                        jarjestyskriteerihistoriaDAO.delete(tulos.getHistoria());
                    }
                }
            }

            valinnanvaihe.getValintatapajonot().clear();
        } else {
            valinnanvaihe = new Valinnanvaihe();
        }

        return valinnanvaihe;
    }

//    /**
//     * TODO
//     * <p/>
//     * edellinenValinnanvaihe on refaktoroitava koodista pois tehdaan niin etta
//     * lasketaan tulokset ensin koko setille ja sen jalkeen toisessa loopissa
//     * mietitaan onko tila hyvaksyttavissa vai hylatty
//     *
//     * @param kaikkiHakemukset
//     * @param valintaperusteet
//     */
//    public void suoritaLaskenta(List<HakemusTyyppi> kaikkiHakemukset, List<ValintaperusteetTyyppi> valintaperusteet) {
//
//        // <hakukohdeoid, list<Hakemuswrapper> >
//        Map<String, List<HakemusWrapper>> hakemuksetHakukohteittain = jarjestaHakemuksetHakukohteittain(kaikkiHakemukset);
//
//        for (ValintaperusteetTyyppi valintaperuste : valintaperusteet) {
//
//            String hakukohdeOid = valintaperuste.getHakukohdeOid();
//            String hakuoid = valintaperuste.getHakuOid();
//            String tarjoajaOid = valintaperuste.getTarjoajaOid();
//            List<HakemusWrapper> hakemukset = hakemuksetHakukohteittain.get(hakukohdeOid);
//            if (hakemukset == null || hakemukset.isEmpty()) {
//                continue;
//            }
//            if (!(valintaperuste.getValinnanVaihe() instanceof TavallinenValinnanVaiheTyyppi)) {
//                continue; // Laskentaa voi suorittaa vain ns. tavallisille
//                // valinnan vaiheille. Toisin sanoen ei
//                // valintakoevalinnanvaiheille.
//            }
//
//            TavallinenValinnanVaiheTyyppi vaihe = (TavallinenValinnanVaiheTyyppi) valintaperuste.getValinnanVaihe();
//
//            String valinnanvaiheoid = vaihe.getValinnanVaiheOid();
//            int jarjestysnumero = vaihe.getValinnanVaiheJarjestysluku();
//
//            Map<String, Esiintyminen> edellinenValinnanvaihe = hakemusoidHyvaksyttavissaJonoissa(edellinenValinnanvaihe(
//                    hakukohdeOid, jarjestysnumero));
//
//            VersiohallintaHakukohde versiohallinta = paivitaTaiLuoVersioituhakukohde(hakuoid, hakukohdeOid,
//                    valinnanvaiheoid, tarjoajaOid, jarjestysnumero);
//            Versioituhakukohde versioituhakukohde = versiohallinta.getHakukohteet().haeUusinVersio();
//            Valinnanvaihe valinnanvaihe = versioituhakukohde.getHakukohde().getValinnanvaihe();
//
//            for (ValintatapajonoJarjestyskriteereillaTyyppi jono : vaihe.getValintatapajono()) {
//                Valintatapajono valintatapajono = new Valintatapajono();
//                valintatapajono.setOid(jono.getOid());
//                valintatapajono.setNimi(jono.getNimi());
//                valintatapajono.setSiirretaanSijoitteluun(jono.isSiirretaanSijoitteluun());
//                valintatapajono.setPrioriteetti(jono.getPrioriteetti());
//                valintatapajono.setAloituspaikat(jono.getAloituspaikat());
//                valintatapajono.setTasasijasaanto(Tasasijasaanto.valueOf(jono.getTasasijasaanto().name()));
//                valintatapajono.setEiVarasijatayttoa(jono.isEiVarasijatayttoa());
//
//                for (HakemusWrapper h : hakemukset) {
//                    Jonosija jonosija = new Jonosija();
//                    valintatapajono.getJonosijat().add(jonosija);
//                    jonosija.setEtunimi(h.getHakemusTyyppi().getHakijanEtunimi());
//                    jonosija.setSukunimi(h.getHakemusTyyppi().getHakijanSukunimi());
//                    jonosija.setHakemusoid(h.getHakemusTyyppi().getHakemusOid());
//                    jonosija.setHakijaoid(h.getHakemusTyyppi().getHakijaOid());
//                    Integer hakutoive = haeHakutoiveNumero(h, hakukohdeOid);
//                    jonosija.setPrioriteetti(hakutoive);
//
//                    // StringBuffer historia = new StringBuffer();
//                    for (JarjestyskriteeriTyyppi j : jono.getJarjestyskriteerit()) {
//                        Funktiokutsu funktiokutsu = funktiokutsuConverter.convert(j.getFunktiokutsu());
//                        StringBuffer historia = new StringBuffer();
//                        Jarjestyskriteeritulos jarjestyskriteeritulos = suoritaLaskenta(
//                                hakukohdeOid,
//                                funktiokutsu,
//                                h,
//                                hakemukset,
//                                edellinenValinnanvaihe != null ? edellinenValinnanvaihe.get(h.getHakemusTyyppi()
//                                        .getHakemusOid()) : null, historia);
//                        assert (jarjestyskriteeritulos != null);
//
//                        JonosijaHistoria jonosijaHistoria = new JonosijaHistoria();
//                        jonosijaHistoria.setHistoria(historia.toString());
//                        jonosijaHistoriaDAO.create(jonosijaHistoria);
//
//                        jonosija.getHistoriat().add(jonosijaHistoria);
//                        jonosija.getJarjestyskriteerit().put(j.getPrioriteetti(), jarjestyskriteeritulos);
//                    }
//
//                }
//
//                valintatapajono.setVersio(versioituhakukohde.getVersio());
//                valinnanvaihe.getValintatapajono().add(valintatapajono);
//
//                valintatapajonoDAO.createOrUpdate(valintatapajono);
//
//            }
//            versiohallinta.getHakukohteet().add(versioituhakukohde);
//            versiohallintaHakukohdeDAO.createOrUpdate(versiohallinta);
//
//        }
//    }
//
//    private Integer haeHakutoiveNumero(HakemusWrapper h, String hakukohdeOid) {
//        for (HakukohdeTyyppi hkt : h.getHakemusTyyppi().getHakutoive()) {
//            if (hkt.getHakukohdeOid().equals(hakukohdeOid)) {
//                return hkt.getPrioriteetti();
//            }
//        }
//        return null;
//    }
//
//    private Jarjestyskriteeritulos suoritaLaskenta(String hakukohde, Funktiokutsu funktiokutsu,
//                                                   HakemusWrapper kasiteltavaHakemus, List<HakemusWrapper> kaikkiHakemukset, Esiintyminen esiintyminen,
//                                                   StringBuffer laskentaHistoria) {
//        Funktiotyyppi tyyppi = funktiokutsu.getFunktionimi().getTyyppi();
//        Jarjestyskriteeritulos jarjestyskriteeritulos = new Jarjestyskriteeritulos();
//
//        switch (tyyppi) {
//            case LUKUARVOFUNKTIO:
//                Funktiokutsu f = Laskentakaavavalidaattori.validoiLaskettavaKaava(funktiokutsu);
//                for (Funktioargumentti farg : f.getFunktioargumentit()) {
//                    for (Abstraktivalidointivirhe v : farg.getFunktiokutsuChild().getValidointivirheet()) {
//                        Validointivirhe vv = (Validointivirhe) v;
//                        LOG.debug("Tyyppi {}, viesti {}", new Object[]{vv.getVirhetyyppi(), vv.getVirheviesti()});
//                    }
//                }
//
//                List<Hakemus> kaikkiLaskentaHakemukset = new ArrayList<Hakemus>();
//                for (HakemusWrapper wrapper : kaikkiHakemukset) {
//                    kaikkiLaskentaHakemukset.add(wrapper.getLaskentahakemus());
//                }
//
//                Laskentatulos<BigDecimal> laskentatulos = laskentaService.suoritaLasku(hakukohde,
//                        kasiteltavaHakemus.getLaskentahakemus(), kaikkiLaskentaHakemukset,
//                        Laskentadomainkonvertteri.muodostaLukuarvolasku(funktiokutsu), laskentaHistoria);
//                Tila tila = laskentatulos.getTila();
//
//                if (Tilatyyppi.HYLATTY.equals(tila.getTilatyyppi())) {
//                    jarjestyskriteeritulos.setTila(JarjestyskriteerituloksenTila.HYLATTY);
//                    if (tila instanceof Hylattytila) {
//                        Hylattytila hylattytila = (Hylattytila) tila;
//                        jarjestyskriteeritulos.setKuvaus(hylattytila.getKuvaus());
//                    }
//                } else if (Tilatyyppi.VIRHE.equals(tila.getTilatyyppi())) {
//                    jarjestyskriteeritulos.setTila(JarjestyskriteerituloksenTila.VIRHE);
//                    if (tila instanceof Virhetila) {
//                        Virhetila virhetila = (Virhetila) tila;
//                        jarjestyskriteeritulos.setKuvaus(virhetila.getKuvaus());
//                    }
//                } else if (Tilatyyppi.HYVAKSYTTAVISSA.equals(tila.getTilatyyppi())) {
//                    // edelliseen valinnanvaiheeseen liittyvän
//                    // hylkäämisperusteen käsittely
//                    if (esiintyminen == null || esiintyminen.getHyvaksyttavissa() > 0) {
//                        jarjestyskriteeritulos.setTila(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA);
//                    } else {
//                        if (esiintyminen.getHyvaksyttavissa().equals(0) && esiintyminen.getEsiintyy() > 0) {
//                            // hylätään koska ei ollut kertaakaan
//                            // hyvaksyttavissä edellisessä
//                            // valinnanvaiheessa
//                            jarjestyskriteeritulos.setTila(JarjestyskriteerituloksenTila.HYLATTY);
//                            jarjestyskriteeritulos
//                                    .setKuvaus("Hylätty koska edellisessä valinnanvaiheessa oli hylätty kaikissa jonoissa!");
//                        }
//                    }
//                } else {
//                    jarjestyskriteeritulos.setTila(JarjestyskriteerituloksenTila.MAARITTELEMATON);
//                }
//                BigDecimal tulos = laskentatulos.getTulos();
//                if (tulos != null) {
//                    jarjestyskriteeritulos.setArvo(tulos);
//                }
//                return jarjestyskriteeritulos;
//            default:
//                throw new LaskentaVaarantyyppisellaFunktiollaException("Palvelu hyväksyy vain lukuarvofunktioita!");
//        }
//    }
//
//    // REFAKTORITAVA
//    private Map<String, Esiintyminen> hakemusoidHyvaksyttavissaJonoissa(Hakukohde hakukohde) {
//        if (hakukohde == null) {
//            return null;
//        }
//        Map<String, Esiintyminen> hakemusoidHyvaksyttyJonoissa = new HashMap<String, Esiintyminen>();
//        for (Valintatapajono jono : hakukohde.getValinnanvaihe().getValintatapajono()) {
//            for (Jonosija tulos : jono.getJonosijat()) {
//                String hakemusoid = tulos.getHakemusoid();
//                if (tulos.getJarjestyskriteerit().size() < 2) {
//                    // ei esiinny mutta hyvaksyttavissa
//                    // LOG.warn("JONOSIJALLA EI OLE EDELTÄVIÄ JÄRJESTYSKRITEEREITÄ (jonosija={}, kuvaus={})",
//                    // new Object[] { tulos.getJonosija(), tulos.getKuvaus() });
//                    hakemusoidHyvaksyttyJonoissa.put(hakemusoid, new Esiintyminen(0, 0));
//                } else {
//                    if (JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA.equals(tulos.getJarjestyskriteerit().firstEntry()
//                            .getValue().getTila())) {
//                        if (hakemusoidHyvaksyttyJonoissa.containsKey(hakemusoid)) {
//                            Esiintyminen esiintyminen = hakemusoidHyvaksyttyJonoissa.get(hakemusoid);
//                            esiintyminen.inkrementoiEsiintyminen();
//                            esiintyminen.inkrementoiHyvaksyttavissa();
//                            hakemusoidHyvaksyttyJonoissa.put(hakemusoid, esiintyminen);
//                        } else {
//                            // ensiesiintyminen ja hyväksyttävissä
//                            hakemusoidHyvaksyttyJonoissa.put(hakemusoid, new Esiintyminen(1, 1));
//                        }
//                    } else {
//                        if (!hakemusoidHyvaksyttyJonoissa.containsKey(hakemusoid)) {
//                            // esiintyy mutta ei ole hyväksyttävissä
//                            hakemusoidHyvaksyttyJonoissa.put(hakemusoid, new Esiintyminen(0, 1));
//                        } else {
//                            Esiintyminen esiintyminen = hakemusoidHyvaksyttyJonoissa.get(hakemusoid);
//                            esiintyminen.inkrementoiEsiintyminen();
//                            hakemusoidHyvaksyttyJonoissa.put(hakemusoid, esiintyminen);
//                        }
//                    }
//                }
//            }
//        }
//        return hakemusoidHyvaksyttyJonoissa;
//    }
//
//    /*
//     * Edellinen valinnanvaihe tai null jos ei ole
//     */
//    private Hakukohde edellinenValinnanvaihe(String oid, int jarjestysnumero) {
//
//        List<VersiohallintaHakukohde> kaksiEdellistaValinnanvaihetta = versiohallintaHakukohdeDAO
//                .findTwoLatestByHakukohdeOid(oid);
//        // if (kaksiEdellistaValinnanvaihetta.size() == 0) {
//        // edellistä valinnanvaihetta ei ollut joten
//        // hylkäys/hyväksymisperuste saa tulla suoraan funktiolta
//        if (kaksiEdellistaValinnanvaihetta.size() == 1) {
//            assert (kaksiEdellistaValinnanvaihetta.get(0).getJarjestysnumero() <= jarjestysnumero);
//            if (kaksiEdellistaValinnanvaihetta.get(0).getJarjestysnumero() != jarjestysnumero) {
//                assert (!kaksiEdellistaValinnanvaihetta.get(0).getHakukohteet().isEmpty());
//                return kaksiEdellistaValinnanvaihetta.get(0).getHakukohteet().last().getHakukohde();
//            } // else { // edellistä valinnanvaihetta ei ollut... }
//        } else if (kaksiEdellistaValinnanvaihetta.size() > 1) {
//            for (VersiohallintaHakukohde k : kaksiEdellistaValinnanvaihetta) {
//                if (k.getJarjestysnumero() != jarjestysnumero) {
//                    assert (!k.getHakukohteet().isEmpty());
//                    return k.getHakukohteet().last().getHakukohde();
//                }
//            }
//        }
//        return null;
//    }
//
//
//    /**
//     * @param hakuoid
//     * @param hakukohdeoid
//     * @param valinnanvaiheoid
//     * @param jarjestysnumero
//     * @return Päivitetty tai uusi versioituhakukode
//     */
//    private VersiohallintaHakukohde paivitaTaiLuoVersioituhakukohde(String hakuoid, String hakukohdeoid,
//                                                                    String valinnanvaiheoid, String tarjoajaOid, int jarjestysnumero) {
//        Hakukohde uusihakukohde = new Hakukohde();
//        uusihakukohde.setHakuoid(hakuoid);
//        uusihakukohde.setOid(hakukohdeoid);
//        uusihakukohde.setTarjoajaoid(tarjoajaOid);
//        Valinnanvaihe valinnanvaihe = new Valinnanvaihe();
//        valinnanvaihe.setJarjestysnumero(jarjestysnumero);
//        valinnanvaihe.setValinnanvaiheoid(valinnanvaiheoid);
//        uusihakukohde.setValinnanvaihe(valinnanvaihe);
//
//        VersiohallintaHakukohde versiohallinta = versiohallintaHakukohdeDAO.readByHakukohdeOidAndJarjestysnumero(
//                hakukohdeoid, jarjestysnumero);
//        //
//        if (versiohallinta == null) {
//            versiohallinta = new VersiohallintaHakukohde();
//            versiohallinta.setHakuoid(hakuoid);
//            versiohallinta.setTarjoajaoid(tarjoajaOid);
//            versiohallinta.setValinnanvaiheoid(valinnanvaiheoid);
//            versiohallinta.setHakukohdeoid(hakukohdeoid);
//            versiohallinta.setJarjestysnumero(jarjestysnumero);
//        }
//        Versioituhakukohde versioituhakukohde = new Versioituhakukohde();
//        versioituhakukohde.setHakukohde(uusihakukohde);
//        Long versioNumero = 0L;
//        if (!versiohallinta.getHakukohteet().isEmpty()) {
//            versioNumero = versiohallinta.getHakukohteet().haeUusinVersio().getVersio() + 1L;
//        }
//        versioituhakukohde.setVersio(versioNumero);
//        versiohallinta.getHakukohteet().add(versioituhakukohde);
//        return versiohallinta;
//    }
}
