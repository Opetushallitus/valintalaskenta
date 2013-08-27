package fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import fi.vm.sade.kaava.Laskentadomainkonvertteri;
import fi.vm.sade.kaava.Laskentakaavavalidaattori;
import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.hakemus.schema.HakukohdeTyyppi;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakemus;
import fi.vm.sade.service.valintaperusteet.laskenta.api.LaskentaService;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Laskentatulos;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Hylattytila;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Tila;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Tila.Tilatyyppi;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Virhetila;
import fi.vm.sade.service.valintaperusteet.model.Abstraktivalidointivirhe;
import fi.vm.sade.service.valintaperusteet.model.Funktioargumentti;
import fi.vm.sade.service.valintaperusteet.model.Funktiokutsu;
import fi.vm.sade.service.valintaperusteet.model.Funktiotyyppi;
import fi.vm.sade.service.valintaperusteet.schema.JarjestyskriteeriTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.TavallinenValinnanVaiheTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteetTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintatapajonoJarjestyskriteereillaTyyppi;
import fi.vm.sade.service.valintaperusteet.service.validointi.virhe.Validointivirhe;
import fi.vm.sade.valintalaskenta.dao.JonosijaHistoriaDAO;
import fi.vm.sade.valintalaskenta.dao.ValintatapajonoDAO;
import fi.vm.sade.valintalaskenta.dao.VersiohallintaHakukohdeDAO;
import fi.vm.sade.valintalaskenta.domain.Hakukohde;
import fi.vm.sade.valintalaskenta.domain.JarjestyskriteerituloksenTila;
import fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.Jonosija;
import fi.vm.sade.valintalaskenta.domain.JonosijaHistoria;
import fi.vm.sade.valintalaskenta.domain.Tasasijasaanto;
import fi.vm.sade.valintalaskenta.domain.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.Valintatapajono;
import fi.vm.sade.valintalaskenta.domain.VersiohallintaHakukohde;
import fi.vm.sade.valintalaskenta.domain.Versioituhakukohde;
import fi.vm.sade.valintalaskenta.laskenta.Esiintyminen;
import fi.vm.sade.valintalaskenta.laskenta.service.exception.LaskentaVaarantyyppisellaFunktiollaException;
import fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion.FunktioKutsuTyyppiToFunktioKutsuConverter;
import fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion.HakemusTyyppiToHakemusConverter;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.ValintalaskentaSuorittajaService;

/**
 * @author Jussi Jartamo
 */
@Service
public class ValintalaskentaSuorittajaServiceImpl implements ValintalaskentaSuorittajaService {

    private static final Logger LOG = LoggerFactory.getLogger(ValintalaskentaSuorittajaServiceImpl.class);

    @Autowired
    private LaskentaService laskentaService;

    @Autowired
    private VersiohallintaHakukohdeDAO versiohallintaHakukohdeDAO;

    @Autowired
    private ValintatapajonoDAO valintatapajonoDAO;

    @Autowired
    private HakemusTyyppiToHakemusConverter hakemusConverter;

    @Autowired
    private FunktioKutsuTyyppiToFunktioKutsuConverter funktiokutsuConverter;

    @Autowired
    private JonosijaHistoriaDAO jonosijaHistoriaDAO;

    /**
     * TODO
     * <p/>
     * edellinenValinnanvaihe on refaktoroitava koodista pois tehdaan niin etta
     * lasketaan tulokset ensin koko setille ja sen jalkeen toisessa loopissa
     * mietitaan onko tila hyvaksyttavissa vai hylatty
     * 
     * @param kaikkiHakemukset
     * @param valintaperusteet
     */
    public void suoritaLaskenta(List<HakemusTyyppi> kaikkiHakemukset, List<ValintaperusteetTyyppi> valintaperusteet) {

        // <hakukohdeoid, list<Hakemuswrapper> >
        Map<String, List<HakemusWrapper>> hakemuksetHakukohteittain = jarjestaHakemuksetHakukohteittain(kaikkiHakemukset);

        for (ValintaperusteetTyyppi valintaperuste : valintaperusteet) {

            String hakukohdeOid = valintaperuste.getHakukohdeOid();
            String hakuoid = valintaperuste.getHakuOid();
            String tarjoajaOid = valintaperuste.getTarjoajaOid();
            List<HakemusWrapper> hakemukset = hakemuksetHakukohteittain.get(hakukohdeOid);
            if (hakemukset == null || hakemukset.isEmpty()) {
                continue;
            }
            if (!(valintaperuste.getValinnanVaihe() instanceof TavallinenValinnanVaiheTyyppi)) {
                continue; // Laskentaa voi suorittaa vain ns. tavallisille
                          // valinnan vaiheille. Toisin sanoen ei
                          // valintakoevalinnanvaiheille.
            }

            TavallinenValinnanVaiheTyyppi vaihe = (TavallinenValinnanVaiheTyyppi) valintaperuste.getValinnanVaihe();

            String valinnanvaiheoid = vaihe.getValinnanVaiheOid();
            int jarjestysnumero = vaihe.getValinnanVaiheJarjestysluku();

            Map<String, Esiintyminen> edellinenValinnanvaihe = hakemusoidHyvaksyttavissaJonoissa(edellinenValinnanvaihe(
                    hakukohdeOid, jarjestysnumero));

            VersiohallintaHakukohde versiohallinta = paivitaTaiLuoVersioituhakukohde(hakuoid, hakukohdeOid,
                    valinnanvaiheoid, tarjoajaOid, jarjestysnumero);
            Versioituhakukohde versioituhakukohde = versiohallinta.getHakukohteet().haeUusinVersio();
            Valinnanvaihe valinnanvaihe = versioituhakukohde.getHakukohde().getValinnanvaihe();

            for (ValintatapajonoJarjestyskriteereillaTyyppi jono : vaihe.getValintatapajono()) {
                Valintatapajono valintatapajono = new Valintatapajono();
                valintatapajono.setOid(jono.getOid());
                valintatapajono.setNimi(jono.getNimi());
                valintatapajono.setSiirretaanSijoitteluun(jono.isSiirretaanSijoitteluun());
                valintatapajono.setPrioriteetti(jono.getPrioriteetti());
                valintatapajono.setAloituspaikat(jono.getAloituspaikat());
                valintatapajono.setTasasijasaanto(Tasasijasaanto.valueOf(jono.getTasasijasaanto().name()));
                valintatapajono.setEiVarasijatayttoa(jono.isEiVarasijatayttoa());

                for (HakemusWrapper h : hakemukset) {
                    Jonosija jonosija = new Jonosija();
                    valintatapajono.getJonosijat().add(jonosija);
                    jonosija.setEtunimi(h.getHakemusTyyppi().getHakijanEtunimi());
                    jonosija.setSukunimi(h.getHakemusTyyppi().getHakijanSukunimi());
                    jonosija.setHakemusoid(h.getHakemusTyyppi().getHakemusOid());
                    jonosija.setHakijaoid(h.getHakemusTyyppi().getHakijaOid());
                    Integer hakutoive = haeHakutoiveNumero(h, hakukohdeOid);
                    jonosija.setPrioriteetti(hakutoive);

                    // StringBuffer historia = new StringBuffer();
                    for (JarjestyskriteeriTyyppi j : jono.getJarjestyskriteerit()) {
                        Funktiokutsu funktiokutsu = funktiokutsuConverter.convert(j.getFunktiokutsu());
                        StringBuffer historia = new StringBuffer();
                        Jarjestyskriteeritulos jarjestyskriteeritulos = suoritaLaskenta(
                                hakukohdeOid,
                                funktiokutsu,
                                h,
                                hakemukset,
                                edellinenValinnanvaihe != null ? edellinenValinnanvaihe.get(h.getHakemusTyyppi()
                                        .getHakemusOid()) : null, historia);
                        assert (jarjestyskriteeritulos != null);

                        JonosijaHistoria jonosijaHistoria = new JonosijaHistoria();
                        jonosijaHistoria.setHistoria(historia.toString());
                        jonosijaHistoriaDAO.create(jonosijaHistoria);

                        jonosija.getHistoriat().add(jonosijaHistoria);
                        jonosija.getJarjestyskriteerit().put(j.getPrioriteetti(), jarjestyskriteeritulos);
                    }

                }

                valintatapajono.setVersio(versioituhakukohde.getVersio());
                valinnanvaihe.getValintatapajono().add(valintatapajono);

                valintatapajonoDAO.createOrUpdate(valintatapajono);

            }
            versiohallinta.getHakukohteet().add(versioituhakukohde);
            versiohallintaHakukohdeDAO.createOrUpdate(versiohallinta);

        }
    }

    private Integer haeHakutoiveNumero(HakemusWrapper h, String hakukohdeOid) {
        for (HakukohdeTyyppi hkt : h.getHakemusTyyppi().getHakutoive()) {
            if (hkt.getHakukohdeOid().equals(hakukohdeOid)) {
                return hkt.getPrioriteetti();
            }
        }
        return null;
    }

    private Jarjestyskriteeritulos suoritaLaskenta(String hakukohde, Funktiokutsu funktiokutsu,
            HakemusWrapper kasiteltavaHakemus, List<HakemusWrapper> kaikkiHakemukset, Esiintyminen esiintyminen,
            StringBuffer laskentaHistoria) {
        Funktiotyyppi tyyppi = funktiokutsu.getFunktionimi().getTyyppi();
        Jarjestyskriteeritulos jarjestyskriteeritulos = new Jarjestyskriteeritulos();

        switch (tyyppi) {
        case LUKUARVOFUNKTIO:
            Funktiokutsu f = Laskentakaavavalidaattori.validoiLaskettavaKaava(funktiokutsu);
            for (Funktioargumentti farg : f.getFunktioargumentit()) {
                for (Abstraktivalidointivirhe v : farg.getFunktiokutsuChild().getValidointivirheet()) {
                    Validointivirhe vv = (Validointivirhe) v;
                    LOG.debug("Tyyppi {}, viesti {}", new Object[] { vv.getVirhetyyppi(), vv.getVirheviesti() });
                }
            }

            List<Hakemus> kaikkiLaskentaHakemukset = new ArrayList<Hakemus>();
            for (HakemusWrapper wrapper : kaikkiHakemukset) {
                kaikkiLaskentaHakemukset.add(wrapper.getLaskentahakemus());
            }

            Laskentatulos<BigDecimal> laskentatulos = laskentaService.suoritaLasku(hakukohde,
                    kasiteltavaHakemus.getLaskentahakemus(), kaikkiLaskentaHakemukset,
                    Laskentadomainkonvertteri.muodostaLukuarvolasku(funktiokutsu), laskentaHistoria);
            Tila tila = laskentatulos.getTila();

            if (Tilatyyppi.HYLATTY.equals(tila.getTilatyyppi())) {
                jarjestyskriteeritulos.setTila(JarjestyskriteerituloksenTila.HYLATTY);
                if (tila instanceof Hylattytila) {
                    Hylattytila hylattytila = (Hylattytila) tila;
                    jarjestyskriteeritulos.setKuvaus(hylattytila.getKuvaus());
                }
            } else if (Tilatyyppi.VIRHE.equals(tila.getTilatyyppi())) {
                jarjestyskriteeritulos.setTila(JarjestyskriteerituloksenTila.VIRHE);
                if (tila instanceof Virhetila) {
                    Virhetila virhetila = (Virhetila) tila;
                    jarjestyskriteeritulos.setKuvaus(virhetila.getKuvaus());
                }
            } else if (Tilatyyppi.HYVAKSYTTAVISSA.equals(tila.getTilatyyppi())) {
                // edelliseen valinnanvaiheeseen liittyvän
                // hylkäämisperusteen käsittely
                if (esiintyminen == null || esiintyminen.getHyvaksyttavissa() > 0) {
                    jarjestyskriteeritulos.setTila(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA);
                } else {
                    if (esiintyminen.getHyvaksyttavissa().equals(0) && esiintyminen.getEsiintyy() > 0) {
                        // hylätään koska ei ollut kertaakaan
                        // hyvaksyttavissä edellisessä
                        // valinnanvaiheessa
                        jarjestyskriteeritulos.setTila(JarjestyskriteerituloksenTila.HYLATTY);
                        jarjestyskriteeritulos
                                .setKuvaus("Hylätty koska edellisessä valinnanvaiheessa oli hylätty kaikissa jonoissa!");
                    }
                }
            } else {
                jarjestyskriteeritulos.setTila(JarjestyskriteerituloksenTila.MAARITTELEMATON);
            }
            BigDecimal tulos = laskentatulos.getTulos();
            if (tulos != null) {
                jarjestyskriteeritulos.setArvo(tulos);
            }
            return jarjestyskriteeritulos;
        default:
            throw new LaskentaVaarantyyppisellaFunktiollaException("Palvelu hyväksyy vain lukuarvofunktioita!");
        }
    }

    // REFAKTORITAVA
    private Map<String, Esiintyminen> hakemusoidHyvaksyttavissaJonoissa(Hakukohde hakukohde) {
        if (hakukohde == null) {
            return null;
        }
        Map<String, Esiintyminen> hakemusoidHyvaksyttyJonoissa = new HashMap<String, Esiintyminen>();
        for (Valintatapajono jono : hakukohde.getValinnanvaihe().getValintatapajono()) {
            for (Jonosija tulos : jono.getJonosijat()) {
                String hakemusoid = tulos.getHakemusoid();
                if (tulos.getJarjestyskriteerit().size() < 2) {
                    // ei esiinny mutta hyvaksyttavissa
                    // LOG.warn("JONOSIJALLA EI OLE EDELTÄVIÄ JÄRJESTYSKRITEEREITÄ (jonosija={}, kuvaus={})",
                    // new Object[] { tulos.getJonosija(), tulos.getKuvaus() });
                    hakemusoidHyvaksyttyJonoissa.put(hakemusoid, new Esiintyminen(0, 0));
                } else {
                    if (JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA.equals(tulos.getJarjestyskriteerit().firstEntry()
                            .getValue().getTila())) {
                        if (hakemusoidHyvaksyttyJonoissa.containsKey(hakemusoid)) {
                            Esiintyminen esiintyminen = hakemusoidHyvaksyttyJonoissa.get(hakemusoid);
                            esiintyminen.inkrementoiEsiintyminen();
                            esiintyminen.inkrementoiHyvaksyttavissa();
                            hakemusoidHyvaksyttyJonoissa.put(hakemusoid, esiintyminen);
                        } else {
                            // ensiesiintyminen ja hyväksyttävissä
                            hakemusoidHyvaksyttyJonoissa.put(hakemusoid, new Esiintyminen(1, 1));
                        }
                    } else {
                        if (!hakemusoidHyvaksyttyJonoissa.containsKey(hakemusoid)) {
                            // esiintyy mutta ei ole hyväksyttävissä
                            hakemusoidHyvaksyttyJonoissa.put(hakemusoid, new Esiintyminen(0, 1));
                        } else {
                            Esiintyminen esiintyminen = hakemusoidHyvaksyttyJonoissa.get(hakemusoid);
                            esiintyminen.inkrementoiEsiintyminen();
                            hakemusoidHyvaksyttyJonoissa.put(hakemusoid, esiintyminen);
                        }
                    }
                }
            }
        }
        return hakemusoidHyvaksyttyJonoissa;
    }

    /*
     * Edellinen valinnanvaihe tai null jos ei ole
     */
    private Hakukohde edellinenValinnanvaihe(String oid, int jarjestysnumero) {

        List<VersiohallintaHakukohde> kaksiEdellistaValinnanvaihetta = versiohallintaHakukohdeDAO
                .findTwoLatestByHakukohdeOid(oid);
        // if (kaksiEdellistaValinnanvaihetta.size() == 0) {
        // edellistä valinnanvaihetta ei ollut joten
        // hylkäys/hyväksymisperuste saa tulla suoraan funktiolta
        if (kaksiEdellistaValinnanvaihetta.size() == 1) {
            assert (kaksiEdellistaValinnanvaihetta.get(0).getJarjestysnumero() <= jarjestysnumero);
            if (kaksiEdellistaValinnanvaihetta.get(0).getJarjestysnumero() != jarjestysnumero) {
                assert (!kaksiEdellistaValinnanvaihetta.get(0).getHakukohteet().isEmpty());
                return kaksiEdellistaValinnanvaihetta.get(0).getHakukohteet().last().getHakukohde();
            } // else { // edellistä valinnanvaihetta ei ollut... }
        } else if (kaksiEdellistaValinnanvaihetta.size() > 1) {
            for (VersiohallintaHakukohde k : kaksiEdellistaValinnanvaihetta) {
                if (k.getJarjestysnumero() != jarjestysnumero) {
                    assert (!k.getHakukohteet().isEmpty());
                    return k.getHakukohteet().last().getHakukohde();
                }
            }
        }
        return null;
    }

    private Map<String, List<HakemusWrapper>> jarjestaHakemuksetHakukohteittain(List<HakemusTyyppi> hakemukset) {
        Map<String, List<HakemusWrapper>> hakukohdeHakemukset = new HashMap<String, List<HakemusWrapper>>();
        for (HakemusTyyppi hakemus : hakemukset) {
            for (HakukohdeTyyppi hakukohde : hakemus.getHakutoive()) {
                String hakukohdeoid = hakukohde.getHakukohdeOid();
                if (!hakukohdeHakemukset.containsKey(hakukohdeoid)) {
                    hakukohdeHakemukset.put(hakukohdeoid, new ArrayList<HakemusWrapper>());
                }
                HakemusWrapper h = new HakemusWrapper();
                h.setHakemusTyyppi(hakemus);
                h.setLaskentahakemus(hakemusConverter.convert(hakemus));

                hakukohdeHakemukset.get(hakukohdeoid).add(h);
            }
        }
        return hakukohdeHakemukset;
    }

    /**
     * @param hakuoid
     * @param hakukohdeoid
     * @param valinnanvaiheoid
     * @param jarjestysnumero
     * @return Päivitetty tai uusi versioituhakukode
     */
    private VersiohallintaHakukohde paivitaTaiLuoVersioituhakukohde(String hakuoid, String hakukohdeoid,
            String valinnanvaiheoid, String tarjoajaOid, int jarjestysnumero) {
        Hakukohde uusihakukohde = new Hakukohde();
        uusihakukohde.setHakuoid(hakuoid);
        uusihakukohde.setOid(hakukohdeoid);
        uusihakukohde.setTarjoajaoid(tarjoajaOid);
        Valinnanvaihe valinnanvaihe = new Valinnanvaihe();
        valinnanvaihe.setJarjestysnumero(jarjestysnumero);
        valinnanvaihe.setValinnanvaiheoid(valinnanvaiheoid);
        uusihakukohde.setValinnanvaihe(valinnanvaihe);

        VersiohallintaHakukohde versiohallinta = versiohallintaHakukohdeDAO.readByHakukohdeOidAndJarjestysnumero(
                hakukohdeoid, jarjestysnumero);
        //
        if (versiohallinta == null) {
            versiohallinta = new VersiohallintaHakukohde();
            versiohallinta.setHakuoid(hakuoid);
            versiohallinta.setTarjoajaoid(tarjoajaOid);
            versiohallinta.setValinnanvaiheoid(valinnanvaiheoid);
            versiohallinta.setHakukohdeoid(hakukohdeoid);
            versiohallinta.setJarjestysnumero(jarjestysnumero);
        }
        Versioituhakukohde versioituhakukohde = new Versioituhakukohde();
        versioituhakukohde.setHakukohde(uusihakukohde);
        Long versioNumero = 0L;
        if (!versiohallinta.getHakukohteet().isEmpty()) {
            versioNumero = versiohallinta.getHakukohteet().haeUusinVersio().getVersio() + 1L;
        }
        versioituhakukohde.setVersio(versioNumero);
        versiohallinta.getHakukohteet().add(versioituhakukohde);
        return versiohallinta;
    }
}
