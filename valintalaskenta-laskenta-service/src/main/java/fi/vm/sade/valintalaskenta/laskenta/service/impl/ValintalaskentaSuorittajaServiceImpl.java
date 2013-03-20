package fi.vm.sade.valintalaskenta.laskenta.service.impl;

import java.util.ArrayList;
import java.util.Collection;
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
import fi.vm.sade.service.valintaperusteet.model.Abstraktivalidointivirhe;
import fi.vm.sade.service.valintaperusteet.model.Funktioargumentti;
import fi.vm.sade.service.valintaperusteet.model.Funktiokutsu;
import fi.vm.sade.service.valintaperusteet.model.Funktiotyyppi;
import fi.vm.sade.service.valintaperusteet.schema.JarjestyskriteeriTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteetTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintatapajonoJarjestyskriteereillaTyyppi;
import fi.vm.sade.service.valintaperusteet.service.validointi.virhe.Validointivirhe;
import fi.vm.sade.valintalaskenta.dao.ValintatapajonoDAO;
import fi.vm.sade.valintalaskenta.dao.VersiohallintaHakukohdeDAO;
import fi.vm.sade.valintalaskenta.domain.Hakukohde;
import fi.vm.sade.valintalaskenta.domain.JarjestyskriteerituloksenTila;
import fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.Valintatapajono;
import fi.vm.sade.valintalaskenta.domain.VersiohallintaHakukohde;
import fi.vm.sade.valintalaskenta.domain.Versioituhakukohde;
import fi.vm.sade.valintalaskenta.laskenta.Esiintyminen;
import fi.vm.sade.valintalaskenta.laskenta.HakemusHelper;
import fi.vm.sade.valintalaskenta.laskenta.service.ValintalaskentaSuorittajaService;
import fi.vm.sade.valintalaskenta.laskenta.service.exception.LaskentaVaarantyyppisellaFunktiollaException;
import fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion.FunktioKutsuTyyppiToFunktioKutsuConverter;
import fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion.HakemusTyyppiToHakemusConverter;

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

    public void suoritaLaskenta(List<HakemusTyyppi> hakemukset, List<ValintaperusteetTyyppi> valintaperusteet) {
        // Hakuoid ei ole toistaiseksi WSDL:ssä joten käytetään kovakoodattua
        // arvoa!
        String hakuoid = "syksynhaku"; // KOVAKOODATTUHAKUOID
        //
        Map<String, List<HakemusTyyppi>> hakukohdeHakemukset = resolveHakukohdeHakemukset(hakemukset);
        for (ValintaperusteetTyyppi valintaperuste : valintaperusteet) {
            String hakukohdeoid = valintaperuste.getHakukohdeOid();
            String valinnanvaiheoid = valintaperuste.getValinnanVaiheOid();
            int jarjestysnumero = valintaperuste.getValinnanVaiheJarjestysluku();
            // Haetaan edellinen (pykälää pienemmällä järjestysnumerolla)
            // valinnanvaihe mahdollisen
            // hylkäämisperustetarpeen vuoksi
            Map<String, Esiintyminen> edellinenValinnanvaihe = hakemusoidHyvaksyttavissaJonoissa(edellinenValinnanvaihe(
                    hakukohdeoid, jarjestysnumero));

            HakemusHelper hakemusKatko = new HakemusHelper();
            for (HakemusTyyppi h : hakukohdeHakemukset.get(hakukohdeoid)) {
                hakemusKatko.setHakemusOidHakemukset(h.getHakemusOid(), hakemusConverter.convert(h), h);
            }

            // päivittää tai luo uuden versioidun hakukohteen
            VersiohallintaHakukohde versiohallinta = paivitaTaiLuoVersioituhakukohde(hakuoid, hakukohdeoid,
                    valinnanvaiheoid, jarjestysnumero);
            Versioituhakukohde versioituhakukohde = versiohallinta.getHakukohteet().haeUusinVersio();
            Valinnanvaihe valinnanvaihe = versioituhakukohde.getHakukohde().getValinnanvaihe();

            for (ValintatapajonoJarjestyskriteereillaTyyppi jono : valintaperuste.getValintatapajonot()) {
                Valintatapajono valintatapajono = new Valintatapajono();
                valintatapajono.setOid(jono.getOid());
                valintatapajono.setNimi(jono.getNimi());
                valintatapajono.setSiirretaanSijoitteluun(jono.isSiirretaanSijoitteluun());
                valintatapajono.setPrioriteetti(jono.getPrioriteetti());
                valintatapajono.setAloituspaikat(jono.getAloituspaikat());

                for (Hakemus h : hakemusKatko.getHakemukset()) {
                    String hakemusoid = h.oid();
                    for (JarjestyskriteeriTyyppi j : jono.getJarjestyskriteerit()) {
                        Funktiokutsu funktiokutsu = funktiokutsuConverter.convert(j.getFunktiokutsu());

                        // Suoritetaan todellinen laskenta haetuille arvoille
                        Jarjestyskriteeritulos jarjestyskriteeritulos = suoritaLaskenta(hakukohdeoid, funktiokutsu, h,
                                hakemusKatko.getHakemukset(),
                                edellinenValinnanvaihe != null ? edellinenValinnanvaihe.get(hakemusoid) : null);
                        jarjestyskriteeritulos.setHakemusoid(hakemusoid);
                        jarjestyskriteeritulos.setEtunimi(hakemusKatko.getEtunimi(hakemusoid));
                        jarjestyskriteeritulos.setSukunimi(hakemusKatko.getSukunimi(hakemusoid));
                        valintatapajono.getJarjestyskriteeritulokset().add(jarjestyskriteeritulos);
                    }

                }
                valintatapajono.setVersio(versioituhakukohde.getVersio());
                valinnanvaihe.getValintatapajono().add(valintatapajono);
                valintatapajonoDAO.createOrUpdate(valintatapajono);
            }
            // LOG.info("Tallennetaan hakukohdetta! Hakukohdeoid {}",
            // uusihakukohde.getOid());
            versiohallinta.getHakukohteet().add(versioituhakukohde);
            versiohallintaHakukohdeDAO.createOrUpdate(versiohallinta);
        }
    }

    private Jarjestyskriteeritulos suoritaLaskenta(String hakukohde, Funktiokutsu funktiokutsu,
            Hakemus kasiteltavaHakemus, Collection<Hakemus> kaikkiHakemukset, Esiintyminen esiintyminen) {
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
            Laskentatulos<Double> laskentatulos = laskentaService.suoritaLasku(hakukohde, kasiteltavaHakemus,
                    kaikkiHakemukset, Laskentadomainkonvertteri.muodostaLukuarvolasku(funktiokutsu));
            Tila tila = laskentatulos.getTila();

            if (Tilatyyppi.HYLATTY.equals(tila.getTilatyyppi())) {
                jarjestyskriteeritulos.setTila(JarjestyskriteerituloksenTila.HYLATTY);
                if (tila instanceof Hylattytila) {
                    Hylattytila hylattytila = (Hylattytila) tila;
                    jarjestyskriteeritulos.setKuvaus(hylattytila.getKuvaus());
                }
            } else if (Tilatyyppi.HYVAKSYTTAVISSA.equals(tila.getTilatyyppi())) {
                // edelliseen valinnanvaiheeseen liittyvän
                // hylkäämisperusteen käsittely
                if (esiintyminen == null || esiintyminen.getHyvaksyttavissa() > 0) {
                    jarjestyskriteeritulos.setTila(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA);
                } else {
                    if (esiintyminen.getHyvaksyttavissa() == 0 && esiintyminen.getEsiintyy() > 0) {
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
            Double tulos = laskentatulos.getTulos();
            if (tulos != null) {
                jarjestyskriteeritulos.setArvo(tulos);
            }
            return jarjestyskriteeritulos;
        default:
            throw new LaskentaVaarantyyppisellaFunktiollaException("Palvelu hyväksyy vain lukuarvofunktioita!");
        }
    }

    private Map<String, Esiintyminen> hakemusoidHyvaksyttavissaJonoissa(Hakukohde hakukohde) {
        if (hakukohde == null) {
            return null;
        }
        Map<String, Esiintyminen> hakemusoidHyvaksyttyJonoissa = new HashMap<String, Esiintyminen>();
        for (Valintatapajono jono : hakukohde.getValinnanvaihe().getValintatapajono()) {
            for (Jarjestyskriteeritulos tulos : jono.getJarjestyskriteeritulokset()) {
                // jos hyväksyttävissä lisätään hyväksyntä
                String hakemusoid = tulos.getHakemusoid();
                if (JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA.equals(tulos.getTila())) {

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

    // hakukohdeoid -> Hakemus ja avainarvoparit
    private Map<String, List<HakemusTyyppi>> resolveHakukohdeHakemukset(List<HakemusTyyppi> hakemukset) {
        Map<String, List<HakemusTyyppi>> hakukohdeHakemukset = new HashMap<String, List<HakemusTyyppi>>();

        for (HakemusTyyppi hakemus : hakemukset) {

            for (HakukohdeTyyppi hakukohde : hakemus.getHakutoive()) {
                String hakukohdeoid = hakukohde.getHakukohdeOid();
                if (!hakukohdeHakemukset.containsKey(hakukohdeoid)) {
                    hakukohdeHakemukset.put(hakukohdeoid, new ArrayList<HakemusTyyppi>());
                }
                hakukohdeHakemukset.get(hakukohdeoid).add(hakemus);
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
            String valinnanvaiheoid, int jarjestysnumero) {
        Hakukohde uusihakukohde = new Hakukohde();
        uusihakukohde.setHakuoid(hakuoid);
        uusihakukohde.setOid(hakukohdeoid);
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