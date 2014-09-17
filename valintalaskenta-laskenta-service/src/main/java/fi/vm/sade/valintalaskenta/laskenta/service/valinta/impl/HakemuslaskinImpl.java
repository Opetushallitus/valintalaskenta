package fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl;

import fi.vm.sade.service.valintaperusteet.laskenta.Lukuarvofunktio;
import fi.vm.sade.service.valintaperusteet.laskenta.Totuusarvofunktio;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakemus;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakukohde;
import fi.vm.sade.service.valintaperusteet.laskenta.api.LaskentaService;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Laskentatulos;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Hylattytila;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Tila;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Virhetila;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.domain.valinta.*;
import fi.vm.sade.valintalaskenta.laskenta.dao.JarjestyskriteerihistoriaDAO;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.HakemuslaskinService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

/**
 * User: wuoti
 * Date: 5.9.2013
 * Time: 9.42
 */
@Service
public class HakemuslaskinImpl implements HakemuslaskinService {

    @Autowired
    private LaskentaService laskentaService;

    @Autowired
    private JarjestyskriteerihistoriaDAO jarjestyskriteerihistoriaDAO;

    @Autowired
    private EdellinenValinnanvaiheKasittelija edellinenValinnanvaiheKasittelija;

    private TilaJaSelite hakijaryhmanTilaJaSelite(Laskentatulos tulos) {
        JarjestyskriteerituloksenTila tila = JarjestyskriteerituloksenTila.MAARITTELEMATON;
        Map<String,String> kuvaus = null;
        String tekninenKuvaus = null;
        Tila laskettuTila = tulos.getTila();

        if (Tila.Tilatyyppi.HYLATTY.equals(laskettuTila.getTilatyyppi())) {
            tila = JarjestyskriteerituloksenTila.HYLATTY;
            if (laskettuTila instanceof Hylattytila) {
                kuvaus = ((Hylattytila) laskettuTila).getKuvaus();
                tekninenKuvaus = ((Hylattytila) laskettuTila).getTekninenKuvaus();
            }
        } else if (Tila.Tilatyyppi.VIRHE.equals(laskettuTila.getTilatyyppi())) {
            tila = JarjestyskriteerituloksenTila.VIRHE;
            if (laskettuTila instanceof Virhetila) {
                kuvaus = ((Virhetila) laskettuTila).getKuvaus();
            }
        } else if (Tila.Tilatyyppi.HYVAKSYTTAVISSA.equals(laskettuTila.getTilatyyppi())) {
            tila = JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA;
        }

        return new TilaJaSelite(tila,kuvaus,tekninenKuvaus);
    }

    @Override
    public void suoritaHakijaryhmaLaskentaHakemukselle(Hakukohde hakukohde, HakemusWrapper laskettavaHakemus, List<Hakemus> kaikkiHakemukset, Lukuarvofunktio lukuarvofunktio, Map<String, JonosijaJaSyotetytArvot> jonosijatHakemusOidinMukaan) {
        Laskentatulos<BigDecimal> tulos = laskentaService.suoritaValintalaskenta(hakukohde,
                laskettavaHakemus.getLaskentahakemus(), kaikkiHakemukset, lukuarvofunktio);

        HakemusDTO hakemus = laskettavaHakemus.getHakemusDTO();

        TilaJaSelite tilaJaSelite = hakijaryhmanTilaJaSelite(tulos);

        Jarjestyskriteeritulos jktulos = muodostaJarjestysKriteeritulos(tilaJaSelite, 0, "Hakijaryhmän tulokset", tulos.getTulos());

        if (!jonosijatHakemusOidinMukaan.containsKey(hakemus.getHakemusoid())) {
            Jonosija jonosija = muodostaJonosija(hakemus,
                    laskettavaHakemus.getHakutoiveprioriteetti(),
                    laskettavaHakemus.isHarkinnanvaraisuus());
            jonosijatHakemusOidinMukaan.put(hakemus.getHakemusoid(), new JonosijaJaSyotetytArvot(jonosija));
        }

        JonosijaJaSyotetytArvot jonosija = jonosijatHakemusOidinMukaan.get(hakemus.getHakemusoid());
        jonosija.getJonosija().getJarjestyskriteeritulokset().add(jktulos);
        jonosija.lisaaSyotetytArvot(tulos.getSyotetytArvot());
        jonosija.lisaaFunktioTulokset(tulos.getFunktioTulokset());

        Jarjestyskriteerihistoria jkhistoria = new Jarjestyskriteerihistoria();
        jkhistoria.setHistoria(tulos.getHistoria().toString());
        jarjestyskriteerihistoriaDAO.create(jkhistoria);
        jktulos.setHistoria(jkhistoria.getId());
    }

    @Override
    public void suoritaHakijaryhmaLaskentaHakemukselle(Hakukohde hakukohde, HakemusWrapper laskettavaHakemus, List<Hakemus> kaikkiHakemukset, Totuusarvofunktio totuusarvofunktio, Map<String, JonosijaJaSyotetytArvot> jonosijatHakemusOidinMukaan) {
        Laskentatulos<Boolean> tulos = laskentaService.suoritaValintalaskenta(hakukohde,
                laskettavaHakemus.getLaskentahakemus(), kaikkiHakemukset, totuusarvofunktio);

        HakemusDTO hakemus = laskettavaHakemus.getHakemusDTO();

        TilaJaSelite tilaJaSelite = hakijaryhmanTilaJaSelite(tulos);

        Jarjestyskriteeritulos jktulos = muodostaJarjestysKriteeritulos(tilaJaSelite, 0, "Hakijaryhmän tulokset", null);

        if (!jonosijatHakemusOidinMukaan.containsKey(hakemus.getHakemusoid())) {
            Jonosija jonosija = muodostaJonosija(hakemus,
                    laskettavaHakemus.getHakutoiveprioriteetti(),
                    laskettavaHakemus.isHarkinnanvaraisuus());
            jonosijatHakemusOidinMukaan.put(hakemus.getHakemusoid(), new JonosijaJaSyotetytArvot(jonosija));
        }

        JonosijaJaSyotetytArvot jonosija = jonosijatHakemusOidinMukaan.get(hakemus.getHakemusoid());
        jonosija.getJonosija().getJarjestyskriteeritulokset().add(jktulos);
        jonosija.lisaaSyotetytArvot(tulos.getSyotetytArvot());
        jonosija.lisaaFunktioTulokset(tulos.getFunktioTulokset());

        Jarjestyskriteerihistoria jkhistoria = new Jarjestyskriteerihistoria();
        jkhistoria.setHistoria(tulos.getHistoria().toString());
        jarjestyskriteerihistoriaDAO.create(jkhistoria);
        jktulos.setHistoria(jkhistoria.getId());
    }

    @Override
    public void suoritaLaskentaHakemukselle(Hakukohde hakukohde,
                                            HakemusWrapper laskettavaHakemus,
                                            List<Hakemus> kaikkiHakemukset,
                                            Lukuarvofunktio lukuarvofunktio,
                                            int jkPrioriteetti,
                                            Valinnanvaihe edellinenVaihe,
                                            Map<String, JonosijaJaSyotetytArvot> jonosijatHakemusOidinMukaan,
                                            String jkNimi) {
        Laskentatulos<BigDecimal> tulos = laskentaService.suoritaValintalaskenta(hakukohde,
                laskettavaHakemus.getLaskentahakemus(), kaikkiHakemukset, lukuarvofunktio);

        muodostaTulos(laskettavaHakemus, jkPrioriteetti, tulos, edellinenVaihe, jonosijatHakemusOidinMukaan, jkNimi);

    }

    @Override
    public void suoritaLaskentaHakemukselle(Hakukohde hakukohde,
                                            HakemusWrapper laskettavaHakemus,
                                            List<Hakemus> kaikkiHakemukset,
                                            Totuusarvofunktio lukuarvofunktio,
                                            int jkPrioriteetti,
                                            Valinnanvaihe edellinenVaihe,
                                            Map<String, JonosijaJaSyotetytArvot> jonosijatHakemusOidinMukaan,
                                            String jkNimi) {
        Laskentatulos<Boolean> tulos = laskentaService.suoritaValintalaskenta(hakukohde,
                laskettavaHakemus.getLaskentahakemus(), kaikkiHakemukset, lukuarvofunktio);

        muodostaTulos(laskettavaHakemus, jkPrioriteetti, tulos, edellinenVaihe, jonosijatHakemusOidinMukaan, jkNimi);

    }

    private void muodostaTulos(HakemusWrapper laskettavaHakemus,
                                            int jkPrioriteetti,
                                            Laskentatulos tulos,
                                            Valinnanvaihe edellinenVaihe,
                                            Map<String, JonosijaJaSyotetytArvot> jonosijatHakemusOidinMukaan,
                                            String jkNimi) {




        HakemusDTO hakemus = laskettavaHakemus.getHakemusDTO();

        TilaJaSelite tilaJaSelite =
                edellinenValinnanvaiheKasittelija.tilaEdellisenValinnanvaiheenMukaan(hakemus.getHakemusoid(),
                        tulos.getTila(), edellinenVaihe);


        TilaJaSelite edellinenTila = edellinenValinnanvaiheKasittelija.hakemusHyvaksyttavissaEdellisenValinnanvaiheenMukaan(hakemus.getHakemusoid(), edellinenVaihe);

        BigDecimal arvo;
        if(tilaJaSelite.getTila().equals(JarjestyskriteerituloksenTila.HYLATTY) &&
                (!tulos.getTila().getTilatyyppi().equals(Tila.Tilatyyppi.HYLATTY) ||
                        (tulos.getTila().getTilatyyppi().equals(Tila.Tilatyyppi.HYLATTY) && tilaJaSelite.getSelite().equals(edellinenTila.getSelite())))) {
            arvo = null;
        } else {
            if(tulos.getTulos() != null && tulos.getTulos() instanceof BigDecimal) {
                arvo = (BigDecimal)tulos.getTulos();
            } else {
                arvo = null;
            }
        }
        Jarjestyskriteeritulos jktulos = muodostaJarjestysKriteeritulos(tilaJaSelite, jkPrioriteetti, jkNimi, arvo);

        if (!jonosijatHakemusOidinMukaan.containsKey(hakemus.getHakemusoid())) {
            Jonosija jonosija = muodostaJonosija(hakemus,
                    laskettavaHakemus.getHakutoiveprioriteetti(),
                    laskettavaHakemus.isHarkinnanvaraisuus());
            jonosijatHakemusOidinMukaan.put(hakemus.getHakemusoid(), new JonosijaJaSyotetytArvot(jonosija));
        }

        JonosijaJaSyotetytArvot jonosija = jonosijatHakemusOidinMukaan.get(hakemus.getHakemusoid());
        jonosija.getJonosija().getJarjestyskriteeritulokset().add(jktulos);
        jonosija.lisaaSyotetytArvot(tulos.getSyotetytArvot());
        jonosija.lisaaFunktioTulokset(tulos.getFunktioTulokset());

        Jarjestyskriteerihistoria jkhistoria = new Jarjestyskriteerihistoria();
        jkhistoria.setHistoria(tulos.getHistoria().toString());
        jarjestyskriteerihistoriaDAO.create(jkhistoria);
        jktulos.setHistoria(jkhistoria.getId());

    }

    private Jarjestyskriteeritulos muodostaJarjestysKriteeritulos(TilaJaSelite tilaJaSelite, int prioriteetti, String nimi, BigDecimal tulos) {
        Jarjestyskriteeritulos jktulos = new Jarjestyskriteeritulos();
        jktulos.setPrioriteetti(prioriteetti);
        jktulos.setTila(tilaJaSelite.getTila());
        jktulos.setKuvaus(tilaJaSelite.getSelite());
        jktulos.setTekninenKuvaus(tilaJaSelite.getTekninenSelite());
        jktulos.setNimi(nimi);
        jktulos.setArvo(tulos);

        return jktulos;
    }

    private Jonosija muodostaJonosija(HakemusDTO hakemus, int prioriteetti, boolean harkinnanvaraisuus) {
        Jonosija jonosija = new Jonosija();
        jonosija.setEtunimi(hakemus.getEtunimi());
        jonosija.setHakemusOid(hakemus.getHakemusoid());
        jonosija.setHakijaOid(hakemus.getHakijaOid());
        jonosija.setHakutoiveprioriteetti(prioriteetti);
        jonosija.setHarkinnanvarainen(harkinnanvaraisuus);
        jonosija.setSukunimi(hakemus.getSukunimi());

        return jonosija;
    }
}
