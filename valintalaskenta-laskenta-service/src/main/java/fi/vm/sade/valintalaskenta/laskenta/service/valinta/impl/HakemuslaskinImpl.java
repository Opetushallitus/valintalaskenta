package fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl;

import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.valintaperusteet.laskenta.Lukuarvofunktio;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakemus;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakukohde;
import fi.vm.sade.service.valintaperusteet.laskenta.api.LaskentaService;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Laskentatulos;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Tila;
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

        Jarjestyskriteeritulos jktulos = new Jarjestyskriteeritulos();
        jktulos.setPrioriteetti(jkPrioriteetti);


        HakemusTyyppi hakemus = laskettavaHakemus.getHakemusTyyppi();
        TilaJaSelite tilaJaSelite =
                edellinenValinnanvaiheKasittelija.tilaEdellisenValinnanvaiheenMukaan(hakemus.getHakemusOid(),
                        tulos.getTila(), edellinenVaihe);

        // Jos hakija ei ole hyväksyttävissä edellisen valinnanvaiheen jäljiltä, niin tulosta ei aseteta
        if(tilaJaSelite.getTila().equals(JarjestyskriteerituloksenTila.HYLATTY) && !tulos.getTila().getTilatyyppi().equals(Tila.Tilatyyppi.HYLATTY)) {
            jktulos.setArvo(null);
        } else {
            jktulos.setArvo(tulos.getTulos());
        }

        jktulos.setTila(tilaJaSelite.getTila());
        jktulos.setKuvaus(tilaJaSelite.getSelite());
        jktulos.setTekninenKuvaus(tilaJaSelite.getTekninenSelite());
        jktulos.setNimi(jkNimi);

        if (!jonosijatHakemusOidinMukaan.containsKey(hakemus.getHakemusOid())) {
            Jonosija jonosija = new Jonosija();
            jonosija.setEtunimi(hakemus.getHakijanEtunimi());
            jonosija.setHakemusOid(hakemus.getHakemusOid());
            jonosija.setHakijaOid(hakemus.getHakijaOid());
            jonosija.setHakutoiveprioriteetti(laskettavaHakemus.getHakutoiveprioriteetti());
            jonosija.setHarkinnanvarainen(laskettavaHakemus.isHarkinnanvaraisuus());
            jonosija.setSukunimi(hakemus.getHakijanSukunimi());
            jonosijatHakemusOidinMukaan.put(hakemus.getHakemusOid(), new JonosijaJaSyotetytArvot(jonosija));
        }

        JonosijaJaSyotetytArvot jonosija = jonosijatHakemusOidinMukaan.get(hakemus.getHakemusOid());
        jonosija.getJonosija().getJarjestyskriteeritulokset().add(jktulos);
        jonosija.lisaaSyotetytArvot(tulos.getSyotetytArvot());
        jonosija.lisaaFunktioTulokset(tulos.getFunktioTulokset());

        Jarjestyskriteerihistoria jkhistoria = new Jarjestyskriteerihistoria();
        jkhistoria.setHistoria(tulos.getHistoria().toString());
        jarjestyskriteerihistoriaDAO.create(jkhistoria);
        jktulos.setHistoria(jkhistoria.getId());
    }
}
