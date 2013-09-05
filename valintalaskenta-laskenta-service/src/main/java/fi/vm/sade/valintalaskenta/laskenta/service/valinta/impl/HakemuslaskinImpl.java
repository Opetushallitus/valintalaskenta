package fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl;

import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.valintaperusteet.laskenta.Lukuarvofunktio;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakemus;
import fi.vm.sade.service.valintaperusteet.laskenta.api.LaskentaService;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Laskentatulos;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.laskenta.dao.JarjestyskriteerihistoriaDAO;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.Hakemuslaskin;
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
public class HakemuslaskinImpl implements Hakemuslaskin {

    @Autowired
    private LaskentaService laskentaService;

    @Autowired
    private JarjestyskriteerihistoriaDAO jarjestyskriteerihistoriaDAO;

    @Autowired
    private EdellinenValinnanvaiheKasittelija edellinenValinnanvaiheKasittelija;

    @Override
    public void suoritaLaskentaHakemukselle(String hakukohdeOid,
                                            HakemusWrapper laskettavaHakemus,
                                            List<Hakemus> kaikkiHakemukset,
                                            Lukuarvofunktio lukuarvofunktio,
                                            Jarjestyskriteeritulos jktulos,
                                            Valinnanvaihe edellinenVaihe,
                                            Map<String, Jonosija> jonosijatHakemusOidinMukaan) {
        StringBuffer historia = new StringBuffer();
        Laskentatulos<BigDecimal> tulos = laskentaService.suoritaLasku(hakukohdeOid,
                laskettavaHakemus.getLaskentahakemus(), kaikkiHakemukset, lukuarvofunktio, historia);

        jktulos.setArvo(tulos.getTulos());

        HakemusTyyppi hakemus = laskettavaHakemus.getHakemusTyyppi();
        TilaJaSelite tilaJaSelite =
                edellinenValinnanvaiheKasittelija.tilaEdellisenValinnanvaiheenMukaan(hakemus.getHakemusOid(),
                        tulos.getTila(), edellinenVaihe);

        jktulos.setTila(tilaJaSelite.getTila());
        jktulos.setKuvaus(tilaJaSelite.getSelite());

        if (!jonosijatHakemusOidinMukaan.containsKey(hakemus.getHakemusOid())) {
            Jonosija jonosija = new Jonosija();
            jonosija.setEtunimi(hakemus.getHakijanEtunimi());
            jonosija.setHakemusOid(hakemus.getHakemusOid());
            jonosija.setHakijaOid(hakemus.getHakijaOid());
            jonosija.setHakutoiveprioriteetti(laskettavaHakemus.getHakutoiveprioriteetti());
            jonosija.setHarkinnanvarainen(laskettavaHakemus.isHarkinnanvaraisuus());
            jonosija.setSukunimi(hakemus.getHakijanSukunimi());
            jonosijatHakemusOidinMukaan.put(hakemus.getHakemusOid(), jonosija);
        }

        Jonosija jonosija = jonosijatHakemusOidinMukaan.get(hakemus.getHakemusOid());
        jonosija.getJarjestyskriteeritulokset().add(jktulos);

        Jarjestyskriteerihistoria jkhistoria = new Jarjestyskriteerihistoria();
        jkhistoria.setHistoria(historia.toString());
        jarjestyskriteerihistoriaDAO.create(jkhistoria);
        jktulos.setHistoria(jkhistoria.getId());
    }
}
