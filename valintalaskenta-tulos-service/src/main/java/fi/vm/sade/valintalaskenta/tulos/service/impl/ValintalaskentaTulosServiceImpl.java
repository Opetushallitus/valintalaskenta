package fi.vm.sade.valintalaskenta.tulos.service.impl;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import fi.vm.sade.service.valintatiedot.schema.HakuTyyppi;
import fi.vm.sade.service.valintatiedot.schema.HakukohdeTyyppi;
import fi.vm.sade.valintalaskenta.domain.*;
import fi.vm.sade.valintalaskenta.domain.dto.HakukohdeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.ValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.tulos.dao.*;
import fi.vm.sade.valintalaskenta.tulos.service.impl.converters.ValintatulosConverter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import fi.vm.sade.valintalaskenta.tulos.service.exception.ValintatapajonoEiOleOlemassaException;

/**
 * @author Jussi Jartamo
 */
@Service
public class ValintalaskentaTulosServiceImpl implements ValintalaskentaTulosService {

    private static final Logger LOGGER = LoggerFactory.getLogger(ValintalaskentaTulosServiceImpl.class);

    @Autowired
    private HakukohdeDAO hakukohdeDAO;

    @Autowired
    private JarjestyskriteeritulosDAO jarjestyskriteeritulosDAO;

    @Autowired
    private ValinnanvaiheDAO valinnanvaiheDAO;

    @Autowired
    private ValintatapajonoDAO valintatapajonoDAO;

    @Autowired
    private ValintakoeOsallistuminenDAO valintakoeOsallistuminenDAO;

    @Autowired
    private MuokattuJonosijaDAO muokattuJonosijaDAO;

    @Autowired
    private ValintatulosConverter valintatulosConverter;


    /*
    public List<Versioituhakukohde> haeHakukohteet() {
        return hakukohdeDAO.readAll();
    }

    public List<Versioituhakukohde> haeHakukohteetHaulle(String hakuoid) {
        return hakukohdeDAO.readByHakuOid(hakuoid);
    }

    @Override
    public List<Jonosija> haeJarjestyskriteerituloksetValintatapajonolle(String valintatapajonooid) {
        return jarjestyskriteeritulosDAO.readByValintatapajonoOid(valintatapajonooid);
    }
     */
    /*
    @Override
    public List<ValintakoeOsallistuminen> haeValintakoeOsallistumiset() {
        return valintakoeOsallistuminenDAO.findAll();
    }
      */


    /*
    @Override
    public List<Valintatapajono> haeValintatapajonoValinnanvaiheelle(String valinnanvaiheoid) {
        return valintatapajonoDAO.readByValinnanvaiheOid(valinnanvaiheoid);
    }
      */

    @Override
    public List<ValinnanvaiheDTO> haeValinnanvaiheetHakukohteelle(String hakukohdeoid) {
        List<Valinnanvaihe> a = valinnanvaiheDAO.readByHakukohdeOid(hakukohdeoid);
        System.out.println("AAAAAAAAAAAAAAAAAAAAAAAAA:n koko" + a.size());

        List<ValinnanvaiheDTO> b = valintatulosConverter.jarjestaValinnanvaiheJaLisaaJonosijaNumero(a);
        System.out.println("BEEEEEEEEEEEEEE:n koko" + b.size());
        return b;
    }

    @Override
    public List<HakukohdeDTO> haeLasketutValinnanvaiheetHaulle(String hakuOid) {
        List<Hakukohde> a = valinnanvaiheDAO.readByHakuOid(hakuOid);
        return valintatulosConverter.jarjestaHakukohteetJaLisaaJonosijaNumero(a);

    }

    @Override
    public List<ValintakoeOsallistuminen> haeValintakoeOsallistumiset(String hakijaoid) {
        return valintakoeOsallistuminenDAO.findByHakijaOid(hakijaoid);
    }

    @Override
    public List<ValintakoeOsallistuminen> haeValintakoeOsallistumisetByHakutoive(String hakukohdeOid) {
        return valintakoeOsallistuminenDAO.findByHakutoive(hakukohdeOid);
    }

    @Override
    public MuokattuJonosija muutaJarjestyskriteerinArvo(String valintatapajonoOid,
                                                        String hakemusOid,
                                                        Integer jarjestyskriteeriPrioriteetti,
                                                        BigDecimal arvo) {

        Valintatapajono valintatapajono = valintatapajonoDAO.findByOid(valintatapajonoOid);
        Hakukohde hakukohde =  hakukohdeDAO.findByValintatapajono(valintatapajono);
        String hakuOid = hakukohde.getHakuoid();

        MuokattuJonosija muokattuJonosija;
        muokattuJonosija = muokattuJonosijaDAO.readByValintatapajonoOid(valintatapajonoOid, hakemusOid);
        if(muokattuJonosija == null) {
            muokattuJonosija = new MuokattuJonosija();
        }
        muokattuJonosija.setHakemusOid(hakemusOid);
        muokattuJonosija.setValintatapajonoOid(valintatapajonoOid);
        muokattuJonosija.setHakuOid(hakuOid);

        Jarjestyskriteeritulos jarjestyskriteeritulos = muokattuJonosija.getJarjestyskriteerit().get(jarjestyskriteeriPrioriteetti);;
        if(jarjestyskriteeritulos == null) {
            jarjestyskriteeritulos = new Jarjestyskriteeritulos();
            muokattuJonosija.getJarjestyskriteerit().put(jarjestyskriteeriPrioriteetti, jarjestyskriteeritulos);
        }
        jarjestyskriteeritulos.setKuvaus("Muokattu käsin");
        jarjestyskriteeritulos.setArvo(arvo);

        muokattuJonosijaDAO.saveOrUpdate(muokattuJonosija);

        return muokattuJonosija;
    }

    @Override
    public MuokattuJonosija muutaJarjestyskriteerinTila(String valintatapajonoOid, String hakemusOid, Integer jarjestyskriteeriPrioriteetti, JarjestyskriteerituloksenTila arvo) {

        Valintatapajono valintatapajono = valintatapajonoDAO.findByOid(valintatapajonoOid);
        Hakukohde hakukohde =  hakukohdeDAO.findByValintatapajono(valintatapajono);
        String hakuOid = hakukohde.getHakuoid();

        MuokattuJonosija muokattuJonosija;
        muokattuJonosija = muokattuJonosijaDAO.readByValintatapajonoOid(valintatapajonoOid, hakemusOid);
        if(muokattuJonosija == null) {
            muokattuJonosija = new MuokattuJonosija();
        }
        muokattuJonosija.setHakemusOid(hakemusOid);
        muokattuJonosija.setValintatapajonoOid(valintatapajonoOid);
        muokattuJonosija.setHakuOid(hakuOid);

        Jarjestyskriteeritulos jarjestyskriteeritulos = muokattuJonosija.getJarjestyskriteerit().get(jarjestyskriteeriPrioriteetti);;
        if(jarjestyskriteeritulos == null) {
            jarjestyskriteeritulos = new Jarjestyskriteeritulos();
            muokattuJonosija.getJarjestyskriteerit().put(jarjestyskriteeriPrioriteetti, jarjestyskriteeritulos);
        }
        jarjestyskriteeritulos.setKuvaus("Muokattu käsin");
        jarjestyskriteeritulos.setTila(arvo);

        muokattuJonosijaDAO.saveOrUpdate(muokattuJonosija);

        return muokattuJonosija;
    }




}
