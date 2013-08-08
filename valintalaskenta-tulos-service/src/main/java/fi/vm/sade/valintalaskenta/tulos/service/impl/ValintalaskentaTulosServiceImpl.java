package fi.vm.sade.valintalaskenta.tulos.service.impl;

import fi.vm.sade.valintalaskenta.domain.*;
import fi.vm.sade.valintalaskenta.domain.dto.*;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.tulos.dao.*;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import fi.vm.sade.valintalaskenta.tulos.service.impl.converters.ValintatulosConverter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.List;

/**
 * @author Jussi Jartamo
 */
@Service
public class ValintalaskentaTulosServiceImpl implements ValintalaskentaTulosService {

    private static final Logger LOGGER = LoggerFactory.getLogger(ValintalaskentaTulosServiceImpl.class);

    @Autowired
    private HakukohdeDAO hakukohdeDAO;

//    @Autowired
    //  private JarjestyskriteeritulosDAO jarjestyskriteeritulosDAO;

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

    private void applyMuokatutJonosijatToValinannvaihe(String hakukohdeoid, List<ValinnanvaiheDTO> b) {
        List<MuokattuJonosija> a = muokattuJonosijaDAO.readByhakukohdeOid(hakukohdeoid);
        applyMuokatutJonosijat(b, a);
    }
    private void applyMuokatutJonosijatToHakukohde(String hakuOid, List<HakukohdeDTO> b) {
        List<MuokattuJonosija> a = muokattuJonosijaDAO.readByHakuOid(hakuOid);
        for(HakukohdeDTO hakukohde : b) {
            applyMuokatutJonosijat(hakukohde.getValinnanvaihe(),a);
        }
    }

    private void applyMuokatutJonosijat(List<ValinnanvaiheDTO> b, List<MuokattuJonosija> a) {
        for(ValinnanvaiheDTO dto : b) {
            for(ValintatapajonoDTO valintatapajonoDTO :  dto.getValintatapajono()) {
                for(JonosijaDTO jonosija : valintatapajonoDTO.getJonosijat()) {
                    for(MuokattuJonosija muokattuJonosija : a) {
                        if(muokattuJonosija.getHakemusOid().equals(jonosija.getHakemusOid()) && valintatapajonoDTO.getOid().equals(muokattuJonosija.getValintatapajonoOid())) {
                            applyJonosija(jonosija, muokattuJonosija);
                        }
                    }
                }
                valintatulosConverter.sort(valintatapajonoDTO);
            }
        }
    }

    private void applyJonosija(JonosijaDTO jonosijaDTO, MuokattuJonosija muokattuJonosija){
        boolean jonosijaMuokattu = false;

        if(muokattuJonosija.getHarkinnanvarainen() != null) {
            jonosijaDTO.setHarkinnanvarainen(muokattuJonosija.getHarkinnanvarainen());
            jonosijaMuokattu = true;
        }
        if(muokattuJonosija.getPrioriteetti() != null) {
            jonosijaDTO.setPrioriteetti(muokattuJonosija.getPrioriteetti());
            jonosijaMuokattu = true;
        }

        for(Integer i : muokattuJonosija.getJarjestyskriteerit().keySet()) {
            Jarjestyskriteeritulos muokattuJarjestyskriteeritulos = muokattuJonosija.getJarjestyskriteerit().get(i);

            JarjestyskriteeritulosDTO alkuperainenJarjestyskriteeritulosDTO = jonosijaDTO.getJarjestyskriteerit().get(i);
            if(alkuperainenJarjestyskriteeritulosDTO == null ) {
                alkuperainenJarjestyskriteeritulosDTO = new JarjestyskriteeritulosDTO();
                jonosijaDTO.getJarjestyskriteerit().put(i, alkuperainenJarjestyskriteeritulosDTO);
            }
            if(muokattuJarjestyskriteeritulos.getArvo() != null) {
                alkuperainenJarjestyskriteeritulosDTO.setArvo(muokattuJarjestyskriteeritulos.getArvo());
                jonosijaMuokattu = true;
            }
            if(muokattuJarjestyskriteeritulos.getKuvaus() != null) {
                alkuperainenJarjestyskriteeritulosDTO.setKuvaus(muokattuJarjestyskriteeritulos.getKuvaus());
                jonosijaMuokattu = true;
            }
            if(muokattuJarjestyskriteeritulos.getTila() != null) {
                alkuperainenJarjestyskriteeritulosDTO.setTila(muokattuJarjestyskriteeritulos.getTila());
                jonosijaMuokattu = true;
            }
        }
        if(jonosijaMuokattu) {
            jonosijaDTO.setMuokattu(true);
        }
    }


    @Override
    public List<ValinnanvaiheDTO> haeValinnanvaiheetHakukohteelle(String hakukohdeoid) {
        List<Valinnanvaihe> a = valinnanvaiheDAO.readByHakukohdeOid(hakukohdeoid);
        List<ValinnanvaiheDTO> b = valintatulosConverter.convertValinnanvaiheList(a);
        applyMuokatutJonosijatToValinannvaihe(hakukohdeoid, b);
        return b;

    }

    @Override
    public List<HakukohdeDTO> haeLasketutValinnanvaiheetHaulle(String hakuOid) {
        List<Hakukohde> a = valinnanvaiheDAO.readByHakuOid(hakuOid);
        List<HakukohdeDTO> b = valintatulosConverter.convertHakukohde(a);
        applyMuokatutJonosijatToHakukohde(hakuOid, b);
        return b;
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
        VersiohallintaHakukohde hakukohde =  hakukohdeDAO.findByValintatapajono(valintatapajono);

        MuokattuJonosija muokattuJonosija;
        muokattuJonosija = muokattuJonosijaDAO.readByValintatapajonoOid(valintatapajonoOid, hakemusOid);
        if(muokattuJonosija == null) {
            muokattuJonosija = new MuokattuJonosija();
        }
        muokattuJonosija.setHakemusOid(hakemusOid);
        muokattuJonosija.setValintatapajonoOid(valintatapajonoOid);
        muokattuJonosija.setHakuOid(hakukohde.getHakuoid());
        muokattuJonosija.setHakukohdeOid(hakukohde.getHakukohdeoid());

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
        VersiohallintaHakukohde hakukohde =  hakukohdeDAO.findByValintatapajono(valintatapajono);

        MuokattuJonosija muokattuJonosija;
        muokattuJonosija = muokattuJonosijaDAO.readByValintatapajonoOid(valintatapajonoOid, hakemusOid);
        if(muokattuJonosija == null) {
            muokattuJonosija = new MuokattuJonosija();
        }
        muokattuJonosija.setHakemusOid(hakemusOid);
        muokattuJonosija.setValintatapajonoOid(valintatapajonoOid);
        muokattuJonosija.setHakuOid(hakukohde.getHakuoid());
        muokattuJonosija.setHakukohdeOid(hakukohde.getHakukohdeoid());

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
