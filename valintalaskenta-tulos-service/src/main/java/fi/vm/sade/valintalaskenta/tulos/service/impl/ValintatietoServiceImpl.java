package fi.vm.sade.valintalaskenta.tulos.service.impl;

import java.util.*;


import javax.jws.WebParam;

import fi.vm.sade.service.valintaperusteet.schema.TasasijasaantoTyyppi;
import fi.vm.sade.service.valintatiedot.ValintatietoService;
import fi.vm.sade.service.valintatiedot.schema.HakijaTyyppi;
import fi.vm.sade.service.valintatiedot.schema.HakukohdeTyyppi;
import fi.vm.sade.service.valintatiedot.schema.ValinnanvaiheTyyppi;
import fi.vm.sade.service.valintatiedot.schema.ValintatapajonoTyyppi;
import fi.vm.sade.valintalaskenta.domain.Hakukohde;
import fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.convert.ConversionService;

/**
 * Created with IntelliJ IDEA.
 * User: kkammone
 * Date: 29.4.2013
 * Time: 13:24
 * To change this template use File | Settings | File Templates.
 */
public class ValintatietoServiceImpl implements ValintatietoService {


    @Autowired
    private ValintalaskentaTulosService tulosService;

    @Autowired
    private ConversionService conversionService;

    @Override
    public List<HakukohdeTyyppi> haeValintatiedot(@WebParam(name = "hakuOid", targetNamespace = "") String hakuOid) {
        List<Hakukohde> a = tulosService.haeLasketutValinnanvaiheetHaulle(hakuOid);

        Map<String, HakukohdeTyyppi> hakukohdeMap = new HashMap<String, HakukohdeTyyppi>();

        for(Hakukohde v: a) {
            HakukohdeTyyppi ht = hakukohdeMap.get(v.getHakuoid());
            if(ht == null) {
                ht = new HakukohdeTyyppi();
                ht.setOid(v.getOid());
                ht.setHakukohteenHakuOid(v.getHakuoid());
                hakukohdeMap.put(v.getHakuoid(), ht);
            }
            ht.getValinnanvaihe().add(createValinnanvaiheTyyppi(v.getValinnanvaihe()));
        }
        return new ArrayList(hakukohdeMap.values());
    }

    private ValinnanvaiheTyyppi createValinnanvaiheTyyppi(Valinnanvaihe valinnanvaihe) {
        ValinnanvaiheTyyppi v  =new ValinnanvaiheTyyppi();
        v.setValinnanvaihe(valinnanvaihe.getJarjestysnumero());
        v.setValinnanvaiheOid(valinnanvaihe.getValinnanvaiheoid());
        for(fi.vm.sade.valintalaskenta.domain.Valintatapajono vt : valinnanvaihe.getValintatapajono())        {
            v.getValintatapajono().add(createValintatapajonoTyyppi(vt)) ;
        }
        return v;
    }

    private ValintatapajonoTyyppi createValintatapajonoTyyppi(fi.vm.sade.valintalaskenta.domain.Valintatapajono vt) {
        ValintatapajonoTyyppi valintatapajonoTyyppi = new ValintatapajonoTyyppi();
        valintatapajonoTyyppi.setOid(vt.getOid());
        //  valintatapajonoTyyppi.setKuvaus(vt.get);
        valintatapajonoTyyppi.setAloituspaikat(vt.getAloituspaikat());
        valintatapajonoTyyppi.setNimi(vt.getNimi());
        valintatapajonoTyyppi.setPrioriteetti(vt.getPrioriteetti());
        valintatapajonoTyyppi.setSiirretaanSijoitteluun(vt.isSiirretaanSijoitteluun());
        //  valintatapajonoTyyppi.setTasasijasaanto(vt.get);

        for(Jarjestyskriteeritulos a : vt.getJarjestyskriteeritulokset())       {
            HakijaTyyppi ht = new HakijaTyyppi();
            // ht.setPrioriteetti(a.getPrioriteetti());
            //    ht.setPisteet(a.getArvo());
            //  ht.setTila(a.getTila());
            ht.setOid(a.getHakemusoid());
            ht.setJonosija((int)a.getArvo());
            valintatapajonoTyyppi.getHakija().add(ht);
            valintatapajonoTyyppi.setTasasijasaanto(TasasijasaantoTyyppi.ARVONTA);
        }

        return valintatapajonoTyyppi;
    }


}

