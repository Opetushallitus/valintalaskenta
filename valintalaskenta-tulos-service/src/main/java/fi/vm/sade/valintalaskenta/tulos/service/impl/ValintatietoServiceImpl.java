package fi.vm.sade.valintalaskenta.tulos.service.impl;

import java.util.*;


import javax.jws.WebParam;

import fi.vm.sade.service.valintaperusteet.schema.TasasijasaantoTyyppi;
import fi.vm.sade.service.valintatiedot.ValintatietoService;
import fi.vm.sade.service.valintatiedot.schema.*;
import fi.vm.sade.valintalaskenta.domain.Hakukohde;
import fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.Jonosija;
import fi.vm.sade.valintalaskenta.domain.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.comparator.JonosijaComparator;
import fi.vm.sade.valintalaskenta.domain.dto.JonosijaDTO;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import org.apache.commons.lang.StringUtils;
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
        if(vt.getTasasijasaanto() != null) {
            valintatapajonoTyyppi.setTasasijasaanto(TasasijasaantoTyyppi.valueOf(vt.getTasasijasaanto().name()));
        }

        List<Jonosija> jonosijat = vt.getJonosijat();

       JonosijaComparator comparator = new JonosijaComparator();
       Collections.sort(jonosijat, comparator);

        int i = 1;
        Jonosija previous = null;
        Iterator<Jonosija> it = jonosijat.iterator();
        while(it.hasNext()) {
            Jonosija dto = it.next();
            if(previous != null && comparator.compare(previous, dto) != 0) {
                i++;
            }
            dto.setJonosija(i);
            previous = dto;
        }


      for(Jonosija dto : jonosijat) {
          HakijaTyyppi ht = new HakijaTyyppi();
          ht.setPrioriteetti(dto.getPrioriteetti());
          //ht.setPisteet(dto.getArvo());

          if(dto.getTuloksenTila() == null) {
              ht.setTila(HakemusTilaTyyppi.MAARITTELEMATON);
          }   else {
              ht.setTila(HakemusTilaTyyppi.valueOf(dto.getTuloksenTila().name()));
          }

          ht.setHakemusOid(dto.getHakemusoid());
          ht.setOid(dto.getHakijaoid());
          ht.setJonosija(dto.getJonosija());
          valintatapajonoTyyppi.getHakija().add(ht);
      }

        /*
       for(HakijaTyyppi ht : valintatapajonoTyyppi.getHakija()) {
           System.out.println("HAKIJA: " + ht.getHakemusOid() + ht.getOid());

       }
          */

        return valintatapajonoTyyppi;
    }


}

