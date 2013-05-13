package fi.vm.sade.valintalaskenta.tulos.service.impl;

import java.util.*;


import javax.jws.WebParam;

import fi.vm.sade.service.valintaperusteet.schema.TasasijasaantoTyyppi;
import fi.vm.sade.service.valintatiedot.ValintatietoService;
import fi.vm.sade.service.valintatiedot.schema.*;
import fi.vm.sade.valintalaskenta.domain.Hakukohde;
import fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.comparator.JonosijaDTOComparator;
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

     System.out.println("STARTTI" + hakuOid);

        List<Hakukohde> a = tulosService.haeLasketutValinnanvaiheetHaulle(hakuOid);

        System.out.println("hakukohteita: " + a.size());

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
        valintatapajonoTyyppi.setTasasijasaanto(TasasijasaantoTyyppi.ARVONTA);
        if(vt.getTasasijasaanto() != null) {
            valintatapajonoTyyppi.setTasasijasaanto(TasasijasaantoTyyppi.valueOf(vt.getTasasijasaanto().name()));
        }

        System.out.println("hakemuksia " + vt.getJarjestyskriteeritulokset().size());

        ArrayList<JonosijaDTO> jonosijat = new ArrayList<JonosijaDTO>();
        for(Jarjestyskriteeritulos a : vt.getJarjestyskriteeritulokset()) {
            JonosijaDTO dto = containsJonosijaDTO(jonosijat, a);
            if(dto == null) {
                System.out.println("Luodaan DTO");
                dto = new JonosijaDTO();
                dto.setHakemusOid(a.getHakemusoid());
                dto.setHakijaOid(a.getHakijaoid());
                dto.setTila (a.getTila());
                dto.setEtunimi(a.getEtunimi());
                dto.setSukunimi(a.getSukunimi());
                dto.setPrioriteetti(a.getPrioriteetti())  ;
                jonosijat.add(dto);
            }
            dto.getJarjestyskriteerit().put(a.getPrioriteetti(), a);
        }


        JonosijaDTOComparator comparator = new JonosijaDTOComparator();
       Collections.sort(jonosijat, comparator);

        int i = 1;
        JonosijaDTO previous = null;
        Iterator<JonosijaDTO> it = jonosijat.iterator();
        while(it.hasNext()) {
            JonosijaDTO dto = it.next();
            if(previous != null && comparator.compare(previous, dto) != 0) {
                i++;
            }
            dto.setJonosija(i);
            previous = dto;
        }

        System.out.println("Jonosijojen maara" + jonosijat.size());

      for(JonosijaDTO dto : jonosijat) {
          HakijaTyyppi ht = new HakijaTyyppi();
          ht.setPrioriteetti(dto.getPrioriteetti());
          //ht.setPisteet(dto.getArvo());

          if(dto.getTila() == null) {
              ht.setTila(HakemusTilaTyyppi.MAARITTELEMATON);
          }   else {
              ht.setTila(HakemusTilaTyyppi.valueOf(dto.getTila().name()));
          }

          ht.setHakemusOid(dto.getHakemusOid());
          ht.setOid(dto.getHakijaOid());
          ht.setJonosija(dto.getJonosija());
          valintatapajonoTyyppi.getHakija().add(ht);
      }
        System.out.println("HERE WE ARE");

       for(HakijaTyyppi ht : valintatapajonoTyyppi.getHakija()) {
           System.out.println("HAKIJA: " + ht.getHakemusOid() + ht.getOid());

       }

        return valintatapajonoTyyppi;
    }


    private JonosijaDTO containsJonosijaDTO(ArrayList<JonosijaDTO> jonosijat, Jarjestyskriteeritulos a)  {
        for(JonosijaDTO dto : jonosijat) {
            if(  (StringUtils.isNotBlank(a.getHakemusoid()) && a.getHakemusoid().equals(dto.getHakemusOid()))
                    ||
                    (StringUtils.isNotBlank(a.getHakijaoid()) && a.getHakijaoid().equals(dto.getHakijaOid()))
                    ) {
                return dto;
            }
        }
        return null;
    }

}

