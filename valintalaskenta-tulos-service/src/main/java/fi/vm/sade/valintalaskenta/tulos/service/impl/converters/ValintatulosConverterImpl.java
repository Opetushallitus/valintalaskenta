package fi.vm.sade.valintalaskenta.tulos.service.impl.converters;

import fi.vm.sade.valintalaskenta.domain.*;
import fi.vm.sade.valintalaskenta.domain.comparator.JonosijaDTOComparator;
import fi.vm.sade.valintalaskenta.domain.dto.*;
import org.springframework.stereotype.Component;

import java.util.*;

/**
 * Created with IntelliJ IDEA. User: kkammone Date: 1.8.2013 Time: 12:48 To
 * change this template use File | Settings | File Templates.
 */

@Component
public class ValintatulosConverterImpl implements ValintatulosConverter{

    @Override
    public List<HakukohdeDTO> convertHakukohde(List<Hakukohde> a) {
        Map<String, HakukohdeDTO> hakukohdeMap = new HashMap<String, HakukohdeDTO>();

        for (Hakukohde v : a) {
            HakukohdeDTO ht = hakukohdeMap.get(v.getOid());
            if (ht == null) {
                ht = new HakukohdeDTO();
                ht.setOid(v.getOid());
                ht.setHakuoid(v.getHakuoid());
                ht.setCreatedAt(v.getCreatedAt());
                hakukohdeMap.put(v.getOid(), ht);
            }
            ht.getValinnanvaihe().add(convertValinnanvaihe(v.getValinnanvaihe()));
        }
        List<HakukohdeDTO> list = new ArrayList<HakukohdeDTO>();
        list.addAll(hakukohdeMap.values());
        return list;
    }

    @Override
    public List<ValinnanvaiheDTO> convertValinnanvaiheList(List<Valinnanvaihe> valinnanVaiheList) {
        List<ValinnanvaiheDTO> list = new ArrayList<ValinnanvaiheDTO>();
        if (valinnanVaiheList == null || valinnanVaiheList.isEmpty()) {
            return list;
        }
        for (Valinnanvaihe valinnanVaihe : valinnanVaiheList) {
            list.add( convertValinnanvaihe(valinnanVaihe));
        }
        return list;
    }

    public ValinnanvaiheDTO convertValinnanvaihe(Valinnanvaihe valinnanvaihe){
        ValinnanvaiheDTO dto = new ValinnanvaiheDTO();
        dto.setCreatedAt(valinnanvaihe.getCreatedAt());
        dto.setJarjestysnumero(valinnanvaihe.getJarjestysnumero());
        dto.setValinnanvaiheoid(valinnanvaihe.getValinnanvaiheoid());
        dto.setValintatapajono(convertValintatapajono(valinnanvaihe.getValintatapajono()));
        return  dto;
    }
    @Override
    public List<ValintatapajonoDTO> convertValintatapajono(List<Valintatapajono> valintapajonoList) {
        List<ValintatapajonoDTO> list = new ArrayList<ValintatapajonoDTO>();
        if (valintapajonoList == null || valintapajonoList.isEmpty()) {
            return list;
        }
        for (Valintatapajono valintatapajono : valintapajonoList) {
            ValintatapajonoDTO dto = new ValintatapajonoDTO();
            dto.setAloituspaikat(valintatapajono.getAloituspaikat());
            dto.setEiVarasijatayttoa(valintatapajono.getEiVarasijatayttoa());
            dto.setJonosijat(convertJonosija(valintatapajono.getJonosijat()));
            dto.setNimi(valintatapajono.getNimi());
            dto.setOid(valintatapajono.getOid());
            dto.setPrioriteetti(valintatapajono.getPrioriteetti());
            dto.setTasasijasaanto(valintatapajono.getTasasijasaanto());
            dto.setVersio(valintatapajono.getVersio());
            dto.setSiirretaanSijoitteluun(valintatapajono.isSiirretaanSijoitteluun());
            list.add(dto);
        }
        return list;
    }
    @Override
    public List<JonosijaDTO> convertJonosija(List<Jonosija> jonosijat) {
        List<JonosijaDTO> list = new ArrayList<JonosijaDTO>();
        if (jonosijat == null || jonosijat.isEmpty()) {
            return list;
        }
        for (Jonosija jonosija : jonosijat) {
            JonosijaDTO dto = new JonosijaDTO();
            dto.setEtunimi(jonosija.getEtunimi());
            dto.setHakemusOid(jonosija.getHakemusoid());
            dto.setHakijaOid(jonosija.getHakemusoid());
            dto.setPrioriteetti(jonosija.getPrioriteetti());
            dto.setSukunimi(jonosija.getSukunimi());
            //dto.setHistoriat(jonosija.getHistoriat());

            for (Integer i : jonosija.getJarjestyskriteerit().keySet()) {
                Jarjestyskriteeritulos jktulos = jonosija.getJarjestyskriteerit().get(i);
                JarjestyskriteeritulosDTO jdto = new JarjestyskriteeritulosDTO();
                jdto.setArvo(jktulos.getArvo());
                jdto.setKuvaus(jktulos.getKuvaus());
                jdto.setTila(jktulos.getTila());

                dto.getJarjestyskriteerit().put(i, jdto);
            }
            list.add(dto);
        }


        return list;
    }
    @Override
    public void sort( List<JonosijaDTO> list) {


        JonosijaDTOComparator comparator = new JonosijaDTOComparator();
        Collections.sort(list, comparator);

        int i = 1;
        JonosijaDTO previous = null;
        Iterator<JonosijaDTO> it = list.iterator();
        while (it.hasNext()) {
            JonosijaDTO jonosija = it.next();

            if (previous != null && comparator.compare(previous, jonosija) != 0) {
                i++;
            }
            jonosija.setJonosija(i);

            if (jonosija.getJarjestyskriteerit().get(0) != null) {
                //System.out.println("===JONOSIJA===")         ;
                //     System.out.println("EKA: "+jonosija.getJarjestyskriteerit().get(0).getTila() )         ;
                //   for(Integer b : jonosija.getJarjestyskriteerit().keySet()) {
                //        System.out.println("[" + b +"]" + jonosija.getJarjestyskriteerit().get(b).getTila() );
                //    }
                //   System.out.println("==============")         ;

                jonosija.setTuloksenTila(jonosija.getJarjestyskriteerit().get(0).getTila());
            }

            previous = jonosija;
        }
    }

}
