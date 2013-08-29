package fi.vm.sade.valintalaskenta.tulos.service.impl.converters;

import fi.vm.sade.valintalaskenta.domain.Hakukohde;
import fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.Jonosija;
import fi.vm.sade.valintalaskenta.domain.Valintatapajono;
import fi.vm.sade.valintalaskenta.domain.comparator.JonosijaDTOComparator;
import fi.vm.sade.valintalaskenta.domain.dto.HakukohdeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.JarjestyskriteeritulosDTO;
import fi.vm.sade.valintalaskenta.domain.dto.JonosijaDTO;
import fi.vm.sade.valintalaskenta.domain.dto.ValintatapajonoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.*;
import fi.vm.sade.valintalaskenta.domain.valintakoe.*;
import org.springframework.stereotype.Component;

import java.util.*;

/**
 * User: kkammone
 * Date: 1.8.2013
 * Time: 12:48
 */

@Component
public class ValintatulosConverterImpl implements ValintatulosConverter {

    @Override
    public List<HakukohdeDTO> convertHakukohde(List<Hakukohde> a) {
        Map<String, HakukohdeDTO> hakukohdeMap = new HashMap<String, HakukohdeDTO>();

        for (Hakukohde v : a) {
            HakukohdeDTO ht = hakukohdeMap.get(v.getOid());
            if (ht == null) {
                ht = new HakukohdeDTO();
                ht.setTarjoajaoid(v.getTarjoajaoid());
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
    public List<fi.vm.sade.valintalaskenta.domain.dto.ValinnanvaiheDTO> convertValinnanvaiheList(List<fi.vm.sade.valintalaskenta.domain.Valinnanvaihe> valinnanVaiheList) {
        List<fi.vm.sade.valintalaskenta.domain.dto.ValinnanvaiheDTO> list = new ArrayList<fi.vm.sade.valintalaskenta.domain.dto.ValinnanvaiheDTO>();
        if (valinnanVaiheList == null || valinnanVaiheList.isEmpty()) {
            return list;
        }
        for (fi.vm.sade.valintalaskenta.domain.Valinnanvaihe valinnanVaihe : valinnanVaiheList) {
            list.add(convertValinnanvaihe(valinnanVaihe));
        }
        return list;
    }

    @Override
    public List<ValintakoeOsallistuminenDTO> convertValintakoeOsallistuminen(List<ValintakoeOsallistuminen> osallistumiset) {
        List<ValintakoeOsallistuminenDTO> dtot = new ArrayList<ValintakoeOsallistuminenDTO>();

        for (ValintakoeOsallistuminen vko : osallistumiset) {
            ValintakoeOsallistuminenDTO dto = new ValintakoeOsallistuminenDTO();

            dto.setCreatedAt(vko.getCreatedAt());
            dto.setEtunimi(vko.getEtunimi());
            dto.setHakemusOid(vko.getHakemusOid());
            dto.setHakijaOid(vko.getHakijaOid());
            dto.setHakuOid(vko.getHakuOid());
            dto.setSukunimi(vko.getSukunimi());
            dto.setHakutoiveet(convertHakutoive(vko.getHakutoiveet()));

            dtot.add(dto);
        }

        return dtot;
    }

    @Override
    public List<HakutoiveDTO> convertHakutoive(List<Hakutoive> hakutoiveet) {
        List<HakutoiveDTO> dtot = new ArrayList<HakutoiveDTO>();

        for (Hakutoive ht : hakutoiveet) {
            HakutoiveDTO dto = new HakutoiveDTO();
            dto.setHakukohdeOid(ht.getHakukohdeOid());
            dto.setValinnanVaiheet(convertValinnanVaihe(ht.getValinnanVaiheet()));
            dtot.add(dto);
        }

        return dtot;
    }

    @Override
    public List<ValinnanvaiheDTO> convertValinnanVaihe(List<Valinnanvaihe> valinnanVaiheet) {
        List<ValinnanvaiheDTO> dtot = new ArrayList<ValinnanvaiheDTO>();

        for (Valinnanvaihe vv : valinnanVaiheet) {
            ValinnanvaiheDTO dto = new ValinnanvaiheDTO();
            dto.setValinnanVaiheJarjestysluku(vv.getValinnanVaiheJarjestysluku());
            dto.setValinnanVaiheOid(vv.getValinnanVaiheOid());
            dto.setValintakokeet(convertValintakoe(vv.getValintakokeet()));

            dtot.add(dto);
        }

        return dtot;
    }

    @Override
    public List<ValintakoeDTO> convertValintakoe(List<Valintakoe> valintakokeet) {
        List<ValintakoeDTO> dtot = new ArrayList<ValintakoeDTO>();

        for (Valintakoe koe : valintakokeet) {
            ValintakoeDTO dto = new ValintakoeDTO();
            dto.setValintakoeOid(koe.getValintakoeOid());
            dto.setValintakoeTunniste(koe.getValintakoeTunniste());
            dto.setOsallistuminenTulos(convertOsallistuminenTulos(koe.getOsallistuminenTulos()));

            dtot.add(dto);
        }

        return dtot;
    }

    @Override
    public OsallistuminenTulosDTO convertOsallistuminenTulos(OsallistuminenTulos osallistuminenTulos) {
        OsallistuminenTulosDTO dto = new OsallistuminenTulosDTO();
        dto.setKuvaus(osallistuminenTulos.getKuvaus());
        dto.setLaskentaTila(osallistuminenTulos.getLaskentaTila());
        dto.setLaskentaTulos(osallistuminenTulos.getLaskentaTulos());
        dto.setOsallistuminen(osallistuminenTulos.getOsallistuminen());

        return dto;
    }

    public fi.vm.sade.valintalaskenta.domain.dto.ValinnanvaiheDTO convertValinnanvaihe(fi.vm.sade.valintalaskenta.domain.Valinnanvaihe valinnanvaihe) {
        fi.vm.sade.valintalaskenta.domain.dto.ValinnanvaiheDTO dto = new fi.vm.sade.valintalaskenta.domain.dto.ValinnanvaiheDTO();
        dto.setCreatedAt(valinnanvaihe.getCreatedAt());
        dto.setJarjestysnumero(valinnanvaihe.getJarjestysnumero());
        dto.setValinnanvaiheoid(valinnanvaihe.getValinnanvaiheoid());
        dto.setValintatapajono(convertValintatapajono(valinnanvaihe.getValintatapajono()));
        return dto;
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
    public List<JonosijaDTO> convertJonosija(Collection<Jonosija> jonosijat) {
        List<JonosijaDTO> list = new ArrayList<JonosijaDTO>();
        if (jonosijat == null || jonosijat.isEmpty()) {
            return list;
        }
        for (Jonosija jonosija : jonosijat) {
            JonosijaDTO dto = new JonosijaDTO();
            dto.setEtunimi(jonosija.getEtunimi());
            dto.setHakemusOid(jonosija.getHakemusoid());
            dto.setHakijaOid(jonosija.getHakijaoid());
            dto.setPrioriteetti(jonosija.getPrioriteetti());
            dto.setSukunimi(jonosija.getSukunimi());
            // dto.setHistoriat(jonosija.getHistoriat());

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
    public void sort(List<JonosijaDTO> list) {

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
                // System.out.println("===JONOSIJA===") ;
                // System.out.println("EKA: "+jonosija.getJarjestyskriteerit().get(0).getTila()
                // ) ;
                // for(Integer b : jonosija.getJarjestyskriteerit().keySet()) {
                // System.out.println("[" + b +"]" +
                // jonosija.getJarjestyskriteerit().get(b).getTila() );
                // }
                // System.out.println("==============") ;

                jonosija.setTuloksenTila(jonosija.getJarjestyskriteerit().get(0).getTila());
            }

            previous = jonosija;
        }
    }

}
