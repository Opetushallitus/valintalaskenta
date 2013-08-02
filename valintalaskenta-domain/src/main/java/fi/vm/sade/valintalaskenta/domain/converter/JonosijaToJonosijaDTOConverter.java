package fi.vm.sade.valintalaskenta.domain.converter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.Jonosija;
import fi.vm.sade.valintalaskenta.domain.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.Valintatapajono;
import fi.vm.sade.valintalaskenta.domain.comparator.JonosijaDTOComparator;
import fi.vm.sade.valintalaskenta.domain.dto.JarjestyskriteeritulosDTO;
import fi.vm.sade.valintalaskenta.domain.dto.JonosijaDTO;
import fi.vm.sade.valintalaskenta.domain.dto.ValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.dto.ValintatapajonoDTO;

/**
 * Created with IntelliJ IDEA. User: kkammone Date: 1.8.2013 Time: 12:48 To
 * change this template use File | Settings | File Templates.
 */
public class JonosijaToJonosijaDTOConverter {

    public List<ValinnanvaiheDTO> jarjestaValinnanvaiheJaLisaaJonosijaNumero(List<Valinnanvaihe> valinnanVaiheList) {
        List<ValinnanvaiheDTO> list = new ArrayList<ValinnanvaiheDTO>();
        if (valinnanVaiheList == null || valinnanVaiheList.isEmpty()) {
            return list;
        }
        for (Valinnanvaihe valinnanVaihe : valinnanVaiheList) {
            ValinnanvaiheDTO dto = new ValinnanvaiheDTO();
            dto.setCreatedAt(valinnanVaihe.getCreatedAt());
            dto.setJarjestysnumero(valinnanVaihe.getJarjestysnumero());
            dto.setValinnanvaiheoid(valinnanVaihe.getValinnanvaiheoid());
            dto.setValintatapajono(jarjestaValintapajonoJaLisaaJonosijaNumero(valinnanVaihe.getValintatapajono()));
            list.add(dto);
        }
        return list;
    }

    public List<ValintatapajonoDTO> jarjestaValintapajonoJaLisaaJonosijaNumero(List<Valintatapajono> valintapajonoList) {
        List<ValintatapajonoDTO> list = new ArrayList<ValintatapajonoDTO>();
        if (valintapajonoList == null || valintapajonoList.isEmpty()) {
            return list;
        }
        for (Valintatapajono valintatapajono : valintapajonoList) {
            ValintatapajonoDTO dto = new ValintatapajonoDTO();
            dto.setAloituspaikat(valintatapajono.getAloituspaikat());
            dto.setEiVarasijatayttoa(valintatapajono.getEiVarasijatayttoa());
            dto.setJonosijat(jarjestaJaLisaaJonosijaNumero(valintatapajono.getJonosijat()));
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

    public List<JonosijaDTO> jarjestaJaLisaaJonosijaNumero(List<Jonosija> jonosijat) {
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
            dto.setHistoriat(jonosija.getHistoriat());

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

            if (!jonosija.getJarjestyskriteerit().isEmpty()
                    && jonosija.getJarjestyskriteerit().firstEntry().getValue() != null) {
                jonosija.setTuloksenTila(jonosija.getJarjestyskriteerit().firstEntry().getValue().getTila());
            } else {
                //
            }

            previous = jonosija;
        }
        return list;
    }
}
