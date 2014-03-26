package fi.vm.sade.valintalaskenta.tulos.service.impl.converters;

import fi.vm.sade.valintalaskenta.domain.comparator.JonosijaDTOComparator;
import fi.vm.sade.valintalaskenta.domain.dto.*;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.*;
import fi.vm.sade.valintalaskenta.domain.valinta.*;
import fi.vm.sade.valintalaskenta.domain.valintakoe.*;
import org.springframework.stereotype.Component;

import java.util.*;

/**
 * User: kkammone Date: 1.8.2013 Time: 12:48
 */

@Component
public class ValintatulosConverterImpl implements ValintatulosConverter {

    @Override
    public List<ValinnanvaiheDTO> convertValinnanvaiheList(List<Valinnanvaihe> valinnanVaiheList) {
        List<ValinnanvaiheDTO> list = new ArrayList<ValinnanvaiheDTO>();
        if (valinnanVaiheList == null || valinnanVaiheList.isEmpty()) {
            return list;
        }
        for (Valinnanvaihe valinnanVaihe : valinnanVaiheList) {
            list.add(convertValinnanvaihe(valinnanVaihe));
        }
        return list;
    }

    @Override
    public List<HakukohdeDTO> convertValinnanvaihe(Collection<Valinnanvaihe> valinnanvaiheet) {
        Map<String, HakukohdeDTO> hakukohdeDTOtOidinMukaan = new HashMap<String, HakukohdeDTO>();

        for (Valinnanvaihe vv : valinnanvaiheet) {
            HakukohdeDTO hakukohdeDTO = null;

            if (hakukohdeDTOtOidinMukaan.containsKey(vv.getHakukohdeOid())) {
                hakukohdeDTO = hakukohdeDTOtOidinMukaan.get(vv.getHakukohdeOid());
            } else {
                hakukohdeDTO = new HakukohdeDTO();
                hakukohdeDTO.setHakuoid(vv.getHakuOid());
                hakukohdeDTO.setOid(vv.getHakukohdeOid());
                hakukohdeDTO.setTarjoajaoid(vv.getTarjoajaOid());
                hakukohdeDTOtOidinMukaan.put(vv.getHakukohdeOid(), hakukohdeDTO);
            }

            hakukohdeDTO.getValinnanvaihe().add(convertValinnanvaihe(vv));
        }

        return new ArrayList<HakukohdeDTO>(hakukohdeDTOtOidinMukaan.values());
    }

    @Override
    public List<ValintakoeOsallistuminenDTO> convertValintakoeOsallistuminen(
            List<ValintakoeOsallistuminen> osallistumiset) {
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
    public List<ValintakoeValinnanvaiheDTO> convertValinnanVaihe(List<ValintakoeValinnanvaihe> valinnanVaiheet) {
        List<ValintakoeValinnanvaiheDTO> dtot = new ArrayList<ValintakoeValinnanvaiheDTO>();

        for (ValintakoeValinnanvaihe vv : valinnanVaiheet) {
            ValintakoeValinnanvaiheDTO dto = new ValintakoeValinnanvaiheDTO();
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
            dto.setLahetetaankoKoekutsut(koe.isLahetaankoKoekutsut());

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

    @Override
    public ValinnanvaiheDTO convertValinnanvaihe(Valinnanvaihe valinnanvaihe) {
        ValinnanvaiheDTO dto = new ValinnanvaiheDTO();
        dto.setCreatedAt(valinnanvaihe.getCreatedAt());
        dto.setJarjestysnumero(valinnanvaihe.getJarjestysnumero());
        dto.setValinnanvaiheoid(valinnanvaihe.getValinnanvaiheOid());
        dto.setValintatapajono(convertValintatapajono(valinnanvaihe.getValintatapajonot()));
        dto.setNimi(valinnanvaihe.getNimi());
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
            dto.setOid(valintatapajono.getValintatapajonoOid());
            dto.setPrioriteetti(valintatapajono.getPrioriteetti());
            dto.setTasasijasaanto(valintatapajono.getTasasijasaanto());
            dto.setSiirretaanSijoitteluun(valintatapajono.isSiirretaanSijoitteluun());
            list.add(dto);
        }
        return list;
    }

    @Override
    public ValintatapajonoDTO convertValintatapajono(Valintatapajono jono) {
        ValintatapajonoDTO jonodto = new ValintatapajonoDTO();
        jonodto.setAloituspaikat(jono.getAloituspaikat());
        jonodto.setEiVarasijatayttoa(jono.getEiVarasijatayttoa());
        jonodto.setNimi(jono.getNimi());
        jonodto.setOid(jono.getValintatapajonoOid());
        jonodto.setPrioriteetti(jono.getPrioriteetti());
        jonodto.setSiirretaanSijoitteluun(jono.isSiirretaanSijoitteluun());
        jonodto.setTasasijasaanto(jono.getTasasijasaanto());
        jonodto.setJonosijat(convertJonosija(jono.getJonosijat()));

        return jonodto;
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
            dto.setHakemusOid(jonosija.getHakemusOid());
            dto.setHakijaOid(jonosija.getHakijaOid());
            dto.setPrioriteetti(jonosija.getHakutoiveprioriteetti());
            dto.setSukunimi(jonosija.getSukunimi());
            dto.setJarjestyskriteerit(new TreeSet<JarjestyskriteeritulosDTO>(convertJarjestyskriteeri(jonosija
                    .getJarjestyskriteeritulokset())));
            dto.setSyotetytArvot(convertSyotettyArvo(jonosija.getSyotetytArvot()));
            list.add(dto);
        }

        return list;
    }

    private List<SyotettyArvoDTO> convertSyotettyArvo(List<SyotettyArvo> syotetytArvot) {
        List<SyotettyArvoDTO> dtos = new ArrayList<SyotettyArvoDTO>();
        for (SyotettyArvo sa : syotetytArvot) {
            dtos.add(convertSyotettyArvo(sa));
        }
        return dtos;
    }

    private SyotettyArvoDTO convertSyotettyArvo(SyotettyArvo sa) {
        SyotettyArvoDTO dto = new SyotettyArvoDTO();
        dto.setArvo(sa.getArvo());
        dto.setLaskennallinenArvo(sa.getLaskennallinenArvo());
        dto.setOsallistuminen(sa.getOsallistuminen());
        dto.setTunniste(sa.getTunniste());
        return dto;
    }

    @Override
    public JarjestyskriteeritulosDTO convertJarjestyskriteeri(Jarjestyskriteeritulos jktulos) {
        JarjestyskriteeritulosDTO jdto = new JarjestyskriteeritulosDTO();
        jdto.setPrioriteetti(jktulos.getPrioriteetti());
        jdto.setArvo(jktulos.getArvo());
        jdto.setKuvaus(jktulos.getKuvaus());
        jdto.setTila(jktulos.getTila());
        jdto.setNimi(jktulos.getNimi());
        return jdto;
    }

    @Override
    public List<JarjestyskriteeritulosDTO> convertJarjestyskriteeri(Collection<Jarjestyskriteeritulos> jktulos) {
        List<JarjestyskriteeritulosDTO> dtos = new ArrayList<JarjestyskriteeritulosDTO>();
        if (jktulos == null || jktulos.isEmpty()) {
            return dtos;
        }

        for (Jarjestyskriteeritulos jk : jktulos) {
            dtos.add(convertJarjestyskriteeri(jk));
        }

        return dtos;
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

            if (!jonosija.getJarjestyskriteerit().isEmpty()) {
                jonosija.setTuloksenTila(jonosija.getJarjestyskriteerit().first().getTila());
            }
            previous = jonosija;
        }
    }

}
