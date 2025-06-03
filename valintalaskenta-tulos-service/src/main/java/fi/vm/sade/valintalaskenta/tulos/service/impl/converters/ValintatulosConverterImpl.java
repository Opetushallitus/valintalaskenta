package fi.vm.sade.valintalaskenta.tulos.service.impl.converters;

import com.google.common.collect.Maps;
import fi.vm.sade.valintalaskenta.domain.comparator.JonosijaDTOComparator;
import fi.vm.sade.valintalaskenta.domain.dto.*;
import fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto.*;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.*;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.ValintatietoValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.ValintatietoValintatapajonoDTO;
import fi.vm.sade.valintalaskenta.domain.valinta.*;
import fi.vm.sade.valintalaskenta.domain.valinta.sijoittelu.SijoitteluJonosija;
import fi.vm.sade.valintalaskenta.domain.valinta.sijoittelu.SijoitteluValintatapajono;
import fi.vm.sade.valintalaskenta.domain.valintakoe.*;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.stream.Collectors;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

@Component
public class ValintatulosConverterImpl implements ValintatulosConverter {
  private static final Logger LOG = LoggerFactory.getLogger(ValintatulosConverterImpl.class);

  @Override
  public List<ValintatietoValinnanvaiheDTO> convertValinnanvaiheList(
      List<Valinnanvaihe> valinnanVaiheList) {
    List<ValintatietoValinnanvaiheDTO> list = new ArrayList<>();
    if (valinnanVaiheList == null || valinnanVaiheList.isEmpty()) {
      return list;
    }
    for (Valinnanvaihe valinnanVaihe : valinnanVaiheList) {
      list.add(convertValinnanvaihe(valinnanVaihe));
    }
    return list;
  }

  @Override
  public List<ValintatietoValinnanvaiheSiirtotiedostoDTO> convertValinnanvaiheListForSiirtotiedosto(
      List<ValintatietoValinnanvaiheDTO> valinnanVaiheList) {
    List<ValintatietoValinnanvaiheSiirtotiedostoDTO> vaiheet = new ArrayList<>();
    SimpleDateFormat sdf =
        new SimpleDateFormat(SiirtotiedostoConstants.SIIRTOTIEDOSTO_DATETIME_FORMAT);
    for (ValintatietoValinnanvaiheDTO valinnanVaiheDTO : valinnanVaiheList) {
      ValintatietoValinnanvaiheSiirtotiedostoDTO newVaihe =
          new ValintatietoValinnanvaiheSiirtotiedostoDTO();
      BeanUtils.copyProperties(valinnanVaiheDTO, newVaihe, "lastModified", "valintatapajonot");
      newVaihe.setLastModified(formatDate(sdf, valinnanVaiheDTO.getLastModified()));
      for (ValintatietoValintatapajonoDTO jonoDTO : valinnanVaiheDTO.getValintatapajonot()) {
        ValintatietoValintatapajonoSiirtotiedostoDTO newJono =
            new ValintatietoValintatapajonoSiirtotiedostoDTO();
        BeanUtils.copyProperties(jonoDTO, newJono, "lastModified", "jonosijat");
        newJono.setLastModified(formatDate(sdf, jonoDTO.getLastModified()));
        for (JonosijaDTO jonosijaDTO : jonoDTO.getJonosijat()) {
          JonosijaSiirtotiedostoDTO newJonosijaDTO = new JonosijaSiirtotiedostoDTO();
          BeanUtils.copyProperties(
              jonosijaDTO,
              newJonosijaDTO,
              "lastModified",
              "jarjestyskriteerit",
              "syotetytArvot",
              "funktioTulokset");
          newJonosijaDTO.setLastModified(formatDate(sdf, jonosijaDTO.getLastModified()));
          newJono.getJonosijat().add(newJonosijaDTO);
          for (JarjestyskriteeritulosDTO tulosDTO : jonosijaDTO.getJarjestyskriteerit()) {
            JarjestyskriteeritulosSiirtotiedostoDTO newTulosDTO =
                new JarjestyskriteeritulosSiirtotiedostoDTO();
            BeanUtils.copyProperties(tulosDTO, newTulosDTO);
            newJonosijaDTO.getJarjestyskriteerit().add(newTulosDTO);
          }
          for (SyotettyArvoDTO arvoDTO : jonosijaDTO.getSyotetytArvot()) {
            SyotettyArvoSiirtotiedostoDTO newArvoDTO = new SyotettyArvoSiirtotiedostoDTO();
            BeanUtils.copyProperties(arvoDTO, newArvoDTO);
            newJonosijaDTO.getSyotetytArvot().add(newArvoDTO);
          }
          for (FunktioTulosDTO tulosDTO : jonosijaDTO.getFunktioTulokset()) {
            FunktioTulosSiirtotiedostoDTO newTulosDTO = new FunktioTulosSiirtotiedostoDTO();
            BeanUtils.copyProperties(tulosDTO, newTulosDTO);
            newJonosijaDTO.getFunktioTulokset().add(newTulosDTO);
          }
        }
        newVaihe.getValintatapajonot().add(newJono);
      }
      vaiheet.add(newVaihe);
    }
    return vaiheet;
  }

  @Override
  public List<ValintakoeOsallistuminenSiirtotiedostoDTO>
      convertValintakoeOsallistuminenListForSiirtotiedosto(
          List<ValintakoeOsallistuminenDTO> valintakoeOsallistuminenList) {
    SimpleDateFormat sdf =
        new SimpleDateFormat(SiirtotiedostoConstants.SIIRTOTIEDOSTO_DATETIME_FORMAT);
    List<ValintakoeOsallistuminenSiirtotiedostoDTO> osallistumiset = new ArrayList<>();
    for (ValintakoeOsallistuminenDTO vko : valintakoeOsallistuminenList) {
      ValintakoeOsallistuminenSiirtotiedostoDTO vkoDto =
          new ValintakoeOsallistuminenSiirtotiedostoDTO();
      BeanUtils.copyProperties(vko, vkoDto, "hakutoiveet");
      vkoDto.setLastModified(formatDate(sdf, vko.getCreatedAt()));
      for (HakutoiveDTO toive : vko.getHakutoiveet()) {
        HakutoiveSiirtotiedostoDTO htDto = new HakutoiveSiirtotiedostoDTO();
        BeanUtils.copyProperties(toive, htDto, "valinnanVaiheet");
        htDto.setLastModified(formatDate(sdf, toive.getCreatedAt()));
        for (ValintakoeValinnanvaiheDTO vaihe : toive.getValinnanVaiheet()) {
          ValintakoeValinnanvaiheSiirtotiedostoDTO vaiheDto =
              new ValintakoeValinnanvaiheSiirtotiedostoDTO();
          BeanUtils.copyProperties(vaihe, vaiheDto, "valintakokeet", "lastModified");
          vaiheDto.setLastModified(formatDate(sdf, vaihe.getLastModified()));
          for (ValintakoeDTO koe : vaihe.getValintakokeet()) {
            ValintakoeSiirtotiedostoDTO koeDto = new ValintakoeSiirtotiedostoDTO();
            BeanUtils.copyProperties(koe, koeDto, "lastModified");
            koeDto.setLastModified(formatDate(sdf, koe.getLastModified()));
            vaiheDto.getValintakokeet().add(koeDto);
          }
          htDto.getValinnanVaiheet().add(vaiheDto);
        }
        vkoDto.getHakutoiveet().add(htDto);
      }
      osallistumiset.add(vkoDto);
    }
    return osallistumiset;
  }

  private String formatDate(SimpleDateFormat sdf, Date date) {
    return date != null ? sdf.format(date) : null;
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
      dto.setHakemusOid(vko.getHakemusOid());
      dto.setHakijaOid(vko.getHakijaOid());
      dto.setHakuOid(vko.getHakuOid());
      dto.setHakutoiveet(convertHakutoive(vko.getHakutoiveetAsList()));
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
      dto.setValinnanVaiheet(convertValinnanVaihe(ht.getValintakoeValinnanvaiheetAsList()));
      dtot.add(dto);
    }
    return dtot;
  }

  @Override
  public List<ValintakoeValinnanvaiheDTO> convertValinnanVaihe(
      List<ValintakoeValinnanvaihe> valinnanVaiheet) {
    List<ValintakoeValinnanvaiheDTO> dtot = new ArrayList<ValintakoeValinnanvaiheDTO>();
    for (ValintakoeValinnanvaihe vv : valinnanVaiheet) {
      ValintakoeValinnanvaiheDTO dto = new ValintakoeValinnanvaiheDTO();
      dto.setValinnanVaiheJarjestysluku(vv.getValinnanVaiheJarjestysluku());
      dto.setValinnanVaiheOid(vv.getValinnanvaiheOid());
      dto.setValintakokeet(convertValintakoe(vv.getValintakokeetAsList()));
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
      dto.setOsallistuminenTulos(convertOsallistuminenTulos(koe));
      dto.setLahetetaankoKoekutsut(koe.isLahetetaankoKoekutsut());
      dto.setAktiivinen(koe.isAktiivinen());
      dto.setKutsuttavienMaara(koe.getKutsuttavienMaara());
      dtot.add(dto);
    }
    return dtot;
  }

  @Override
  public OsallistuminenTulosDTO convertOsallistuminenTulos(Valintakoe valintakoe) {
    OsallistuminenTulosDTO dto = new OsallistuminenTulosDTO();
    dto.setKuvaus(valintakoe.getKuvaus());
    dto.setLaskentaTila(valintakoe.getLaskentaTila());
    dto.setLaskentaTulos(valintakoe.getLaskentaTulos());
    dto.setOsallistuminen(valintakoe.getOsallistuminen());

    return dto;
  }

  @Override
  public ValintatietoValinnanvaiheDTO convertValinnanvaihe(Valinnanvaihe valinnanvaihe) {
    ValintatietoValinnanvaiheDTO dto = new ValintatietoValinnanvaiheDTO();
    dto.setCreatedAt(valinnanvaihe.getCreatedAt());
    dto.setJarjestysnumero(valinnanvaihe.getJarjestysnumero());
    dto.setValinnanvaiheoid(valinnanvaihe.getValinnanVaiheOid());
    dto.setHakuOid(valinnanvaihe.getHakuOid());
    dto.setValintatapajonot(convertValintatapajono(valinnanvaihe.getValintatapajonot()));
    dto.setNimi(valinnanvaihe.getNimi());
    dto.setLastModified(valinnanvaihe.getLastModified());
    dto.setHakukohdeOid(valinnanvaihe.getHakukohdeOid());
    return dto;
  }

  @Override
  public List<ValintatietoValintatapajonoDTO> convertValintatapajono(
      List<Valintatapajono> valintapajonoList) {
    List<ValintatietoValintatapajonoDTO> list = new ArrayList<ValintatietoValintatapajonoDTO>();
    if (valintapajonoList == null || valintapajonoList.isEmpty()) {
      return list;
    }
    for (Valintatapajono valintatapajono : valintapajonoList) {
      ValintatietoValintatapajonoDTO dto = new ValintatietoValintatapajonoDTO();
      dto.setAloituspaikat(valintatapajono.getAloituspaikat());
      dto.setEiVarasijatayttoa(valintatapajono.getEiVarasijatayttoa());
      dto.setKaytetaanValintalaskentaa(valintatapajono.getKaytetaanValintalaskentaa());
      dto.setKaikkiEhdonTayttavatHyvaksytaan(valintatapajono.getKaikkiEhdonTayttavatHyvaksytaan());
      dto.setPoissaOlevaTaytto(valintatapajono.getPoissaOlevaTaytto());
      dto.setJonosijat(convertJonosija(valintatapajono.getJonosijat()));
      dto.setNimi(valintatapajono.getNimi());
      dto.setOid(valintatapajono.getValintatapajonoOid());
      dto.setPrioriteetti(valintatapajono.getPrioriteetti());
      dto.setTasasijasaanto(valintatapajono.getTasasijasaanto());
      dto.setSiirretaanSijoitteluun(valintatapajono.isSiirretaanSijoitteluun());
      if (valintatapajono.getValmisSijoiteltavaksi() != null) {
        dto.setValmisSijoiteltavaksi(valintatapajono.getValmisSijoiteltavaksi());
      }
      if (valintatapajono.getSijoitteluajoId() != null) {
        dto.setSijoitteluajoId(valintatapajono.getSijoitteluajoId());
      }
      dto.setKaytetaanKokonaispisteita(valintatapajono.getKaytetaanKokonaispisteita());
      dto.setLastModified(valintatapajono.getLastModified());
      list.add(dto);
    }
    return list;
  }

  @Override
  public ValintatietoValintatapajonoDTO convertValintatapajono(Valintatapajono jono) {
    ValintatietoValintatapajonoDTO jonodto = new ValintatietoValintatapajonoDTO();
    jonodto.setAloituspaikat(jono.getAloituspaikat());
    jonodto.setEiVarasijatayttoa(jono.getEiVarasijatayttoa());
    jonodto.setKaytetaanValintalaskentaa(jono.getKaytetaanValintalaskentaa());
    jonodto.setKaikkiEhdonTayttavatHyvaksytaan(jono.getKaikkiEhdonTayttavatHyvaksytaan());
    jonodto.setPoissaOlevaTaytto(jono.getPoissaOlevaTaytto());
    jonodto.setNimi(jono.getNimi());
    jonodto.setOid(jono.getValintatapajonoOid());
    jonodto.setPrioriteetti(jono.getPrioriteetti());
    jonodto.setSiirretaanSijoitteluun(jono.isSiirretaanSijoitteluun());
    jonodto.setTasasijasaanto(jono.getTasasijasaanto());
    jonodto.setJonosijat(convertJonosija(jono.getJonosijat()));
    if (jono.getValmisSijoiteltavaksi() != null) {
      jonodto.setValmisSijoiteltavaksi(jono.getValmisSijoiteltavaksi());
    }
    if (jono.getSijoitteluajoId() != null) {
      jonodto.setSijoitteluajoId(jono.getSijoitteluajoId());
    }

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
      dto.setHakemusOid(jonosija.getHakemusOid());
      dto.setHakijaOid(jonosija.getHakijaOid());
      dto.setPrioriteetti(jonosija.getHakutoiveprioriteetti());
      dto.setJarjestyskriteerit(
          new TreeSet<>(
              convertJarjestyskriteeri(
                  jonosija.getJarjestyskriteeritulokset().jarjestyskriteeritulokset)));
      dto.setSyotetytArvot(convertSyotettyArvo(jonosija.getSyotetytArvot()));
      dto.setFunktioTulokset(convertFunktioTulos(jonosija.getFunktioTulokset().funktioTulokset));
      dto.setHylattyValisijoittelussa(jonosija.isHylattyValisijoittelussa());
      dto.setLastModified(jonosija.getLastModified());
      list.add(dto);
    }
    return list;
  }

  private List<SyotettyArvoDTO> convertSyotettyArvo(List<SyotettyArvo> syotetytArvot) {
    List<SyotettyArvoDTO> dtos = new ArrayList<>();
    for (SyotettyArvo sa : syotetytArvot) {
      dtos.add(convertSyotettyArvo(sa));
    }
    return dtos;
  }

  public SyotettyArvoDTO convertSyotettyArvo(SyotettyArvo sa) {
    SyotettyArvoDTO dto = new SyotettyArvoDTO();
    dto.setArvo(sa.getArvo());
    dto.setLaskennallinenArvo(sa.getLaskennallinenArvo());
    dto.setOsallistuminen(sa.getOsallistuminen());
    dto.setTunniste(sa.getTunniste());
    dto.setTyypinKoodiUri(sa.getTyypinKoodiUri());
    dto.setTilastoidaan(sa.isTilastoidaan());
    return dto;
  }

  @Override
  public ValintatietoValintatapajonoDTO convertSijoitteluValintatapajono(
      SijoitteluValintatapajono jono, List<SijoitteluJonosija> kriteeritValintatapajonolle) {

    ValintatietoValintatapajonoDTO jonoDto = new ValintatietoValintatapajonoDTO();
    jonoDto.setAloituspaikat(jono.aloituspaikat);
    jonoDto.setEiVarasijatayttoa(jono.eiVarasijatayttoa);
    jonoDto.setKaikkiEhdonTayttavatHyvaksytaan(jono.kaikkiEhdonTayttavatHyvaksytaan);
    jonoDto.setKaytetaanKokonaispisteita(jono.kaytetaanKokonaisPisteita);
    jonoDto.setKaytetaanValintalaskentaa(jono.kaytetaanValintalaskentaa);
    jonoDto.setSijoitteluajoId(jono.sijoitteluajoId);
    jonoDto.setSiirretaanSijoitteluun(jono.siirretaanSijoitteluun);
    jonoDto.setPrioriteetti(jono.prioriteetti);
    jonoDto.setTasasijasaanto(jono.tasasijasaanto);
    jonoDto.setNimi(jono.nimi);
    jonoDto.setOid(jono.valintatapajonoOid);
    jonoDto.setPoissaOlevaTaytto(jono.poissaOlevaTaytto);
    jonoDto.setValmisSijoiteltavaksi(jono.valmisSijoiteltavaksi);

    jonoDto.setJonosijat(
        kriteeritValintatapajonolle.stream().map(this::convertJonosijaDTOForSijoittelu).toList());

    return jonoDto;
  }

  private JonosijaDTO convertJonosijaDTOForSijoittelu(SijoitteluJonosija jonosija) {
    JonosijaDTO jonosijaDTO = new JonosijaDTO();
    jonosijaDTO.setHakemusOid(jonosija.hakemusOid);
    jonosijaDTO.setHakijaOid(jonosija.hakijaOid);
    jonosijaDTO.setPrioriteetti(jonosija.hakutoiveprioriteetti);
    jonosijaDTO.setSyotetytArvot(
        jonosija.syotetytArvot.syotetytArvot.stream().map(this::convertSyotettyArvo).toList());
    jonosijaDTO.setHylattyValisijoittelussa(jonosija.hylattyValisijoittelussa);

    jonosijaDTO.setJarjestyskriteerit(
        new TreeSet<>(
            jonosija.jarjestyskriteeritulokset.jarjestyskriteeritulokset.stream()
                .map(this::convertSijoitteluJarjestyskriteeritulosToDTO)
                .collect(Collectors.toSet())));
    return jonosijaDTO;
  }

  private JarjestyskriteeritulosDTO convertSijoitteluJarjestyskriteeritulosToDTO(
      Jarjestyskriteeritulos kri) {
    JarjestyskriteeritulosDTO kriDTO = new JarjestyskriteeritulosDTO();
    kriDTO.setArvo(kri.getArvo());
    kriDTO.setPrioriteetti(kri.getPrioriteetti());
    kriDTO.setTila(kri.getTila());
    kriDTO.setNimi(kri.getNimi());
    kriDTO.setKuvaus(kri.getKuvaus());
    return kriDTO;
  }

  private List<FunktioTulosDTO> convertFunktioTulos(List<FunktioTulos> funktioTulokset) {
    List<FunktioTulosDTO> dtos = new ArrayList<FunktioTulosDTO>();
    for (FunktioTulos ft : funktioTulokset) {
      dtos.add(convertFunktioTulos(ft));
    }
    return dtos;
  }

  private FunktioTulosDTO convertFunktioTulos(FunktioTulos ft) {
    FunktioTulosDTO dto = new FunktioTulosDTO();
    dto.setArvo(ft.getArvo());
    dto.setTunniste(ft.getTunniste());
    dto.setNimiFi(ft.getNimiFi());
    dto.setNimiSv(ft.getNimiSv());
    dto.setNimiEn(ft.getNimiEn());
    dto.setOmaopintopolku(ft.isOmaopintopolku());
    return dto;
  }

  @Override
  public JarjestyskriteeritulosDTO convertJarjestyskriteeri(Jarjestyskriteeritulos jktulos) {
    JarjestyskriteeritulosDTO jdto = new JarjestyskriteeritulosDTO();
    jdto.setPrioriteetti(jktulos.getPrioriteetti());
    jdto.setArvo(jktulos.getArvo());
    jdto.setKuvaus(Collections.<String, String>emptyMap());
    try {
      if (jktulos.getKuvaus() != null) {
        jdto.setKuvaus(Maps.newHashMap(jktulos.getKuvaus()));
      }
    } catch (Exception e) {
      try {
        LOG.error("jktulos is of type({})", jktulos.getKuvaus().getClass());
      } catch (Exception ex) {
        LOG.error("jktuloksesta ei saa edes tyyppiä ulos");
      }
      jdto.setKuvaus(Collections.<String, String>emptyMap());
      LOG.error("Siirto kantaentiteetistä(Jarjestyskriteeritulos.class) dto:ksi epäonnistui!", e);
    }
    jdto.setTila(jktulos.getTila());
    jdto.setNimi(jktulos.getNimi());
    return jdto;
  }

  @Override
  public List<JarjestyskriteeritulosDTO> convertJarjestyskriteeri(
      Collection<Jarjestyskriteeritulos> jktulos) {
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
    int j = -1;
    JonosijaDTO previous = null;
    Iterator<JonosijaDTO> it = list.iterator();
    while (it.hasNext()) {
      JonosijaDTO jonosija = it.next();
      if (previous != null) {
        int compareResult = comparator.compare(previous, jonosija);
        if (compareResult != 0) {
          if (j != -1) {
            i = j;
            j = -1;
          }
          i++;
        } else if (compareResult == 0) {
          if (j == -1) {
            j = i;
          }
          j++;
        }
      }
      jonosija.setJonosija(i);

      if (!jonosija.getJarjestyskriteerit().isEmpty()) {
        jonosija.setTuloksenTila(jonosija.getJarjestyskriteerit().first().getTila());
      }
      previous = jonosija;
    }
  }
}
