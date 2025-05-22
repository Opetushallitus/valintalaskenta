package fi.vm.sade.valintalaskenta.tulos.service.impl;

import com.google.common.collect.Collections2;
import fi.vm.sade.auditlog.Changes;
import fi.vm.sade.auditlog.User;
import fi.vm.sade.valinta.sharedutils.ValintaResource;
import fi.vm.sade.valinta.sharedutils.ValintaperusteetOperation;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakijaryhmaDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakukohdeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.JarjestyskriteeritulosDTO;
import fi.vm.sade.valintalaskenta.domain.dto.JonoDto;
import fi.vm.sade.valintalaskenta.domain.dto.JonosijaDTO;
import fi.vm.sade.valintalaskenta.domain.dto.MuokattuJonosijaArvoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.ValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto.ValintatietoValinnanvaiheSiirtotiedostoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.HakutoiveDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeOsallistuminenDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.ValintatietoValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.ValintatietoValintatapajonoDTO;
import fi.vm.sade.valintalaskenta.domain.valinta.*;
import fi.vm.sade.valintalaskenta.domain.valinta.sijoittelu.SijoitteluJonosija;
import fi.vm.sade.valintalaskenta.domain.valinta.sijoittelu.SijoitteluValintatapajono;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Hakutoive;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Valintakoe;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeValinnanvaihe;
import fi.vm.sade.valintalaskenta.tulos.LaskentaAudit;
import fi.vm.sade.valintalaskenta.tulos.dao.*;
import fi.vm.sade.valintalaskenta.tulos.logging.LaskentaAuditLog;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import fi.vm.sade.valintalaskenta.tulos.service.AuthorizationUtil;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import fi.vm.sade.valintalaskenta.tulos.service.exception.EiOikeuttaPoistaaValintatapajonoaSijoittelustaException;
import fi.vm.sade.valintalaskenta.tulos.service.impl.converters.ValintatulosConverter;
import java.util.*;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.repository.query.Param;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

@Service
public class ValintalaskentaTulosServiceImpl implements ValintalaskentaTulosService {
  private static final Logger LOGGER =
      LoggerFactory.getLogger(ValintalaskentaTulosServiceImpl.class);

  private final TulosValinnanvaiheDAO tulosValinnanvaiheDAO;

  private final TulosHakijaryhmaDAO tulosHakijaryhmaDAO;

  private final TulosValintakoeOsallistuminenDAO tulosValintakoeOsallistuminenDAO;

  private final MuokattuJonosijaDAO muokattuJonosijaDAO;

  private final ValintatulosConverter valintatulosConverter;

  private final HarkinnanvarainenHyvaksyminenDAO harkinnanvarainenHyvaksyminenDAO;

  private final ValintalaskentaModelMapper modelMapper;

  private final TulosValintatapajonoDAO tulosValintatapajonoDAO;

  private final LaskentaAuditLog auditLog;

  @Value("${root.organisaatio.oid}")
  private String rootOrgOid;

  @Autowired
  public ValintalaskentaTulosServiceImpl(
      TulosValinnanvaiheDAO tulosValinnanvaiheDAO,
      TulosHakijaryhmaDAO tulosHakijaryhmaDAO,
      TulosValintakoeOsallistuminenDAO tulosValintakoeOsallistuminenDAO,
      MuokattuJonosijaDAO muokattuJonosijaDAO,
      ValintatulosConverter valintatulosConverter,
      HarkinnanvarainenHyvaksyminenDAO harkinnanvarainenHyvaksyminenDAO,
      TulosValintatapajonoDAO tulosValintatapajonoDAO,
      ValintalaskentaModelMapper modelMapper,
      @Qualifier("LaskentaAuditLog") LaskentaAuditLog auditLog) {
    this.tulosValinnanvaiheDAO = tulosValinnanvaiheDAO;
    this.tulosHakijaryhmaDAO = tulosHakijaryhmaDAO;
    this.tulosValintakoeOsallistuminenDAO = tulosValintakoeOsallistuminenDAO;
    this.muokattuJonosijaDAO = muokattuJonosijaDAO;
    this.valintatulosConverter = valintatulosConverter;
    this.harkinnanvarainenHyvaksyminenDAO = harkinnanvarainenHyvaksyminenDAO;
    this.tulosValintatapajonoDAO = tulosValintatapajonoDAO;
    this.modelMapper = modelMapper;
    this.auditLog = auditLog;
  }

  public List<JonoDto> haeJonotSijoittelussa(String hakuOid) {
    final Function<Valinnanvaihe, Stream<JonoDto>> valinnanvaiheToJonoDtos =
        vv ->
            vv.getValintatapajonot().stream()
                .map(
                    jono ->
                        new JonoDto(
                            vv.getHakukohdeOid(),
                            jono.getValintatapajonoOid(),
                            jono.getValmisSijoiteltavaksi(),
                            jono.isSiirretaanSijoitteluun()));
    return tulosValinnanvaiheDAO.readByHakuOid(hakuOid).stream()
        .flatMap(valinnanvaiheToJonoDtos)
        .collect(Collectors.toList());
  }

  public HakemusDTO haeTuloksetHakemukselle(final String hakuOid, final String hakemusOid) {
    List<Valinnanvaihe> valinnanVaiheet =
        tulosValinnanvaiheDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid);
    Map<String, HakukohdeDTO> hakukohdeDTOtOidinMukaan = new HashMap<>();
    List<MuokattuJonosija> muokatutJonosijat =
        muokattuJonosijaDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid);
    List<HarkinnanvarainenHyvaksyminen> harkinnanvaraiset =
        harkinnanvarainenHyvaksyminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid);
    ValintakoeOsallistuminenDTO kokeet =
        modelMapper.map(haeValintakoeOsallistumiset(hakemusOid), ValintakoeOsallistuminenDTO.class);

    for (Valinnanvaihe vv : valinnanVaiheet) {
      HakukohdeDTO hakukohdeDTO = getOrCreateHakukohdeDTO(hakukohdeDTOtOidinMukaan, vv);
      ValintatietoValinnanvaiheDTO vvdto =
          createValintatietoValinnanvaiheDTO(
              hakuOid, vv.getCreatedAt(), vv.getValinnanVaiheOid(), vv.getJarjestysnumero());
      for (Valintatapajono jono : vv.getValintatapajonot()) {
        jono.setJonosijat(
            new HashSet<>(
                Collections2.filter(
                    jono.getJonosijat(), jonosija -> hakemusOid.equals(jonosija.getHakemusOid()))));
        vvdto.getValintatapajonot().add(valintatulosConverter.convertValintatapajono(jono));
      }
      hakukohdeDTO.getValinnanvaihe().add(vvdto);
    }

    // Valintakokeet
    for (HakutoiveDTO toive : kokeet.getHakutoiveet()) {
      HakukohdeDTO hakukohdeDTO = null;
      if (hakukohdeDTOtOidinMukaan.containsKey(toive.getHakukohdeOid())) {
        hakukohdeDTO = hakukohdeDTOtOidinMukaan.get(toive.getHakukohdeOid());
      } else {
        hakukohdeDTO = new HakukohdeDTO();
        hakukohdeDTO.setHakuoid(kokeet.getHakuOid());
        hakukohdeDTO.setOid(toive.getHakukohdeOid());
        hakukohdeDTOtOidinMukaan.put(toive.getHakukohdeOid(), hakukohdeDTO);
      }
      for (ValintakoeValinnanvaiheDTO vv : toive.getValinnanVaiheet()) {
        ValintatietoValinnanvaiheDTO vvdto =
            createValintatietoValinnanvaiheDTO(
                hakuOid,
                kokeet.getCreatedAt(),
                vv.getValinnanVaiheOid(),
                vv.getValinnanVaiheJarjestysluku());
        vvdto.getValintakokeet().addAll(vv.getValintakokeet());
        hakukohdeDTO.getValinnanvaihe().add(vvdto);
      }
    }
    for (HakukohdeDTO hk : hakukohdeDTOtOidinMukaan.values()) {
      applyMuokatutJonosijat(
          hk.getOid(), hk.getValinnanvaihe(), muokatutJonosijat, harkinnanvaraiset);
    }
    return new HakemusDTO(hakuOid, hakemusOid, new ArrayList<>(hakukohdeDTOtOidinMukaan.values()));
  }

  private ValintatietoValinnanvaiheDTO createValintatietoValinnanvaiheDTO(
      String hakuOid, Date createdAt, String valinnanvaiheOid, int jarjestysnumero) {
    ValintatietoValinnanvaiheDTO vvdto = new ValintatietoValinnanvaiheDTO();
    vvdto.setHakuOid(hakuOid);
    vvdto.setCreatedAt(createdAt);
    vvdto.setValinnanvaiheoid(valinnanvaiheOid);
    vvdto.setJarjestysnumero(jarjestysnumero);
    return vvdto;
  }

  private void applyMuokatutJonosijatToValinnanvaihe(
      String hakukohdeoid, List<ValintatietoValinnanvaiheDTO> b) {
    List<MuokattuJonosija> a = muokattuJonosijaDAO.readByhakukohdeOid(hakukohdeoid);
    List<HarkinnanvarainenHyvaksyminen> c =
        harkinnanvarainenHyvaksyminenDAO.haeHarkinnanvarainenHyvaksyminen(hakukohdeoid);
    applyMuokatutJonosijat(hakukohdeoid, b, a, c);
  }

  private void applyMuokatutJonosijatToValinnanvaihe(
      List<ValintatietoValinnanvaiheDTO> valinnanvaiheet) {
    valinnanvaiheet.forEach(
        valinnanvaihe -> {
          String hakukohdeOid = valinnanvaihe.getHakukohdeOid();
          List<MuokattuJonosija> muokatutJonosjat =
              muokattuJonosijaDAO.readByhakukohdeOid(hakukohdeOid);
          List<HarkinnanvarainenHyvaksyminen> harkinnanvaraisetHyvaksymiset =
              harkinnanvarainenHyvaksyminenDAO.haeHarkinnanvarainenHyvaksyminen(hakukohdeOid);
          applyMuokatutJonosijat(
              hakukohdeOid,
              List.of(valinnanvaihe),
              muokatutJonosjat,
              harkinnanvaraisetHyvaksymiset);
        });
  }

  private void applyMuokatutJonosijatToHakukohde(String hakuOid, List<HakukohdeDTO> b) {
    LOGGER.info("Haetaan muokatut jonosijat {}!", hakuOid);
    List<MuokattuJonosija> a = muokattuJonosijaDAO.readByHakuOid(hakuOid);
    LOGGER.info("Muokatut jonosijat haettu, haetaan harkinnanvaraiset {}!", hakuOid);
    List<HarkinnanvarainenHyvaksyminen> c =
        harkinnanvarainenHyvaksyminenDAO.haeHarkinnanvaraisetHyvaksymisetHaulle(hakuOid);
    LOGGER.info(
        "Harkinnavaraiset haettu, loopataan hakukohteet {} - yhteensä {}", hakuOid, b.size());
    for (int i = 0; i < b.size(); i++) {
      HakukohdeDTO hakukohde = b.get(i);
      LOGGER.debug("Laitetaan jonosijat hakukohteelle {} - indeksi {}", hakukohde.getOid(), i);
      applyMuokatutJonosijat(hakukohde.getOid(), hakukohde.getValinnanvaihe(), a, c);
    }
    LOGGER.info("Muokatut jonosijat laitettu kaikille hakukohteille {}!", hakuOid);
  }

  private void applyMuokatutJonosijat(
      String hakukohdeoid,
      List<ValintatietoValinnanvaiheDTO> valinnanvaiheet,
      List<MuokattuJonosija> muokatutJonosijat,
      List<HarkinnanvarainenHyvaksyminen> harkinnanvaraisetHyvaksymiset) {
    for (ValintatietoValinnanvaiheDTO dto : valinnanvaiheet) {
      for (ValintatietoValintatapajonoDTO valintatapajonoDTO : dto.getValintatapajonot()) {
        for (JonosijaDTO jonosija : valintatapajonoDTO.getJonosijat()) {
          for (MuokattuJonosija muokattuJonosija : muokatutJonosijat) {
            if (muokattuJonosija.getHakemusOid().equals(jonosija.getHakemusOid())
                && valintatapajonoDTO.getOid().equals(muokattuJonosija.getValintatapajonoOid())) {
              applyJonosija(jonosija, muokattuJonosija);
            }
          }
          harkinnanvaraisetHyvaksymiset.forEach(
              h -> {
                if (h.getHakemusOid().equals(jonosija.getHakemusOid())
                    && h.getHakukohdeOid().equals(hakukohdeoid)) {
                  applyHarkinnanvarainenHyvaksynta(jonosija, h);
                }
              });
        }
        valintatapajonoDTO.setJonosijat(new ArrayList<>(valintatapajonoDTO.getJonosijat()));
        valintatulosConverter.sort(valintatapajonoDTO.getJonosijat());
      }
    }
  }

  private void applyHarkinnanvarainenHyvaksynta(
      JonosijaDTO jonosija, HarkinnanvarainenHyvaksyminen hyvaksyminen) {
    if (hyvaksyminen.getHarkinnanvaraisuusTila() == HarkinnanvaraisuusTila.HYVAKSYTTY) {
      JarjestyskriteeritulosDTO jarjestyskriteeritulos = jonosija.getJarjestyskriteerit().first();
      if (jarjestyskriteeritulos != null) {
        jarjestyskriteeritulos.setTila(
            JarjestyskriteerituloksenTila.HYVAKSYTTY_HARKINNANVARAISESTI);
        jarjestyskriteeritulos.setKuvaus(new HashMap<>());
      }
      jonosija.setTuloksenTila(JarjestyskriteerituloksenTila.HYVAKSYTTY_HARKINNANVARAISESTI);
      if (safeCheckDateIsAfter(hyvaksyminen.getLastModified(), jonosija.getLastModified())) {
        jonosija.setLastModified(hyvaksyminen.getLastModified());
      }
    }
  }

  private void applyJonosija(JonosijaDTO jonosijaDTO, MuokattuJonosija muokattuJonosija) {
    boolean jonosijaMuokattu = false;
    if (muokattuJonosija.getPrioriteetti() != null) {
      jonosijaDTO.setPrioriteetti(muokattuJonosija.getPrioriteetti());
      jonosijaMuokattu = true;
    }
    Map<Integer, Jarjestyskriteeritulos> muokatunJonosijanKriteerit =
        jarjestyskriteeritPrioriteetitMukaan(muokattuJonosija.getJarjestyskriteerit());
    Map<Integer, JarjestyskriteeritulosDTO> jonosijanKriteerit =
        jarjestyskriteeriDtotPrioriteetitMukaan(jonosijaDTO.getJarjestyskriteerit());

    for (Integer i : muokatunJonosijanKriteerit.keySet()) {
      Jarjestyskriteeritulos muokattuJarjestyskriteeritulos = muokatunJonosijanKriteerit.get(i);
      JarjestyskriteeritulosDTO alkuperainenJarjestyskriteeritulosDTO = jonosijanKriteerit.get(i);

      if (alkuperainenJarjestyskriteeritulosDTO == null) {
        alkuperainenJarjestyskriteeritulosDTO = new JarjestyskriteeritulosDTO();
        alkuperainenJarjestyskriteeritulosDTO.setPrioriteetti(i);
        jonosijaDTO.getJarjestyskriteerit().add(alkuperainenJarjestyskriteeritulosDTO);
      }
      if (muokattuJarjestyskriteeritulos.getArvo() != null) {
        alkuperainenJarjestyskriteeritulosDTO.setArvo(muokattuJarjestyskriteeritulos.getArvo());
        jonosijaMuokattu = true;
      }
      if (muokattuJarjestyskriteeritulos.getKuvaus() != null) {
        alkuperainenJarjestyskriteeritulosDTO.setKuvaus(muokattuJarjestyskriteeritulos.getKuvaus());
        jonosijaMuokattu = true;
      }
      if (muokattuJarjestyskriteeritulos.getTila() != null) {
        alkuperainenJarjestyskriteeritulosDTO.setTila(muokattuJarjestyskriteeritulos.getTila());
        jonosijaMuokattu = true;
      }
    }
    if (jonosijaMuokattu) {
      jonosijaDTO.setMuokattu(true);
      if (safeCheckDateIsAfter(muokattuJonosija.getLastModified(), jonosijaDTO.getLastModified())) {
        jonosijaDTO.setLastModified(muokattuJonosija.getLastModified());
      }
    }
  }

  private boolean safeCheckDateIsAfter(Date date1, Date date2) {
    if (date1 != null && date2 == null) {
      return true;
    }
    if (date1 == null) {
      return false;
    }
    return date1.after(date2);
  }

  private Map<Integer, Jarjestyskriteeritulos> jarjestyskriteeritPrioriteetitMukaan(
      Collection<Jarjestyskriteeritulos> kriteerit) {
    Map<Integer, Jarjestyskriteeritulos> map = new HashMap<>();
    for (Jarjestyskriteeritulos jktulos : kriteerit) {
      map.put(jktulos.getPrioriteetti(), jktulos);
    }
    return map;
  }

  private Map<Integer, JarjestyskriteeritulosDTO> jarjestyskriteeriDtotPrioriteetitMukaan(
      Collection<JarjestyskriteeritulosDTO> kriteerit) {
    Map<Integer, JarjestyskriteeritulosDTO> map = new HashMap<>();
    for (JarjestyskriteeritulosDTO dto : kriteerit) {
      map.put(dto.getPrioriteetti(), dto);
    }
    return map;
  }

  @Override
  public List<ValintatietoValinnanvaiheDTO> haeValinnanvaiheetHakukohteelle(String hakukohdeoid) {
    List<Valinnanvaihe> valinnanVaihes = tulosValinnanvaiheDAO.readByHakukohdeOid(hakukohdeoid);
    List<ValintatietoValinnanvaiheDTO> valintatietoValinnanVaihes =
        valintatulosConverter.convertValinnanvaiheList(valinnanVaihes);
    applyMuokatutJonosijatToValinnanvaihe(hakukohdeoid, valintatietoValinnanVaihes);
    return valintatietoValinnanVaihes;
  }

  @Override
  public List<ValintatietoValinnanvaiheSiirtotiedostoDTO> haeValinnanvaiheetForSiirtotiedosto(
      List<String> valinnanvaiheOids) {
    List<Valinnanvaihe> valinnanVaihes =
        tulosValinnanvaiheDAO.readByValinnanvaiheOids(valinnanvaiheOids);
    List<ValintatietoValinnanvaiheDTO> valintatietoValinnanVaihes =
        valintatulosConverter.convertValinnanvaiheList(valinnanVaihes);
    applyMuokatutJonosijatToValinnanvaihe(valintatietoValinnanVaihes);
    return valintatulosConverter.convertValinnanvaiheListForSiirtotiedosto(
        valintatietoValinnanVaihes);
  }

  @Override
  public Optional<HakukohdeDTO> haeValinnanvaiheetHakukohteelleJaJonolle(
      String hakukohdeoid, List<String> valintatapajonot) {
    List<Valinnanvaihe> a = tulosValinnanvaiheDAO.readByHakukohdeOid(hakukohdeoid);
    List<Valinnanvaihe> result = new ArrayList<>();
    a.forEach(
        vaihe -> {
          List<Valintatapajono> jonot =
              vaihe.getValintatapajonot().stream()
                  .filter(j -> valintatapajonot.indexOf(j.getValintatapajonoOid()) != -1)
                  .collect(Collectors.toList());
          if (!jonot.isEmpty()) {
            vaihe.setValintatapajonot(jonot);
            result.add(vaihe);
          } else {
            LOGGER.warn("Yhtään jonoa ei löytynyt {}!", hakukohdeoid);
          }
        });

    if (result.isEmpty()) {
      LOGGER.warn("Yhtään valinnanvaihetta ei löytynyt {}!", hakukohdeoid);
      return Optional.empty();
    }
    List<HakukohdeDTO> b = valintatulosConverter.convertValinnanvaihe(result);
    applyMuokatutJonosijatToHakukohde(b.get(0).getHakuoid(), b);
    if (b.isEmpty()) {
      return Optional.empty();
    }
    return Optional.of(b.get(0));
  }

  @Override
  public List<HakijaryhmaDTO> haeHakijaryhmatHakukohteelle(String hakukohdeoid) {
    List<Hakijaryhma> hakijaryhmat = tulosHakijaryhmaDAO.readByHakukohdeOid(hakukohdeoid);
    return modelMapper.mapList(hakijaryhmat, HakijaryhmaDTO.class);
  }

  @Override
  public List<ValintakoeOsallistuminenDTO> haeValintakoevirheetHaulle(String hakuOid) {
    final Osallistuminen virhe = Osallistuminen.VIRHE;
    List<ValintakoeOsallistuminen> osallistumiset =
        tulosValintakoeOsallistuminenDAO.findByHakuAndOsallistuminen(hakuOid, virhe);

    Iterator<ValintakoeOsallistuminen> i = osallistumiset.iterator();
    while (i.hasNext()) {
      ValintakoeOsallistuminen vko = i.next();

      Iterator<Hakutoive> j = vko.getHakutoiveet().iterator();
      while (j.hasNext()) {
        Hakutoive ht = j.next();
        Iterator<ValintakoeValinnanvaihe> k = ht.getValintakoeValinnanvaiheet().iterator();
        while (k.hasNext()) {
          ValintakoeValinnanvaihe vv = k.next();
          Iterator<Valintakoe> l = vv.getValintakokeet().iterator();
          while (l.hasNext()) {
            Valintakoe vk = l.next();
            if (!Osallistuminen.VIRHE.equals(vk.getOsallistuminen())) {
              l.remove();
            }
          }
          if (vv.getValintakokeet().isEmpty()) {
            k.remove();
          }
        }
        if (ht.getValintakoeValinnanvaiheet().isEmpty()) {
          j.remove();
        }
      }
      if (vko.getHakutoiveet().isEmpty()) {
        i.remove();
      }
    }
    return valintatulosConverter.convertValintakoeOsallistuminen(osallistumiset);
  }

  @Override
  public List<HakukohdeDTO> haeVirheetHaulle(String hakuOid) {
    // FIXME: Suora mongo kysely tälle.
    List<Valinnanvaihe> valinnanvaiheet = tulosValinnanvaiheDAO.readByHakuOid(hakuOid);

    Iterator<Valinnanvaihe> i = valinnanvaiheet.iterator();
    while (i.hasNext()) {
      Valinnanvaihe vv = i.next();
      Iterator<Valintatapajono> j = vv.getValintatapajonot().iterator();
      while (j.hasNext()) {
        Valintatapajono jono = j.next();
        Iterator<Jonosija> k = jono.getJonosijat().iterator();
        while (k.hasNext()) {
          Jonosija jonosija = k.next();
          Iterator<Jarjestyskriteeritulos> l =
              jonosija.getJarjestyskriteeritulokset().jarjestyskriteeritulokset.iterator();
          while (l.hasNext()) {
            Jarjestyskriteeritulos jktulos = l.next();
            if (!JarjestyskriteerituloksenTila.VIRHE.equals(jktulos.getTila())) {
              l.remove();
            }
          }
          if (jonosija.getJarjestyskriteeritulokset().jarjestyskriteeritulokset.isEmpty()) {
            k.remove();
          }
        }
        if (jono.getJonosijat().isEmpty()) {
          j.remove();
        }
      }
      if (vv.getValintatapajonot().isEmpty()) {
        i.remove();
      }
    }
    Map<String, HakukohdeDTO> hakukohdeDTOtOidinMukaan = new HashMap<>();
    for (Valinnanvaihe vv : valinnanvaiheet) {
      HakukohdeDTO hakukohdeDTO = getOrCreateHakukohdeDTO(hakukohdeDTOtOidinMukaan, vv);
      hakukohdeDTO.getValinnanvaihe().add(valintatulosConverter.convertValinnanvaihe(vv));
    }
    return new ArrayList<>(hakukohdeDTOtOidinMukaan.values());
  }

  private HakukohdeDTO getOrCreateHakukohdeDTO(
      Map<String, HakukohdeDTO> hakukohdeDTOtOidinMukaan, Valinnanvaihe vv) {
    if (hakukohdeDTOtOidinMukaan.containsKey(vv.getHakukohdeOid())) {
      return hakukohdeDTOtOidinMukaan.get(vv.getHakukohdeOid());
    } else {
      HakukohdeDTO hakukohdeDTO = new HakukohdeDTO();
      hakukohdeDTO.setHakuoid(vv.getHakuOid());
      hakukohdeDTO.setOid(vv.getHakukohdeOid());
      hakukohdeDTO.setTarjoajaoid(vv.getTarjoajaOid());
      hakukohdeDTOtOidinMukaan.put(vv.getHakukohdeOid(), hakukohdeDTO);
      return hakukohdeDTO;
    }
  }

  @Override
  public List<HakukohdeDTO> haeLasketutValinnanvaiheetHaulle(String hakuOid) {
    List<HakukohdeDTO> b = getValinnanvaihesByHakukohteet(hakuOid);
    LOGGER.info("Valintatiedot kovertoitu DTO:iksi {}!", hakuOid);
    applyMuokatutJonosijatToHakukohde(hakuOid, b);
    LOGGER.info("Muokatut jonosijat liitetty {}!", hakuOid);
    b.forEach(
        hakukohde ->
            hakukohde.getHakijaryhma().addAll(haeHakijaryhmatHakukohteelle(hakukohde.getOid())));
    return b;
  }

  @Override
  public Stream<HakukohdeDTO> haeLasketutValinnanvaiheetHaulle(
      String hakuOid, Function<HakukohdeDTO, HakukohdeDTO> convertor) {
    return haeLasketutValinnanvaiheetHaulleStreamina(hakuOid).map(convertor);
  }

  private Stream<HakukohdeDTO> haeLasketutValinnanvaiheetHaulleStreamina(String hakuOid) {
    List<HakukohdeDTO> b = getValinnanvaihesByHakukohteet(hakuOid);
    LOGGER.info("Valintatiedot kovertoitu DTO:iksi {}!", hakuOid);
    applyMuokatutJonosijatToHakukohde(hakuOid, b);
    LOGGER.info("Muokatut jonosijat liitetty {}!", hakuOid);
    b.forEach(
        hakukohde ->
            hakukohde.getHakijaryhma().addAll(haeHakijaryhmatHakukohteelle(hakukohde.getOid())));
    return b.stream();
  }

  private List<HakukohdeDTO> getValinnanvaihesByHakukohteet(String hakuOid) {
    LOGGER.info("Valintatietoja haetaan kannasta {}!", hakuOid);

    long timeTotal = System.currentTimeMillis();
    List<HakukohdeDTO> hakukohdeDTOList = new ArrayList<>();
    List<SijoitteluValintatapajono> valintatapajonot =
        tulosValinnanvaiheDAO.haeValintatapajonotValinnanvaiheetSijoittelulle(hakuOid);
    LOGGER.info(
        "Sijoitteluvalintatapajonojen haussa kesti: {} ms, valinnan vaiheita löytyi {} hakukohteelle.",
        System.currentTimeMillis() - timeTotal,
        valintatapajonot.stream().map(jono -> jono.hakukohdeOid).distinct().count());
    long timeToFetchSijoitteluData = System.currentTimeMillis();
    Map<UUID, List<SijoitteluJonosija>> kriteeritValintatapajonolle =
        tulosValinnanvaiheDAO.haeJarjestyskriteerituloksetJonosijoillaHaulle(hakuOid).stream()
            .collect(Collectors.groupingBy(a -> a.valintatapajono));
    LOGGER.info(
        "Sijoittelujarjestyskriteerituloksien haussa kesti: {} ms.",
        System.currentTimeMillis() - timeToFetchSijoitteluData);

    valintatapajonot.stream()
        .collect(Collectors.groupingBy(m -> m.hakukohdeOid))
        .forEach(
            (hakukohdeOid, valinnanvaiheet) -> {
              HakukohdeDTO hakukohdeDTO = new HakukohdeDTO();
              hakukohdeDTO.setHakuoid(hakuOid);
              hakukohdeDTO.setOid(hakukohdeOid);
              valinnanvaiheet.stream()
                  .collect(Collectors.groupingBy(jono -> jono.valinnanvaiheOid))
                  .forEach(
                      (key, values) -> {
                        ValintatietoValinnanvaiheDTO vaiheDTO = new ValintatietoValinnanvaiheDTO();
                        vaiheDTO.setValinnanvaiheoid(key);
                        vaiheDTO.setJarjestysnumero(values.get(0).jarjestysnumero);
                        vaiheDTO.setHakuOid(hakuOid);
                        vaiheDTO.setNimi(values.get(0).valinnanvaiheNimi);

                        vaiheDTO.setValintatapajonot(
                            values.stream()
                                .map(
                                    jono ->
                                        valintatulosConverter.convertSijoitteluValintatapajono(
                                            jono, kriteeritValintatapajonolle.get(jono.id)))
                                .toList());
                        hakukohdeDTO.getValinnanvaihe().add(vaiheDTO);
                      });

              hakukohdeDTOList.add(hakukohdeDTO);
            });

    LOGGER.info(
        "Kokonaisaika valintatietojen haulle kesti: {} s, valintatapajonoja: {}, jonosijoja {}, jarjestyskriteerituloksia {}.",
        (System.currentTimeMillis() - timeTotal) / 1000,
        hakukohdeDTOList.stream()
            .flatMap(o -> o.getValinnanvaihe().stream())
            .map(o -> o.getValintatapajonot().size())
            .reduce(0, Integer::sum),
        hakukohdeDTOList.stream()
            .flatMap(o -> o.getValinnanvaihe().stream())
            .flatMap(o -> o.getValintatapajonot().stream())
            .map(o -> o.getJonosijat().size())
            .reduce(0, Integer::sum),
        hakukohdeDTOList.stream()
            .flatMap(o -> o.getValinnanvaihe().stream())
            .flatMap(o -> o.getValintatapajonot().stream())
            .flatMap(o -> o.getJonosijat().stream())
            .map(o -> o.getJarjestyskriteerit().size())
            .reduce(0, Integer::sum));

    return hakukohdeDTOList;
  }

  @Override
  public ValintakoeOsallistuminen haeValintakoeOsallistumiset(String hakemusOid) {
    ValintakoeOsallistuminen byHakemusOid =
        tulosValintakoeOsallistuminenDAO.findByHakemusOid(hakemusOid);
    if (byHakemusOid == null) {
      byHakemusOid = new ValintakoeOsallistuminen();
    }
    return byHakemusOid;
  }

  @Override
  public List<ValintakoeOsallistuminen> haeValintakoeOsallistumisetByHakukohdes(
      List<String> hakukohdeOids) {
    return tulosValintakoeOsallistuminenDAO.findByHakutoiveet(hakukohdeOids).stream()
        .filter(Objects::nonNull)
        .collect(Collectors.toList());
  }

  @Override
  public List<ValintakoeOsallistuminen> haeValintakoeOsallistumisetByHakijas(
      List<String> hakijaOids) {
    return tulosValintakoeOsallistuminenDAO.findByHakijaOids(hakijaOids).stream()
        .filter(Objects::nonNull)
        .collect(Collectors.toList());
  }

  @Override
  public List<ValintakoeOsallistuminen> haeValintakoeOsallistumisetByHakutoive(
      String hakukohdeOid) {
    return tulosValintakoeOsallistuminenDAO.findByHakutoive(hakukohdeOid);
  }

  @Override
  public void asetaHarkinnanvaraisestiHyvaksymisenTila(
      String hakuoid,
      String hakukohdeoid,
      String hakemusoid,
      HarkinnanvaraisuusTila tila,
      User auditUser) {
    HarkinnanvarainenHyvaksyminen a =
        harkinnanvarainenHyvaksyminenDAO.haeHarkinnanvarainenHyvaksyminen(hakukohdeoid, hakemusoid);
    if (a == null) {
      a = new HarkinnanvarainenHyvaksyminen();
      a.setHakemusOid(hakemusoid);
      a.setHakukohdeOid(hakukohdeoid);
      a.setHakuOid(hakuoid);
    }
    a.setHarkinnanvaraisuusTila(tila);
    saveHarkinnanvarainenHyvaksyminen(a, auditUser);
  }

  private void saveHarkinnanvarainenHyvaksyminen(
      HarkinnanvarainenHyvaksyminen harkinnanvarainenHyvaksyminen, User auditUser) {
    auditLog.log(
        LaskentaAudit.AUDIT,
        auditUser,
        ValintaperusteetOperation.HARKINNANVARAINEN_HYVAKSYMINEN_PAIVITYS,
        ValintaResource.HARKINNANVARAINEN_HYVAKSYMINEN,
        harkinnanvarainenHyvaksyminen.getHakemusOid(),
        Changes.addedDto(harkinnanvarainenHyvaksyminen));
    harkinnanvarainenHyvaksyminenDAO.tallennaHarkinnanvarainenHyvaksyminen(
        harkinnanvarainenHyvaksyminen);
  }

  @Override
  public List<HarkinnanvarainenHyvaksyminen> haeHarkinnanvaraisestiHyvaksymisenTila(
      String hakukohdeoid) {
    return harkinnanvarainenHyvaksyminenDAO.haeHarkinnanvarainenHyvaksyminen(hakukohdeoid);
  }

  @Override
  public List<HarkinnanvarainenHyvaksyminen> haeHakemuksenHarkinnanvaraisestiHyvaksymisenTilat(
      String hakuOid, String hakukohdeoid) {
    return harkinnanvarainenHyvaksyminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakukohdeoid);
  }

  @Override
  public ValinnanvaiheDTO lisaaTuloksia(
      ValinnanvaiheDTO vaihe, String hakukohdeoid, String tarjoajaOid) {
    Valinnanvaihe haettu = tulosValinnanvaiheDAO.haeValinnanvaihe(vaihe.getValinnanvaiheoid());
    Valinnanvaihe annettu = modelMapper.map(vaihe, Valinnanvaihe.class);
    annettu.setHakukohdeOid(hakukohdeoid);
    if (haettu == null) {
      tulosValinnanvaiheDAO.saveOrUpdate(annettu);
    } else {
      List<Valintatapajono> vanhat = new ArrayList<Valintatapajono>();
      for (Valintatapajono jono : haettu.getValintatapajonot()) {
        boolean mukana = false;
        for (Valintatapajono uusi : annettu.getValintatapajonot()) {
          if (uusi.getValintatapajonoOid().equals(jono.getValintatapajonoOid())) {
            mukana = true;
          }
        }
        if (!mukana) {
          vanhat.add(jono);
        }
      }
      annettu.getValintatapajonot().addAll(vanhat);
      haettu.setHakukohdeOid(hakukohdeoid);
      haettu.setHakuOid(vaihe.getHakuOid());
      haettu.setTarjoajaOid(tarjoajaOid);
      haettu.setValintatapajonot(annettu.getValintatapajonot());
      tulosValinnanvaiheDAO.saveOrUpdate(haettu);
    }
    return vaihe;
  }

  @Override
  public Optional<Valintatapajono> muokkaaValintatapajonoa(
      String valintatapajonoOid, Consumer<Valintatapajono> muokkausFunktio, User auditUser) {
    if (haeSijoitteluStatus(valintatapajonoOid) && !isOPH()) {
      throw new EiOikeuttaPoistaaValintatapajonoaSijoittelustaException(
          "Ei oikeutta muokata valintatapajonoa");
    }
    Valinnanvaihe vaihe = tulosValinnanvaiheDAO.findByValintatapajonoOid(valintatapajonoOid);
    vaihe.getValintatapajonot().stream()
        .filter(j -> j.getValintatapajonoOid().equals(valintatapajonoOid))
        .forEach(j -> muokkausFunktio.accept(j));
    tulosValinnanvaiheDAO.saveOrUpdate(vaihe);
    return vaihe.getValintatapajonot().stream()
        .filter(j -> j.getValintatapajonoOid().equals(valintatapajonoOid))
        .findFirst();
  }

  @Override
  public boolean haeSijoitteluStatus(String valintatapajonoOid) {
    Valinnanvaihe vaihe = tulosValinnanvaiheDAO.findByValintatapajonoOid(valintatapajonoOid);
    if (vaihe == null) {
      return false;
    }
    return vaihe.getValintatapajonot().stream()
        .filter(j -> j.getValintatapajonoOid().equals(valintatapajonoOid))
        .allMatch(Valintatapajono::getValmisSijoiteltavaksi);
  }

  /** Muokattu jonosija works in mysterious ways. */
  @Override
  public MuokattuJonosija muutaJarjestyskriteeri(
      String valintatapajonoOid,
      String hakemusOid,
      Integer jarjestyskriteeriPrioriteetti,
      MuokattuJonosijaArvoDTO jonosija,
      User auditUser) {
    Valinnanvaihe valinnanvaihe =
        tulosValinnanvaiheDAO.findByValintatapajonoOid(valintatapajonoOid);
    MuokattuJonosija muokattuJonosija =
        muokattuJonosijaDAO.readByValintatapajonoOid(valintatapajonoOid, hakemusOid);
    if (muokattuJonosija == null) {
      muokattuJonosija = new MuokattuJonosija();
    }
    muokattuJonosija.setHakemusOid(hakemusOid);
    muokattuJonosija.setValintatapajonoOid(valintatapajonoOid);
    muokattuJonosija.setHakuOid(valinnanvaihe.getHakuOid());
    muokattuJonosija.setHakukohdeOid(valinnanvaihe.getHakukohdeOid());

    Jarjestyskriteeritulos jarjestyskriteeritulos = null;

    for (Jarjestyskriteeritulos tulos : muokattuJonosija.getJarjestyskriteerit()) {
      if (tulos.getPrioriteetti() == jarjestyskriteeriPrioriteetti) {
        jarjestyskriteeritulos = tulos;
      }
    }
    if (jarjestyskriteeritulos == null) {
      jarjestyskriteeritulos = new Jarjestyskriteeritulos();
      jarjestyskriteeritulos.setPrioriteetti(jarjestyskriteeriPrioriteetti);
      muokattuJonosija.getJarjestyskriteerit().add(jarjestyskriteeritulos);
    }

    Map<String, String> muokattuKasin = new HashMap<>();
    if (jonosija.getSelite() != null && !jonosija.getSelite().isEmpty()) {
      muokattuKasin.put("FI", jonosija.getSelite());
    } else {
      muokattuKasin.put("FI", "Muokattu käsin");
    }

    jarjestyskriteeritulos.setKuvaus(muokattuKasin);
    jarjestyskriteeritulos.setArvo(jonosija.getArvo());
    jarjestyskriteeritulos.setTila(jonosija.getTila());

    muokattuJonosija.setMuokkaaja(AuthorizationUtil.getCurrentUser());
    muokattuJonosija.setSelite(jonosija.getSelite());
    muokattuJonosija.setMuutos(
        "jarjestyskriteeriPrioriteetti: "
            + jarjestyskriteeriPrioriteetti
            + " arvo: "
            + jonosija.getArvo()
            + " tila: "
            + jonosija.getTila().name());

    saveMuokattuJonosija(muokattuJonosija, auditUser);
    return muokattuJonosija;
  }

  @Override
  public MuokattuJonosija poistaMuokattuJonosija(
      String valintatapajonoOid,
      String hakemusOid,
      Integer jarjestyskriteeriPrioriteetti,
      User auditUser) {
    MuokattuJonosija muokattuJonosija;
    muokattuJonosija = muokattuJonosijaDAO.readByValintatapajonoOid(valintatapajonoOid, hakemusOid);
    if (muokattuJonosija == null) {
      return null;
    } else {
      Set<Jarjestyskriteeritulos> saastettavat =
          muokattuJonosija.getJarjestyskriteerit().stream()
              .filter(j -> j.getPrioriteetti() != jarjestyskriteeriPrioriteetti)
              .collect(Collectors.toSet());
      muokattuJonosija.setJarjestyskriteerit(saastettavat);
      saveMuokattuJonosija(muokattuJonosija, auditUser);
      return muokattuJonosija;
    }
  }

  @Override
  public Optional<Valintatapajono> paivitaValmisSijoiteltavaksi(
      @Param("valintatapajonoOid") String valintatapajonoOid,
      @Param("valmisSijoiteltavaksi") boolean valmisSijoiteltavaksi) {
    return tulosValintatapajonoDAO.paivitaValmisSijoiteltavaksi(
        valintatapajonoOid, valmisSijoiteltavaksi);
  }

  private void saveMuokattuJonosija(MuokattuJonosija muokattuJonosija, User auditUser) {
    auditLog.log(
        LaskentaAudit.AUDIT,
        auditUser,
        ValintaperusteetOperation.JONOSIJA_PAIVITYS,
        ValintaResource.JONOSIJA,
        muokattuJonosija.getHakemusOid(),
        Changes.addedDto(muokattuJonosija));
    muokattuJonosijaDAO.saveOrUpdate(muokattuJonosija);
  }

  private boolean isOPH() {
    Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
    for (GrantedAuthority authority : authentication.getAuthorities()) {
      if (authority.getAuthority().contains(rootOrgOid)) {
        return true;
      }
    }
    return false;
  }
}
