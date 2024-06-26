package fi.vm.sade.valintalaskenta.tulos.service.impl.rest;

import fi.vm.sade.service.valintaperusteet.dto.*;
import fi.vm.sade.valintalaskenta.domain.dto.*;
import fi.vm.sade.valintalaskenta.domain.dto.AvainArvoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.ValintatapajonoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.*;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Hakutoive;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Valintakoe;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeValinnanvaihe;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import fi.vm.sade.valintalaskenta.tulos.service.impl.ValintatietoService;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/** User: kkammone Date: 29.4.2013 Time: 13:24 */
@Service
public class ValintatietoServiceImpl implements ValintatietoService {

  private static final Logger LOG = LoggerFactory.getLogger(ValintatietoServiceImpl.class);
  @Autowired private ValintalaskentaTulosService tulosService;

  @Autowired private ValintalaskentaModelMapper modelMapper;

  public List<HakemusOsallistuminenDTO> haeValintatiedotHakukohteelle(
      List<String> valintakoeTunnisteet, String hakukohdeOid) {
    List<HakemusOsallistuminenDTO> osallistumiset = new ArrayList<HakemusOsallistuminenDTO>();
    try {
      List<ValintakoeOsallistuminen> valinnanvaiheet =
          tulosService.haeValintakoeOsallistumisetByHakutoive(hakukohdeOid);
      Set<String> tunnisteet = new HashSet<String>(valintakoeTunnisteet);
      for (ValintakoeOsallistuminen koetulos : valinnanvaiheet) {
        for (Hakutoive hakutoive : koetulos.getHakutoiveet()) {
          for (ValintakoeValinnanvaihe vaihe : hakutoive.getValintakoeValinnanvaiheet()) {
            HakemusOsallistuminenDTO h = new HakemusOsallistuminenDTO();
            h.setHakukohdeOid(hakutoive.getHakukohdeOid());
            for (Valintakoe valintakoe : vaihe.getValintakokeet()) {
              if (tunnisteet.contains(valintakoe.getValintakoeTunniste())) {
                ValintakoeOsallistuminenDTO osallistuminen = new ValintakoeOsallistuminenDTO();
                osallistuminen.setOsallistuminen(
                    modelMapper.map(valintakoe.getOsallistuminen(), OsallistuminenDTO.class));
                osallistuminen.setValintakoeOid(valintakoe.getValintakoeOid());
                osallistuminen.setValintakoeTunniste(valintakoe.getValintakoeTunniste());
                osallistuminen.setNimi(valintakoe.getNimi());
                h.getOsallistumiset().add(osallistuminen);
              }
            }
            // lisataan tulosjoukkoon vaan jos valinnanvaiheessa oli
            // valintakoe hakemukselle!
            if (!h.getOsallistumiset().isEmpty()) {
              h.setLuontiPvm(koetulos.getCreatedAt());
              h.setHakemusOid(koetulos.getHakemusOid());
              osallistumiset.add(h);
            }
          }
        }
      }
    } catch (Exception e) {
      LOG.error("Valintakoelaskennan osallitujia ei saatu haettua hakukohteelle {}!", hakukohdeOid);
      LOG.error("Virhe osallistujien hakemisessa!", e);
      throw new RuntimeException("Valintatieto osallistujille pyyntö epäonnistui!", e);
    }
    return osallistumiset;
  }

  public HakuDTO haeValintatiedot(String hakuOid) {
    try {
      Function<HakukohdeDTO, HakukohdeDTO> convertor =
          (HakukohdeDTO v) -> {
            HakukohdeDTO ht = new HakukohdeDTO();
            ht.setOid(v.getOid());
            ht.setTarjoajaoid(v.getTarjoajaoid());
            ht.getHakijaryhma().addAll(v.getHakijaryhma());

            for (ValinnanvaiheDTO valinnanvaiheDTO : v.getValinnanvaihe()) {
              ht.getValinnanvaihe()
                  .add(createValinnanvaiheTyyppi(valinnanvaiheDTO, Optional.empty()));
            }
            return ht;
          };

      Stream<HakukohdeDTO> convertedHakukohdeDTOs =
          tulosService.haeLasketutValinnanvaiheetHaulle(hakuOid, convertor);

      HakuDTO hakuDTO = new HakuDTO();
      hakuDTO.setHakuOid(hakuOid);
      convertedHakukohdeDTOs.forEach(hakukohdeDTO -> hakuDTO.getHakukohteet().add(hakukohdeDTO));
      return hakuDTO;
    } catch (Exception e) {
      LOG.error("Valintatietoja ei saatu haettua haulle {}!", hakuOid);
      LOG.error("Virhe valintatietojen hakemisessa!", e);
      throw new RuntimeException("Valintatietojen haku epäonnistui!", e);
    }
  }

  @Override
  public HakuDTO haeValintatiedotJonoille(
      String hakuoid,
      Map<String, List<String>> jonot,
      Optional<Map<String, List<fi.vm.sade.service.valintaperusteet.dto.ValintatapajonoDTO>>>
          valintaperusteet) {
    try {
      List<HakukohdeDTO> kohteet = new ArrayList<>();
      jonot.keySet().parallelStream()
          .forEach(
              oid -> {
                Optional<HakukohdeDTO> kohde =
                    tulosService.haeValinnanvaiheetHakukohteelleJaJonolle(oid, jonot.get(oid));

                kohde.ifPresent(kohteet::add);
              });
      HakuDTO hakuDTO = new HakuDTO();
      hakuDTO.setHakuOid(hakuoid);

      for (HakukohdeDTO v : kohteet) {
        if (v != null) {
          HakukohdeDTO ht = new HakukohdeDTO();
          ht.setOid(v.getOid());
          ht.setTarjoajaoid(v.getTarjoajaoid());
          hakuDTO.getHakukohteet().add(ht);

          for (ValinnanvaiheDTO valinnanvaiheDTO : v.getValinnanvaihe()) {
            if (valinnanvaiheDTO != null) {
              ht.getValinnanvaihe()
                  .add(
                      createValinnanvaiheTyyppi(
                          valinnanvaiheDTO, valintaperusteet.map(m -> m.get(v.getOid()))));
            }
          }
        }
      }
      return hakuDTO;
    } catch (Exception e) {
      LOG.error("Valintatietoja ei saatu haettua haulle {}!", hakuoid);
      LOG.error("Virhe valintatietojen hakemisessa!", e);
      throw new RuntimeException("Valintatietojen haku epäonnistui!", e);
    }
  }

  private ValintatietoValinnanvaiheDTO createValinnanvaiheTyyppi(
      ValinnanvaiheDTO valinnanvaihe,
      Optional<List<fi.vm.sade.service.valintaperusteet.dto.ValintatapajonoDTO>> valintaperusteet) {
    ValintatietoValinnanvaiheDTO v = new ValintatietoValinnanvaiheDTO();
    v.setValinnanvaihe(valinnanvaihe.getJarjestysnumero());
    v.setValinnanvaiheoid(valinnanvaihe.getValinnanvaiheoid());
    v.setHakuOid(valinnanvaihe.getHakuOid());
    for (ValintatapajonoDTO vt : valinnanvaihe.getValintatapajonot()) {
      Optional<fi.vm.sade.service.valintaperusteet.dto.ValintatapajonoDTO> perusteet =
          valintaperusteet.flatMap(
              l -> l.stream().filter(vtj -> vtj.getOid().equals(vt.getOid())).findAny());
      v.getValintatapajonot().add(createValintatapajonoTyyppi(vt, perusteet));
    }
    return v;
  }

  private ValintatietoValintatapajonoDTO createValintatapajonoTyyppi(
      ValintatapajonoDTO vt,
      Optional<fi.vm.sade.service.valintaperusteet.dto.ValintatapajonoDTO> valintaperusteet) {
    ValintatietoValintatapajonoDTO dto = new ValintatietoValintatapajonoDTO();
    dto.setValintatapajonooid(vt.getValintatapajonooid());
    dto.setAloituspaikat(vt.getAloituspaikat());
    dto.setNimi(vt.getNimi());
    dto.setPrioriteetti(vt.getPrioriteetti());
    dto.setSiirretaanSijoitteluun(vt.isSiirretaanSijoitteluun());
    dto.setValmisSijoiteltavaksi(vt.getValmisSijoiteltavaksi());
    dto.setAktiivinen(vt.getAktiivinen());
    dto.setEiVarasijatayttoa(vt.getEiVarasijatayttoa());
    dto.setKaikkiEhdonTayttavatHyvaksytaan(vt.getKaikkiEhdonTayttavatHyvaksytaan());
    dto.setKaytetaanValintalaskentaa(vt.getKaytetaanValintalaskentaa());
    dto.setTasasijasaanto(vt.getTasasijasaanto());
    valintaperusteet.ifPresent(
        perusteet -> {
          dto.setTayttojono(perusteet.getTayttojono());
          dto.setVarasijat(perusteet.getVarasijat());
          dto.setVarasijaTayttoPaivat(perusteet.getVarasijaTayttoPaivat());
          dto.setVarasijojaKaytetaanAlkaen(perusteet.getVarasijojaKaytetaanAlkaen());
          dto.setVarasijojaTaytetaanAsti(perusteet.getVarasijojaTaytetaanAsti());
          dto.setPoissaOlevaTaytto(perusteet.getPoissaOlevaTaytto());
        });

    for (JonosijaDTO jonosija : vt.getJonosijat()) {
      HakijaDTO ht = new HakijaDTO();
      ht.setPrioriteetti(jonosija.getPrioriteetti());

      if (jonosija.getTuloksenTila() == null) {
        ht.setTila(JarjestyskriteerituloksenTilaDTO.MAARITTELEMATON);
      } else {
        ht.setTila(JarjestyskriteerituloksenTilaDTO.valueOf(jonosija.getTuloksenTila().name()));
      }
      ht.setHakemusOid(jonosija.getHakemusOid());
      ht.setEtunimi(jonosija.getEtunimi());
      ht.setSukunimi(jonosija.getSukunimi());
      ht.setOid(jonosija.getHakijaOid());
      ht.setJonosija(jonosija.getJonosija());
      for (SyotettyArvoDTO sa : jonosija.getSyotetytArvot()) {
        ht.getSyotettyArvo().add(sa);
      }

      if (jonosija.isHarkinnanvarainen()) {
        ht.setHarkinnanvarainen(Boolean.TRUE);
      }

      if (jonosija.getJarjestyskriteerit().size() > 0) {
        ht.setPisteet(jonosija.getJarjestyskriteerit().first().getArvo());
      }

      if (jonosija.getJarjestyskriteerit().size() > 0
          && !jonosija.getJarjestyskriteerit().first().getKuvaus().isEmpty()) {
        Map<String, String> kuvaus = jonosija.getJarjestyskriteerit().first().getKuvaus();
        ht.setTilanKuvaus(
            kuvaus.keySet().stream()
                .map(
                    k -> {
                      AvainArvoDTO a = new AvainArvoDTO();
                      a.setAvain(k);
                      a.setArvo(kuvaus.getOrDefault(k, ""));
                      return a;
                    })
                .collect(Collectors.toList()));
      }

      ht.setHylattyValisijoittelussa(jonosija.isHylattyValisijoittelussa());

      dto.getHakija().add(ht);
    }
    return dto;
  }
}
