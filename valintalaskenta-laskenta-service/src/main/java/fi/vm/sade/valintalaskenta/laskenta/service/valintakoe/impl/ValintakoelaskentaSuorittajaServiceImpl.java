package fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.impl;

import fi.vm.sade.service.valintaperusteet.dto.HakukohteenValintaperusteDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintakoeDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetValinnanVaiheDTO;
import fi.vm.sade.service.valintaperusteet.dto.model.Koekutsu;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakemus;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakukohde;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Hyvaksyttavissatila;
import fi.vm.sade.service.valintaperusteet.model.Funktiokutsu;
import fi.vm.sade.valintalaskenta.domain.dto.AvainArvoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakukohdeDTO;
import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Hakutoive;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.OsallistuminenTulos;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Valintakoe;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeValinnanvaihe;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValinnanvaiheDAO;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValintakoeOsallistuminenDAO;
import fi.vm.sade.valintalaskenta.laskenta.resource.ValintakoelaskennanKumulatiivisetTulokset;
import fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion.HakemusDTOToHakemusConverter;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.EdellinenValinnanvaiheKasittelija;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.TilaJaSelite;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.ValintakoelaskentaSuorittajaService;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.Valintakoeosallistumislaskin;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.impl.util.HakukohdeValintakoeData;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class ValintakoelaskentaSuorittajaServiceImpl
    implements ValintakoelaskentaSuorittajaService {
  private final String r = "\\{\\{([A-Za-z0–9\\-_]+)\\.([A-Za-z0–9\\-_]+)\\}\\}";
  private final Pattern pattern = Pattern.compile(r);
  private static final Logger LOG =
      LoggerFactory.getLogger(ValintakoelaskentaSuorittajaServiceImpl.class);
  public static final String VALINNANVAIHE_HAKIJAN_VALINTA = "valinnanVaiheHakijanValinta";

  private final HakemusDTOToHakemusConverter hakemusConverter;
  private final ValintakoeOsallistuminenDAO valintakoeOsallistuminenDAO;
  private final Valintakoeosallistumislaskin valintakoeosallistumislaskin;
  private final ValinnanvaiheDAO valinnanvaiheDAO;
  private final EdellinenValinnanvaiheKasittelija edellinenValinnanvaiheKasittelija;
  private final ValintalaskentaModelMapper modelMapper;

  @Autowired
  public ValintakoelaskentaSuorittajaServiceImpl(
      ValintalaskentaModelMapper modelMapper,
      HakemusDTOToHakemusConverter hakemusConverter,
      EdellinenValinnanvaiheKasittelija edellinenValinnanvaiheKasittelija,
      ValintakoeOsallistuminenDAO valintakoeOsallistuminenDAO,
      Valintakoeosallistumislaskin valintakoeosallistumislaskin,
      ValinnanvaiheDAO valinnanvaiheDAO) {
    this.modelMapper = modelMapper;
    this.hakemusConverter = hakemusConverter;
    this.edellinenValinnanvaiheKasittelija = edellinenValinnanvaiheKasittelija;
    this.valintakoeOsallistuminenDAO = valintakoeOsallistuminenDAO;
    this.valintakoeosallistumislaskin = valintakoeosallistumislaskin;
    this.valinnanvaiheDAO = valinnanvaiheDAO;
  }

  private String haeTunniste(String mustache, Map<String, String> hakukohteenValintaperusteet) {
    final Matcher m = pattern.matcher(mustache);
    String avain = null;
    while (m.find()) {
      if (!m.group(1).isEmpty() && m.group(1).contentEquals("hakukohde") && !m.group(2).isEmpty()) {
        avain = m.group(2);
      }
    }
    if (avain == null) {
      return mustache;
    } else {
      return hakukohteenValintaperusteet.get(avain);
    }
  }

  @Override
  public void laske(
      HakemusDTO hakemus,
      List<ValintaperusteetDTO> valintaperusteet,
      Map<String, Map<Integer, Valinnanvaihe>> valinnanvaiheidenEsihaetutTulokset,
      String uuid,
      ValintakoelaskennanKumulatiivisetTulokset kumulatiivisetTulokset,
      boolean korkeakouluhaku) {
    LOG.info(
        "(Uuid={}) (Thread={}) Laskentaan valintakoeosallistumiset hakemukselle {}",
        uuid,
        Thread.currentThread(),
        hakemus.getHakemusoid());
    if (valintaperusteet.size() == 0) {
      return;
    }
    final Map<String, HakukohdeDTO> hakutoiveetByOid = luoHakutoiveMap(hakemus.getHakukohteet());
    LOG.debug(
        String.format(
            "Hakijan %s hakemukselle hakutoiveetByOid.size() == %d",
            hakemus.getHakijaOid(), hakutoiveetByOid.size()));
    Map<String, List<HakukohdeValintakoeData>> valintakoeData = new HashMap<>();
    String hakuOid = valintaperusteet.get(0).getHakuOid();
    ValintakoeOsallistuminen vanhatOsallistumiset =
        valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemus.getHakemusoid());
    poistaVanhatOsallistumiset(hakemus, valintaperusteet);
    LOG.debug(
        String.format(
            "Hakijan %s hakemukselle valintaperusteet.size() == %d",
            hakemus.getHakijaOid(), valintaperusteet.size()));
    for (ValintaperusteetDTO vp : valintaperusteet) {
      Map<String, String> hakukohteenValintaperusteet =
          muodostaHakukohteenValintaperusteetMap(vp.getHakukohteenValintaperuste());
      if (hakutoiveetByOid.containsKey(vp.getHakukohdeOid())
          && !vp.getValinnanVaihe().getValintakoe().isEmpty()) {
        ValintaperusteetValinnanVaiheDTO vaihe = vp.getValinnanVaihe();
        int edellisenVaiheenJarjestysluku = vaihe.getValinnanVaiheJarjestysluku() - 1;
        for (ValintakoeDTO koe : vaihe.getValintakoe()) {
          if (koe.getAktiivinen()) {
            String tunniste = haeTunniste(koe.getTunniste(), hakukohteenValintaperusteet);
            if (tunniste == null) {
              LOG.error(
                  "(Uuid={}) Valintakokoeen tunnistetta ei pystytty määrittelemään. HakukohdeOid: {} - ValintakoeOid: {}",
                  uuid,
                  vp.getHakukohdeOid(),
                  koe.getOid());
              continue;
            }
            valintakoeData.putIfAbsent(tunniste, new ArrayList<>());

            Map<Integer, Valinnanvaihe> hakukohteenTulokset =
                valinnanvaiheidenEsihaetutTulokset.getOrDefault(
                    vp.getHakukohdeOid(), new HashMap<>());

            Valinnanvaihe edellinenVaihe = null;
            if (edellisenVaiheenJarjestysluku >= 0) {
              edellinenVaihe = hakukohteenTulokset.get(edellisenVaiheenJarjestysluku);
              if (edellinenVaihe == null) {
                LOG.error(
                    "FIXME Aiempia tuloksia ei löytynyt, vaikka oletettavasti pitäisi! "
                        + "Yritetään vielä hakea vanhalla tavalla edellisen vaiheen tulokset.");
                edellinenVaihe =
                    valinnanvaiheDAO.haeEdeltavaValinnanvaihe(
                        vp.getHakuOid(),
                        vp.getHakukohdeOid(),
                        vaihe.getValinnanVaiheJarjestysluku());
              }
            }
            if (invalidEdellinenValinnanVaihe(uuid, vp, vaihe, edellinenVaihe)) {
              LOG.warn(
                  "Invalid edellinen valinnan vaihe! "
                      + edellisenVaiheenJarjestysluku
                      + " hakukohteessa "
                      + vp.getHakukohdeOid());
              continue;
            }
            Valinnanvaihe viimeisinValinnanVaihe =
                getViimeisinValinnanvaihe(vp, vaihe, edellinenVaihe);
            OsallistuminenTulos osallistuminen =
                getOsallistuminenTulos(
                    hakemus,
                    vp,
                    hakukohteenValintaperusteet,
                    koe,
                    viimeisinValinnanVaihe,
                    korkeakouluhaku,
                    vanhatOsallistumiset);
            HakukohdeValintakoeData data =
                getHakukohdeValintakoeData(hakemus, uuid, vp, vaihe, koe, tunniste);
            data.setOsallistuminenTulos(osallistuminen);
            valintakoeData.get(tunniste).add(data);
          }
        }
      }
    }
    Map<String, ValintakoeOsallistuminen> osallistumisetByHaku = new HashMap<>();
    for (Map.Entry<String, List<HakukohdeValintakoeData>> entry : valintakoeData.entrySet()) {
      List<HakukohdeValintakoeData> kokeet = entry.getValue();
      List<HakukohdeValintakoeData> olemassaOlevat = new ArrayList<>();
      LOG.debug(String.format("Käsitellään %d koetta", kokeet.size()));
      for (HakukohdeValintakoeData c : kokeet) {
        ValintakoeOsallistuminen valintakoeOsallistuminen =
            luoValintakoeOsallistuminen(c, hakemus, hakutoiveetByOid);
        if (LOG.isDebugEnabled()) {
          LOG.debug(
              String.format(
                  "\tvalintakoeOsallistuminen == %s",
                  ToStringBuilder.reflectionToString(valintakoeOsallistuminen)));
        }
        osallistumisetByHaku.putIfAbsent(c.getHakuOid(), valintakoeOsallistuminen);
        olemassaOlevat.add(c);
        addValintaKokeetByMatchingTunnisteAndDifferentValintakoeOid(
            osallistumisetByHaku, olemassaOlevat, c);
      }
      asetaOsallistumisetKokeisiin(olemassaOlevat, hakutoiveetByOid);
      for (HakukohdeValintakoeData c : olemassaOlevat) {
        ValintakoeOsallistuminen osallistuminen = osallistumisetByHaku.get(c.getHakuOid());
        haeTaiLuoHakutoive(osallistuminen, c);
      }
    }
    for (ValintakoeOsallistuminen osallistuminen : osallistumisetByHaku.values()) {
      if (LOG.isDebugEnabled()) {
        LOG.debug(
            String.format(
                "Tallennetaan %s (hakutoiveet: %s)",
                ToStringBuilder.reflectionToString(osallistuminen),
                osallistuminen.getHakutoiveet().stream()
                    .map(ToStringBuilder::reflectionToString)
                    .collect(Collectors.toList())));
        debugLogitaKoetiedot(osallistuminen);
      }
      kumulatiivisetTulokset.add(osallistuminen);
    }

    ValintakoeOsallistuminen osallistuminenJohonOnYhdistettyKokoLaskentaAjonOsallistumiset =
        kumulatiivisetTulokset.get(hakemus.getHakemusoid());

    if (osallistuminenJohonOnYhdistettyKokoLaskentaAjonOsallistumiset != null) {
      saveValintakoeOsallistuminen(osallistuminenJohonOnYhdistettyKokoLaskentaAjonOsallistumiset);
    }

    if (LOG.isDebugEnabled()) {
      LOG.debug("Tallentamisen jälkeen:");
      ValintakoeOsallistuminen tallennettuOsallistuminen =
          valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(
              hakemus.getHakuoid(), hakemus.getHakemusoid());
      debugLogitaKoetiedot(tallennettuOsallistuminen);
    }
  }

  // Siivotaan sellaiset valinnanvaiheet, jotka eivät löydy säästettävien listalta.
  // Huom: VALINNANVAIHE_HAKIJAN_VALINTA on erikoistapaus, jossa luodaan keinotekoinen
  // valinnanvaiheOid valintakoeosallistumiselle.
  // Tämä oid ei siis tule valintaperusteista kuten muut vv-oidit, vaan syntyy tietyssä tilanteessa
  // valintakoelaskennan aikana.
  @Override
  public void siivoaValintakoeOsallistumiset(
      List<HakemusDTO> hakemukset,
      String hakukohdeOid,
      List<String> saastettavienValinnanvaiheidenOidit) {
    saastettavienValinnanvaiheidenOidit.add(VALINNANVAIHE_HAKIJAN_VALINTA);
    LOG.info(
        "Tutkitaan, löytyykö {} hakemuksen valintakoeOsallistumisista valinnanvaiheOideja, jotka eivät kuulu säästettäviin ({}) hakukohteessa {}",
        hakemukset.size(),
        saastettavienValinnanvaiheidenOidit,
        hakukohdeOid);
    hakemukset.parallelStream()
        .forEach(
            hakemus -> {
              ValintakoeOsallistuminen tallennettuOsallistuminen =
                  valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(
                      hakemus.getHakuoid(), hakemus.getHakemusoid());
              if (tallennettuOsallistuminen != null) {
                tallennettuOsallistuminen
                    .getHakutoiveet()
                    .forEach(
                        ht -> {
                          if (ht.getHakukohdeOid().equals(hakukohdeOid)) {
                            List<ValintakoeValinnanvaihe> siivotutValinnanvaiheet =
                                ht.getValintakoeValinnanvaiheet().stream()
                                    .filter(
                                        vv ->
                                            saastettavienValinnanvaiheidenOidit.contains(
                                                vv.getValinnanvaiheOid()))
                                    .toList();
                            int ennen = ht.getValintakoeValinnanvaiheet().size();
                            int jalkeen = siivotutValinnanvaiheet.size();
                            if (ennen > jalkeen) {
                              LOG.warn(
                                  "Siivottiin hakemuksen {} valinnanvaiheet hakukohteessa {}. Ennen siivousta {} kpl, jälkeen {} kpl.",
                                  hakemus.getHakemusoid(),
                                  hakukohdeOid,
                                  ennen,
                                  jalkeen);
                              ht.getValintakoeValinnanvaiheet()
                                  .forEach(
                                      vv -> {
                                        if (!saastettavienValinnanvaiheidenOidit.contains(
                                            vv.getValinnanvaiheOid())) {
                                          LOG.warn(
                                              "Poistetaan hakemuksen {} hakutoiveen {} valintakoeosallistumisista valinnanvaihe oidilla {}, "
                                                  + "koska se vaikuttaa kadonneen valintaperusteista.",
                                              hakemus.getHakemusoid(),
                                              hakukohdeOid,
                                              vv.getValinnanvaiheOid());
                                        }
                                      });
                              ht.setValintakoeValinnanvaiheet(
                                  new HashSet<>(siivotutValinnanvaiheet));
                              valintakoeOsallistuminenDAO.createOrUpdate(tallennettuOsallistuminen);
                            }
                          }
                        });
              } else {
                LOG.debug(
                    "Ilmeisesti hakemukselle {} ei ole vielä valintakoeosallistumisia.",
                    hakemus.getHakemusoid());
              }
            });
  }

  private void debugLogitaKoetiedot(ValintakoeOsallistuminen osallistuminen) {
    if (osallistuminen == null) {
      LOG.debug("osallistuminen == null");
      return;
    }
    osallistuminen
        .getHakutoiveet()
        .forEach(
            hakutoive -> {
              LOG.debug(String.format("\thakutoive %s : ", hakutoive.getHakukohdeOid()));
              hakutoive
                  .getValintakoeValinnanvaiheet()
                  .forEach(
                      valintakoeValinnanvaihe -> {
                        LOG.debug(
                            String.format(
                                "\t\tvalinnanvaihe %s : ",
                                valintakoeValinnanvaihe.getValinnanvaiheOid()));
                        valintakoeValinnanvaihe
                            .getValintakokeet()
                            .forEach(
                                valintakoe ->
                                    LOG.debug(
                                        String.format(
                                            "\t\t\tvalintakoe %s : %s",
                                            valintakoe.getValintakoeTunniste(),
                                            ToStringBuilder.reflectionToString(valintakoe))));
                      });
            });
  }

  private void addValintaKokeetByMatchingTunnisteAndDifferentValintakoeOid(
      Map<String, ValintakoeOsallistuminen> osallistumisetByHaku,
      List<HakukohdeValintakoeData> olemassaOlevat,
      HakukohdeValintakoeData c) {
    ValintakoeOsallistuminen osallistuminen = osallistumisetByHaku.get(c.getHakuOid());
    osallistuminen
        .getHakutoiveet()
        .forEach(
            h ->
                h.getValintakoeValinnanvaiheet()
                    .forEach(
                        v ->
                            v.getValintakokeet()
                                .forEach(
                                    koe -> {
                                      if (koe.getValintakoeTunniste()
                                              .equals(c.getValintakoeTunniste())
                                          && !koe.getValintakoeOid().equals(c.getValintakoeOid())) {
                                        HakukohdeValintakoeData data =
                                            new HakukohdeValintakoeData();
                                        data.setHakuOid(c.getHakuOid());
                                        data.setHakukohdeOid(h.getHakukohdeOid());
                                        data.setOsallistuminenTulos(koe.getOsallistuminenTulos());
                                        data.setValinnanVaiheJarjestysNro(
                                            v.getValinnanVaiheJarjestysluku());
                                        data.setValinnanVaiheOid(v.getValinnanvaiheOid());
                                        data.setValintakoeOid(koe.getValintakoeOid());
                                        data.setValintakoeTunniste(koe.getValintakoeTunniste());
                                        data.setNimi(koe.getNimi());
                                        data.setLahetetaankoKoekutsut(
                                            koe.isLahetetaankoKoekutsut());
                                        data.setAktiivinen(koe.isAktiivinen());
                                        data.setKutsunKohde(koe.getKutsunKohde());
                                        data.setKutsunKohdeAvain(koe.getKutsunKohdeAvain());
                                        olemassaOlevat.add(data);
                                      }
                                    })));
  }

  private boolean invalidEdellinenValinnanVaihe(
      String uuid,
      ValintaperusteetDTO vp,
      ValintaperusteetValinnanVaiheDTO vaihe,
      Valinnanvaihe edellinenVaihe) {
    if (edellinenVaihe == null && vaihe.getValinnanVaiheJarjestysluku() > 0) {
      if (!valintakoeOsallistuminenDAO.onkoEdeltavaValinnanvaiheOlemassa(
          vp.getHakuOid(), vp.getHakukohdeOid(), vaihe.getValinnanVaiheJarjestysluku())) {
        LOG.warn(
            String.format(
                "(Uuid=%s) Valinnanvaiheen %s järjestysnumero on suurempi kuin 0, mutta edellistä valinnanvaihetta ei löytynyt",
                uuid, vaihe.getValinnanVaiheOid()));
        return true;
      }
    }
    return false;
  }

  private HakukohdeValintakoeData getHakukohdeValintakoeData(
      HakemusDTO hakemus,
      String uuid,
      ValintaperusteetDTO vp,
      ValintaperusteetValinnanVaiheDTO vaihe,
      ValintakoeDTO koe,
      String tunniste) {
    HakukohdeValintakoeData data = new HakukohdeValintakoeData();
    handleKutsunKohde(hakemus, uuid, vp, vaihe, koe, data);
    data.setHakuOid(vp.getHakuOid());
    data.setLaskettavaHakukohdeOid(vp.getHakukohdeOid());
    data.setLaskettavaValinnanVaiheJarjestysNro(vaihe.getValinnanVaiheJarjestysluku());
    data.setValintakoeOid(koe.getOid());
    data.setValintakoeTunniste(tunniste);
    data.setNimi(koe.getNimi());
    data.setLahetetaankoKoekutsut(koe.getLahetetaankoKoekutsut());
    data.setAktiivinen(koe.getAktiivinen());
    data.setKutsunKohde(koe.getKutsunKohde());
    data.setKutsunKohdeAvain(koe.getKutsunKohdeAvain());
    return data;
  }

  private void handleKutsunKohde(
      HakemusDTO hakemus,
      String uuid,
      ValintaperusteetDTO vp,
      ValintaperusteetValinnanVaiheDTO vaihe,
      ValintakoeDTO koe,
      HakukohdeValintakoeData data) {
    if (koe.getKutsunKohde().equals(Koekutsu.HAKIJAN_VALINTA)) {
      data.setValinnanVaiheOid(VALINNANVAIHE_HAKIJAN_VALINTA);
      data.setValinnanVaiheJarjestysNro(100);
      final Optional<AvainArvoDTO> avainArvo =
          hakemus.getAvaimet().stream()
              .filter(avainArvoDTO -> avainArvoDTO.getAvain().equals(koe.getKutsunKohdeAvain()))
              .findFirst();
      if (avainArvo.isPresent()) {
        data.setHakukohdeOid(avainArvo.get().getArvo());
      } else {
        LOG.error(
            "(Uuid={}) Hakemukselta {} puuttuu kutsun kohde {}",
            uuid,
            hakemus.getHakemusoid(),
            koe.getKutsunKohdeAvain());
        throw new RuntimeException("Hakemukselta ei löytynyt kutsun kohdetta!");
      }
    } else {
      data.setHakukohdeOid(vp.getHakukohdeOid());
      data.setValinnanVaiheJarjestysNro(vaihe.getValinnanVaiheJarjestysluku());
      data.setValinnanVaiheOid(vaihe.getValinnanVaiheOid());
    }
  }

  private Valinnanvaihe getViimeisinValinnanvaihe(
      ValintaperusteetDTO vp,
      ValintaperusteetValinnanVaiheDTO vaihe,
      Valinnanvaihe edellinenVaihe) {
    // Haetaan viimeisin varsinainen valinnan vaihe, jos sellainen on olemassa (tämä saattaa olla
    // sama kuin edeltävä vaihe)
    Valinnanvaihe viimeisinValinnanVaihe = null;
    if (vaihe.getValinnanVaiheJarjestysluku() > 0) {
      if (edellinenVaihe != null
          && edellinenVaihe.getJarjestysnumero() == vaihe.getValinnanVaiheJarjestysluku() - 1) {
        viimeisinValinnanVaihe = edellinenVaihe;
      } else {
        viimeisinValinnanVaihe =
            valinnanvaiheDAO.haeViimeisinValinnanvaihe(
                vp.getHakuOid(), vp.getHakukohdeOid(), vaihe.getValinnanVaiheJarjestysluku());
      }
    }
    return viimeisinValinnanVaihe;
  }

  private OsallistuminenTulos getOsallistuminenTulos(
      HakemusDTO hakemus,
      ValintaperusteetDTO vp,
      Map<String, String> hakukohteenValintaperusteet,
      ValintakoeDTO koe,
      Valinnanvaihe viimeisinValinnanVaihe,
      boolean korkeakouluhaku,
      ValintakoeOsallistuminen vanhatOsallistumiset) {
    OsallistuminenTulos osallistuminen = new OsallistuminenTulos();
    Hakemus hak = hakemusConverter.convert(hakemus);
    Funktiokutsu fuk = modelMapper.map(koe.getFunktiokutsu(), Funktiokutsu.class);
    if (viimeisinValinnanVaihe != null) {
      TilaJaSelite tilaJaSelite =
          edellinenValinnanvaiheKasittelija.tilaEdellisenValinnanvaiheenMukaan(
              hakemus.getHakemusoid(),
              new Hyvaksyttavissatila(),
              viimeisinValinnanVaihe,
              Optional.of(vp),
              Optional.ofNullable(vanhatOsallistumiset));
      if (tilaJaSelite.getTila().equals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA)) {
        if (koe.getKutsunKohde().equals(Koekutsu.HAKIJAN_VALINTA)) {
          osallistuminen = createOsallistuminen(tilaJaSelite, Osallistuminen.OSALLISTUU);
        } else {
          osallistuminen =
              getOsallistuminenTulos(
                  new Hakukohde(vp.getHakukohdeOid(), hakukohteenValintaperusteet, korkeakouluhaku),
                  hak,
                  fuk);
        }
      } else {
        osallistuminen = createOsallistuminen(tilaJaSelite, Osallistuminen.EI_OSALLISTU);
      }
    } else {
      if (koe.getKutsunKohde().equals(Koekutsu.HAKIJAN_VALINTA)) {
        osallistuminen.setOsallistuminen(Osallistuminen.OSALLISTUU);
      } else {
        osallistuminen =
            getOsallistuminenTulos(
                new Hakukohde(vp.getHakukohdeOid(), hakukohteenValintaperusteet, korkeakouluhaku),
                hak,
                fuk);
      }
    }
    return osallistuminen;
  }

  private void poistaVanhatOsallistumiset(
      final HakemusDTO hakemus, final List<ValintaperusteetDTO> valintaperusteet) {
    final String ensimmainenHakukohde = valintaperusteet.get(0).getHakukohdeOid();
    final Stream<ValintakoeDTO> laskettavat =
        valintaperusteet.stream()
            .flatMap(vp -> vp.getValinnanVaihe().getValintakoe().stream())
            .filter(koe -> !koe.getKutsunKohde().equals(Koekutsu.HAKIJAN_VALINTA));
    final boolean onMuitaValintakokeitaKuinHakijanValitsemia = laskettavat.count() > 0;
    if (!onMuitaValintakokeitaKuinHakijanValitsemia) {
      final Optional<ValintakoeOsallistuminen> kaikkiOsallistumisetOpt =
          Optional.ofNullable(
              valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(
                  hakemus.getHakuoid(), hakemus.getHakemusoid()));
      if (kaikkiOsallistumisetOpt.isPresent()) {
        final ValintakoeOsallistuminen kaikkiOsallistumiset = kaikkiOsallistumisetOpt.get();
        final List<ValintakoeValinnanvaihe> valinnanvaiheet =
            kaikkiOsallistumiset.getHakutoiveet().stream()
                .filter(h -> h.getHakukohdeOid().equals(ensimmainenHakukohde))
                .flatMap(hakutoive -> hakutoive.getValintakoeValinnanvaiheet().stream())
                .toList();
        valinnanvaiheet.forEach(
            vv -> {
              final List<Valintakoe> saastettavat =
                  vv.getValintakokeet().stream()
                      .filter(
                          valintakoe ->
                              valintakoe.getKutsunKohde().equals(Koekutsu.HAKIJAN_VALINTA))
                      .collect(Collectors.toList());
              vv.setValintakokeet(saastettavat);
            });
        if (valinnanvaiheet.size() > 0) {
          final boolean eiValintakokeita =
              valinnanvaiheet.stream().flatMap(vv -> vv.getValintakokeet().stream()).count() == 0;
          if (eiValintakokeita) {
            final List<Hakutoive> saastettavat =
                kaikkiOsallistumiset.getHakutoiveet().stream()
                    .filter(hakutoive -> !hakutoive.getHakukohdeOid().equals(ensimmainenHakukohde))
                    .toList();
            kaikkiOsallistumiset.setHakutoiveet(new HashSet<>(saastettavat));
          } else {
            kaikkiOsallistumiset
                .getHakutoiveet()
                .forEach(
                    hakutoive -> {
                      if (hakutoive.getHakukohdeOid().equals(ensimmainenHakukohde)) {
                        hakutoive.setValintakoeValinnanvaiheet(new HashSet<>(valinnanvaiheet));
                      }
                    });
          }
          LOG.debug(
              String.format(
                  "poistaVanhatOsallistumiset: hakemukselle %s tallennetaan osallistumiset %s (%d hakutoivetta)",
                  hakemus.getHakemusoid(),
                  ToStringBuilder.reflectionToString(kaikkiOsallistumiset),
                  kaikkiOsallistumiset.getHakutoiveet().size()));
          saveValintakoeOsallistuminen(kaikkiOsallistumiset);
        }
      }
    }
  }

  private void saveValintakoeOsallistuminen(ValintakoeOsallistuminen v) {
    valintakoeOsallistuminenDAO.createOrUpdate(v);
  }

  private OsallistuminenTulos getOsallistuminenTulos(
      Hakukohde hakukohde, Hakemus convert, Funktiokutsu fuk) {
    return valintakoeosallistumislaskin.laskeOsallistuminenYhdelleHakukohteelle(
        hakukohde, convert, fuk);
  }

  private OsallistuminenTulos createOsallistuminen(
      TilaJaSelite tilaJaSelite, Osallistuminen osallistuu) {
    OsallistuminenTulos osallistuminen = new OsallistuminenTulos();
    osallistuminen.setKuvaus(tilaJaSelite.getSelite());
    osallistuminen.setTekninenKuvaus(tilaJaSelite.getTekninenSelite());
    osallistuminen.setLaskentaTila(tilaJaSelite.getTila().toString());
    osallistuminen.setOsallistuminen(osallistuu);
    return osallistuminen;
  }

  private Map<String, String> muodostaHakukohteenValintaperusteetMap(
      List<HakukohteenValintaperusteDTO> hakukohteenValintaperuste) {
    Map<String, String> map = new HashMap<>();
    for (HakukohteenValintaperusteDTO vp : hakukohteenValintaperuste) {
      map.put(vp.getTunniste(), vp.getArvo());
    }
    return map;
  }

  protected void asetaOsallistumisetKokeisiin(
      List<HakukohdeValintakoeData> kokeet, final Map<String, HakukohdeDTO> hakukohteetByOid) {
    // Käydään hakijan hakutoiveet läpi prioriteetin mukaan ja asetetaan
    // kullekin hakukohteelle
    // valintakoekohtainen osallistumistieto
    Collections.sort(
        kokeet,
        (o1, o2) ->
            hakukohteetByOid
                    .getOrDefault(
                        o1.getHakukohdeOid(),
                        new HakukohdeDTO() {
                          {
                            setPrioriteetti(Integer.MAX_VALUE);
                          }
                        })
                    .getPrioriteetti()
                - hakukohteetByOid
                    .getOrDefault(
                        o2.getHakukohdeOid(),
                        new HakukohdeDTO() {
                          {
                            setPrioriteetti(Integer.MAX_VALUE);
                          }
                        })
                    .getPrioriteetti());

    // Jos hakija osallistuu korkeamman prioriteetin hakuktoiveen
    // valintakokeeseen, hakija ei osallistu
    // pienemmällä prioriteetilla oleviin samoihin valintakokeisiin.
    boolean osallistuminenLoydetty = false;
    final List<HakukohdeValintakoeData> lasketutKokeet =
        kokeet.stream()
            .filter(koe -> !koe.getKutsunKohde().equals(Koekutsu.HAKIJAN_VALINTA))
            .toList();
    for (HakukohdeValintakoeData d : lasketutKokeet) {
      if (!osallistuminenLoydetty
          && Osallistuminen.OSALLISTUU.equals(d.getOsallistuminenTulos().getOsallistuminen())) {
        osallistuminenLoydetty = true;
        continue;
      }
      if (!d.getOsallistuminenTulos().getOsallistuminen().equals(Osallistuminen.VIRHE)) {
        d.getOsallistuminenTulos().setOsallistuminen(Osallistuminen.EI_OSALLISTU);
      }
    }
  }

  protected ValintakoeOsallistuminen luoValintakoeOsallistuminen(
      HakukohdeValintakoeData data,
      HakemusDTO hakemus,
      Map<String, HakukohdeDTO> hakutoiveetByOid) {
    ValintakoeOsallistuminen osallistuminen =
        valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(
            data.getHakuOid(), hakemus.getHakemusoid());
    if (osallistuminen == null) {
      osallistuminen = new ValintakoeOsallistuminen();
    }
    for (Hakutoive h : osallistuminen.getHakutoiveet()) {
      String oid = h.getHakukohdeOid();
      if (!hakutoiveetByOid.containsKey(oid) || oid.equals(data.getLaskettavaHakukohdeOid())) {
        h.setValintakoeValinnanvaiheet(getHakijanValintaVaiheet(h));
      }
    }
    osallistuminen.setHakutoiveet(
        osallistuminen.getHakutoiveet().stream()
            .filter(h -> !h.getValintakoeValinnanvaiheet().isEmpty())
            .collect(Collectors.toSet()));
    osallistuminen.setHakuOid(data.getHakuOid());
    osallistuminen.setHakemusOid(hakemus.getHakemusoid());
    osallistuminen.setHakijaOid(hakemus.getHakijaOid());
    return osallistuminen;
  }

  private Set<ValintakoeValinnanvaihe> getHakijanValintaVaiheet(Hakutoive t) {
    return t.getValintakoeValinnanvaiheet().stream()
        .filter(v -> v.getValinnanvaiheOid().equals(VALINNANVAIHE_HAKIJAN_VALINTA))
        .collect(Collectors.toSet());
  }

  protected void haeTaiLuoHakutoive(
      ValintakoeOsallistuminen osallistuminen, HakukohdeValintakoeData data) {
    Hakutoive toive = null;
    for (Hakutoive t : osallistuminen.getHakutoiveet()) {
      if (data.getHakukohdeOid().equals(t.getHakukohdeOid())) {
        toive = t;
        break;
      }
    }
    if (toive == null) {
      toive = new Hakutoive();
      osallistuminen.getHakutoiveet().add(toive);
    }
    toive.setHakukohdeOid(data.getHakukohdeOid());
    haeTaiLuoValinnanVaihe(toive, data);
  }

  protected void haeTaiLuoValinnanVaihe(Hakutoive hakutoive, HakukohdeValintakoeData data) {
    ValintakoeValinnanvaihe vaihe = null;
    for (ValintakoeValinnanvaihe v : hakutoive.getValintakoeValinnanvaiheet()) {
      if (data.getValinnanVaiheOid().equals(v.getValinnanvaiheOid())) {
        vaihe = v;
        break;
      }
    }
    if (vaihe == null) {
      vaihe = new ValintakoeValinnanvaihe();
      hakutoive.getValintakoeValinnanvaiheet().add(vaihe);
    }
    vaihe.setValinnanvaiheOid(data.getValinnanVaiheOid());
    vaihe.setValinnanVaiheJarjestysluku(data.getValinnanVaiheJarjestysNro());
    haeTaiLuoValintakoe(vaihe, data);
  }

  protected void haeTaiLuoValintakoe(
      ValintakoeValinnanvaihe valinnanVaihe, HakukohdeValintakoeData data) {
    Valintakoe koe = null;
    for (Valintakoe k : valinnanVaihe.getValintakokeet()) {
      if (data.getValintakoeTunniste().equals(k.getValintakoeTunniste())) {
        koe = k;
        break;
      }
    }
    if (koe == null) {
      koe = new Valintakoe();
      valinnanVaihe.getValintakokeet().add(koe);
    }
    koe.setOsallistuminenTulos(data.getOsallistuminenTulos());
    koe.setValintakoeOid(data.getValintakoeOid());
    koe.setNimi(data.getNimi());
    koe.setValintakoeTunniste(data.getValintakoeTunniste());
    koe.setLahetetaankoKoekutsut(data.isLahetetaankoKoekutsut());
    koe.setAktiivinen(data.isAktiivinen());
    koe.setKutsunKohde(data.getKutsunKohde());
    koe.setKutsunKohdeAvain(data.getKutsunKohdeAvain());
  }

  protected Map<String, HakukohdeDTO> luoHakutoiveMap(List<HakukohdeDTO> hakutoiveet) {
    Map<String, HakukohdeDTO> toiveetMap = new HashMap<String, HakukohdeDTO>();
    for (HakukohdeDTO hk : hakutoiveet) {
      toiveetMap.put(hk.getOid(), hk);
    }
    return toiveetMap;
  }
}
