package fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl;

import fi.vm.sade.kaava.Laskentadomainkonvertteri;
import fi.vm.sade.service.valintaperusteet.dto.HakukohteenValintaperusteDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetHakijaryhmaDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetJarjestyskriteeriDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetValinnanVaiheDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintatapajonoJarjestyskriteereillaDTO;
import fi.vm.sade.service.valintaperusteet.dto.model.Funktiotyyppi;
import fi.vm.sade.service.valintaperusteet.dto.model.ValinnanVaiheTyyppi;
import fi.vm.sade.service.valintaperusteet.laskenta.Lukuarvofunktio;
import fi.vm.sade.service.valintaperusteet.laskenta.Totuusarvofunktio;
import fi.vm.sade.service.valintaperusteet.laskenta.api.FunktioTulos;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakemus;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakukohde;
import fi.vm.sade.service.valintaperusteet.laskenta.api.SyotettyArvo;
import fi.vm.sade.service.valintaperusteet.model.Funktiokutsu;
import fi.vm.sade.service.valintaperusteet.service.validointi.virhe.LaskentakaavaEiOleValidiException;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakukohdeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.Tasasijasaanto;
import fi.vm.sade.valintalaskenta.domain.valinta.*;
import fi.vm.sade.valintalaskenta.laskenta.dao.HakijaryhmaDAO;
import fi.vm.sade.valintalaskenta.laskenta.dao.JarjestyskriteerihistoriaDAO;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValinnanvaiheDAO;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValintakoeOsallistuminenDAO;
import fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion.HakemusDTOToHakemusConverter;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.HakemuslaskinService;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.ValintalaskentaSuorittajaService;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import java.util.*;
import java.util.stream.Collectors;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class ValintalaskentaSuorittajaServiceImpl implements ValintalaskentaSuorittajaService {
  private static final Logger LOG =
      LoggerFactory.getLogger(ValintalaskentaSuorittajaServiceImpl.class);

  private final HakemusDTOToHakemusConverter hakemusConverter;
  private final ValinnanvaiheDAO valinnanvaiheDAO;
  private final HakijaryhmaDAO hakijaryhmaDAO;
  private final JarjestyskriteerihistoriaDAO jarjestyskriteerihistoriaDAO;
  private final HakemuslaskinService hakemuslaskinService;
  private final ValintakoeOsallistuminenDAO valintakoeOsallistuminenDAO;
  private final ValintalaskentaModelMapper modelMapper;
  private final EdellinenValinnanvaiheKasittelija edellinenValinnanvaiheKasittelija;

  @Autowired
  public ValintalaskentaSuorittajaServiceImpl(
      HakemusDTOToHakemusConverter hakemusConverter,
      ValinnanvaiheDAO valinnanvaiheDAO,
      HakijaryhmaDAO hakijaryhmaDAO,
      JarjestyskriteerihistoriaDAO jarjestyskriteerihistoriaDAO,
      HakemuslaskinService hakemuslaskinService,
      ValintakoeOsallistuminenDAO valintakoeOsallistuminenDAO,
      ValintalaskentaModelMapper modelMapper,
      EdellinenValinnanvaiheKasittelija edellinenValinnanvaiheKasittelija) {
    this.hakemusConverter = hakemusConverter;
    this.valinnanvaiheDAO = valinnanvaiheDAO;
    this.hakijaryhmaDAO = hakijaryhmaDAO;
    this.jarjestyskriteerihistoriaDAO = jarjestyskriteerihistoriaDAO;
    this.hakemuslaskinService = hakemuslaskinService;
    this.valintakoeOsallistuminenDAO = valintakoeOsallistuminenDAO;
    this.modelMapper = modelMapper;
    this.edellinenValinnanvaiheKasittelija = edellinenValinnanvaiheKasittelija;
  }

  @Override
  public void suoritaLaskenta(
      List<HakemusDTO> kaikkiHakemukset,
      List<ValintaperusteetDTO> valintaperusteet,
      List<ValintaperusteetHakijaryhmaDTO> hakijaryhmat,
      String hakukohdeOid,
      String uuid,
      boolean korkeakouluhaku) {
    Map<String, Hakemukset> hakemuksetHakukohteittain =
        jarjestaHakemuksetHakukohteittain(kaikkiHakemukset);
    jarjestaValinnanVaiheenJarjestysluvunMukaan(valintaperusteet);

    for (ValintaperusteetDTO vp : valintaperusteet) {
      if (!hakemuksetHakukohteittain.containsKey(hakukohdeOid)) {
        LOG.error(
            "(Uuid={}) Hakukohteelle {} ei ole yhtään hakemusta. Hypätään yli.",
            uuid,
            hakukohdeOid);
        continue;
      }
      List<HakemusWrapper> hakemukset = hakemuksetHakukohteittain.get(hakukohdeOid).getHakemukset();
      List<Hakemus> laskentahakemukset =
          hakemuksetHakukohteittain.get(hakukohdeOid).getLaskentahakemukset();
      if (emptyHakemuksetOrValinnanVaiheTyyppiValintakoe(vp, hakemukset)) {
        continue;
      }
      Map<String, String> hakukohteenValintaperusteet =
          muodostaHakukohteenValintaperusteetMap(vp.getHakukohteenValintaperuste());
      ValintaperusteetValinnanVaiheDTO vaihe = vp.getValinnanVaihe();
      final String valinnanvaiheOid = vaihe.getValinnanVaiheOid();
      final int jarjestysnumero = vaihe.getValinnanVaiheJarjestysluku();
      final String hakuOid = vp.getHakuOid();

      LOG.info(
          "(Uuid={}) Haku {}, hakukohde {}, valinnanvaihe {} - jarjestysnumero {}",
          uuid,
          hakuOid,
          hakukohdeOid,
          valinnanvaiheOid,
          jarjestysnumero);
      Valinnanvaihe edellinenVaihe =
          valinnanvaiheDAO.haeEdeltavaValinnanvaihe(hakuOid, hakukohdeOid, jarjestysnumero);
      if (invalidEdellinenVaihe(hakukohdeOid, uuid, jarjestysnumero, hakuOid, edellinenVaihe)) {
        continue;
      }
      final Valinnanvaihe viimeisinVaihe =
          getViimeisinValinnanVaihe(hakukohdeOid, jarjestysnumero, hakuOid, edellinenVaihe);
      LOG.info(
          "(Uuid={}) Viimeisin valinnanvaihe: id {}",
          uuid,
          viimeisinVaihe != null ? viimeisinVaihe.getId() : "null");

      Valinnanvaihe valinnanvaihe =
          haeTaiLuoValinnanvaihe(valinnanvaiheOid, hakuOid, hakukohdeOid, jarjestysnumero, vaihe);
      valinnanvaihe.setHakukohdeOid(hakukohdeOid);
      valinnanvaihe.setHakuOid(hakuOid);
      valinnanvaihe.setJarjestysnumero(jarjestysnumero);
      valinnanvaihe.setValinnanVaiheOid(valinnanvaiheOid);
      valinnanvaihe.setTarjoajaOid(vp.getTarjoajaOid());
      valinnanvaihe.setNimi(vp.getValinnanVaihe().getNimi());
      LOG.info("(Uuid={}) Hae tai luo valinnanvaihe: valinnanvaiheOid {}", uuid, valinnanvaiheOid);

      heitaPoikkeusJosValintatapajonoJaaIlmanTuloksia(uuid, vaihe, valinnanvaihe);

      boolean edellinenValinnanvaiheOnOlemassa =
          valintakoeOsallistuminenDAO.onkoEdeltavaValinnanvaiheOlemassa(
              hakuOid, hakukohdeOid, jarjestysnumero);
      LOG.info(
          "(Uuid={}) Edellinen valinnanvaihe olemassa: {}", uuid, edellinenValinnanvaiheOnOlemassa);

      searchForPassives("PRE", valinnanvaihe, hakemukset);

      LOG.info("(Uuid={}) Laske valintatapajonot", uuid);
      laskeValintatapajonot(
          hakukohdeOid,
          uuid,
          hakemukset,
          laskentahakemukset,
          hakukohteenValintaperusteet,
          vaihe,
          jarjestysnumero,
          edellinenVaihe,
          viimeisinVaihe,
          valinnanvaihe,
          edellinenValinnanvaiheOnOlemassa,
          korkeakouluhaku);

      searchForPassives("POST ", valinnanvaihe, hakemukset);
      LOG.info("(Uuid={}) Tallenna valinnanvaihe: valinnanvaiheOid {}", uuid, valinnanvaiheOid);
      valinnanvaiheDAO.saveOrUpdate(valinnanvaihe);
    }

    poistaHaamuryhmat(hakijaryhmat, valintaperusteet.get(0).getHakukohdeOid(), uuid);
    LOG.info(
        "(Uuid={}) Hakijaryhmien määrä {} hakukohteessa {}",
        uuid,
        hakijaryhmat.size(),
        hakukohdeOid);
    laskeHakijaryhmat(
        valintaperusteet,
        hakijaryhmat,
        hakukohdeOid,
        uuid,
        hakemuksetHakukohteittain,
        korkeakouluhaku);
  }

  private void heitaPoikkeusJosValintatapajonoJaaIlmanTuloksia(
      String uuid,
      ValintaperusteetValinnanVaiheDTO valinnanvaiheValintaperusteissa,
      Valinnanvaihe valinnanvaiheValintalaskennassa) {
    valinnanvaiheValintaperusteissa.getValintatapajono().stream()
        .filter(
            valintatapajonoValintaperusteissa ->
                valintatapajonoValintaperusteissa.getEiLasketaPaivamaaranJalkeen() != null
                    && beforeDate(
                        valintatapajonoValintaperusteissa.getEiLasketaPaivamaaranJalkeen(),
                        new Date())
                    && valintalaskennastaEiLoydyValintaperusteissaOlevaaJonoa(
                        valintatapajonoValintaperusteissa, valinnanvaiheValintalaskennassa))
        .findAny()
        .ifPresent(
            valintatapajonoValintaperusteissa -> {
              final String message =
                  String.format(
                      "(Uuid=%s) Valintaperusteissa olevan valinnanvaiheen %s valintatapajono %s uhkaa jäädä kokonaan ilman tuloksia, koska jonoa ei ole laskettu aiemmin eikä sitä saa laskea päivämäärän %s jälkeen",
                      uuid,
                      valinnanvaiheValintaperusteissa.getValinnanVaiheOid(),
                      valintatapajonoValintaperusteissa.getOid(),
                      valintatapajonoValintaperusteissa.getEiLasketaPaivamaaranJalkeen());
              LOG.error(message);
              throw new RuntimeException(message);
            });
  }

  private boolean valintalaskennastaEiLoydyValintaperusteissaOlevaaJonoa(
      ValintatapajonoJarjestyskriteereillaDTO valintatapajonoValintaperusteissa,
      Valinnanvaihe valinnanvaiheValintalaskennassa) {
    return valinnanvaiheValintalaskennassa.getValintatapajonot().stream()
        .noneMatch(
            valintatapajonoValintalaskennassa ->
                valintatapajonoValintalaskennassa
                    .getValintatapajonoOid()
                    .equals(valintatapajonoValintaperusteissa.getOid()));
  }

  private Date removeTime(Date date) {
    Calendar removed = Calendar.getInstance();
    removed.setTime(date);
    removed.set(Calendar.HOUR_OF_DAY, 0);
    removed.set(Calendar.MINUTE, 0);
    removed.set(Calendar.SECOND, 0);
    removed.set(Calendar.MILLISECOND, 0);
    return removed.getTime();
  }

  private boolean beforeDate(Date a, Date b) {
    return removeTime(a).before(removeTime(b));
  }

  private void searchForPassives(
      String etuliite, Valinnanvaihe uusi, List<HakemusWrapper> hakemukset) {
    List<String> validHakemusOids =
        hakemukset.stream()
            .map(h -> h.getHakemusDTO().getHakemusoid())
            .collect(Collectors.toList());
    // Passivoituja ovat ehkä jonosijat, joiden hakemusoideja ei löydy tuoreista (tätä laskentaa
    // varten haetuista) hakemuksista
    List<String> passiveHakemusOids = new ArrayList<>();
    List<String> goodHakemusOids = new ArrayList<>();
    for (Valintatapajono jono : uusi.getValintatapajonot()) {
      for (Jonosija j : jono.getJonosijat()) {
        if (!validHakemusOids.contains(j.getHakemusOid())) {
          passiveHakemusOids.add(j.getHakemusOid());
        } else {
          goodHakemusOids.add(j.getHakemusOid());
        }
      }
    }
    if (!passiveHakemusOids.isEmpty()) {
      LOG.warn("Löytyi passiivisia hakemuksia!");
      uusi.getValintatapajonot()
          .forEach(
              j ->
                  j.getJonosijat()
                      .forEach(
                          js ->
                              LOG.warn(
                                  etuliite + " HAKEMUS {} JONOSSA {}",
                                  js.getHakemusOid(),
                                  j.getValintatapajonoOid())));
      LOG.warn(
          etuliite + " Valid hakemusOids (active, incomplete hakemukses): " + validHakemusOids);
      LOG.error(
          etuliite
              + " Passivoidut jonosijas, hakemusOid not in valids: "
              + passiveHakemusOids
              + ", good hakemusOids: "
              + goodHakemusOids);
    }
    LOG.info(
        etuliite
            + " Hakukohde {}, hakemuksia {} kpl : Tutkittiin, löytyykö laskennan tuloksista hakemusoideja joita ei ole tämän laskentakierroksen hakemusten joukossa. Passivoituja: {}, hyviä: {}",
        uusi.getHakukohdeOid(),
        hakemukset.size(),
        passiveHakemusOids.size(),
        goodHakemusOids.size());
  }

  private boolean emptyHakemuksetOrValinnanVaiheTyyppiValintakoe(
      ValintaperusteetDTO vp, List<HakemusWrapper> hakemukset) {
    return hakemukset == null
        || hakemukset.isEmpty()
        || (vp.getValinnanVaihe().getValinnanVaiheTyyppi().equals(ValinnanVaiheTyyppi.VALINTAKOE));
  }

  private boolean invalidEdellinenVaihe(
      String hakukohdeOid,
      String uuid,
      int jarjestysnumero,
      String hakuOid,
      Valinnanvaihe edellinenVaihe) {
    if (edellinenVaihe == null && jarjestysnumero > 0) {
      if (!valintakoeOsallistuminenDAO.onkoEdeltavaValinnanvaiheOlemassa(
          hakuOid, hakukohdeOid, jarjestysnumero)) {
        LOG.warn(
            "(Uuid={}) Valinnanvaiheen järjestysnumero on suurempi kuin 0, mutta edellistä valinnanvaihetta ei löytynyt",
            uuid);
        return true;
      }
    }
    return false;
  }

  private Valinnanvaihe getViimeisinValinnanVaihe(
      String hakukohdeOid, int jarjestysnumero, String hakuOid, Valinnanvaihe edellinenVaihe) {
    final Valinnanvaihe viimeisinVaihe;
    if (jarjestysnumero > 0) {
      if (edellinenVaihe != null && edellinenVaihe.getJarjestysnumero() == jarjestysnumero - 1) {
        viimeisinVaihe = edellinenVaihe;
      } else {
        viimeisinVaihe =
            valinnanvaiheDAO.haeViimeisinValinnanvaihe(hakuOid, hakukohdeOid, jarjestysnumero);
      }
    } else {
      viimeisinVaihe = null;
    }
    return viimeisinVaihe;
  }

  private void laskeHakijaryhmat(
      List<ValintaperusteetDTO> valintaperusteet,
      List<ValintaperusteetHakijaryhmaDTO> hakijaryhmat,
      String hakukohdeOid,
      String uuid,
      Map<String, Hakemukset> hakemuksetHakukohteittain,
      boolean korkeakouluhaku) {
    if (!hakijaryhmat.isEmpty()) {
      List<Hakijaryhma> vanhatHakijaryhmat = hakijaryhmaDAO.haeHakijaryhmat(hakukohdeOid);
      hakijaryhmat.parallelStream()
          .forEach(
              h -> {
                if (!hakemuksetHakukohteittain.containsKey(hakukohdeOid)) {
                  LOG.info(
                      "(Uuid={}) Hakukohteelle {} ei ole yhtään hakemusta. Hypätään yli.",
                      uuid,
                      hakukohdeOid);
                  return;
                }

                List<HakemusWrapper> hakemukset =
                    hakemuksetHakukohteittain.get(hakukohdeOid).getHakemukset();
                List<Hakemus> laskentahakemukset =
                    hakemuksetHakukohteittain.get(hakukohdeOid).getLaskentahakemukset();
                if (hakemukset == null || hakemukset.isEmpty()) {
                  return;
                }

                Hakijaryhma hakijaryhma = haeTaiLuoHakijaryhma(h);

                Map<String, JonosijaJaSyotetytArvot> jonosijatHakemusOidinMukaan = new HashMap<>();
                Map<String, String> hakukohteenValintaperusteet =
                    muodostaHakukohteenValintaperusteetMap(
                        valintaperusteet.get(0).getHakukohteenValintaperuste());

                Funktiokutsu funktiokutsu =
                    modelMapper.map(h.getFunktiokutsu(), Funktiokutsu.class);
                Optional<Lukuarvofunktio> lukuarvofunktio = Optional.empty();
                Optional<Totuusarvofunktio> totuusarvofunktio = Optional.empty();
                try {
                  if (Funktiotyyppi.LUKUARVOFUNKTIO.equals(
                      funktiokutsu.getFunktionimi().getTyyppi())) {
                    lukuarvofunktio =
                        Optional.ofNullable(
                            Laskentadomainkonvertteri.muodostaLukuarvolasku(funktiokutsu));
                  } else {
                    totuusarvofunktio =
                        Optional.ofNullable(
                            Laskentadomainkonvertteri.muodostaTotuusarvolasku(funktiokutsu));
                  }
                } catch (LaskentakaavaEiOleValidiException e) {
                  LOG.error(
                      "(Uuid={}) Hakukohteen {} Hakijaryhmän {} funktiokutsu ei ole validi. Laskentaa ei voida suorittaa.",
                      uuid,
                      hakukohdeOid,
                      h.getOid());
                  return;
                }

                for (HakemusWrapper hw : hakemukset) {
                  LOG.debug("hakemus {}", new Object[] {hw.getHakemusDTO().getHakemusoid()});
                  if (lukuarvofunktio.isPresent()) {
                    hakemuslaskinService.suoritaHakijaryhmaLaskentaHakemukselle(
                        new Hakukohde(hakukohdeOid, hakukohteenValintaperusteet, korkeakouluhaku),
                        hw,
                        laskentahakemukset,
                        lukuarvofunktio.get(),
                        jonosijatHakemusOidinMukaan);
                  } else {
                    hakemuslaskinService.suoritaHakijaryhmaLaskentaHakemukselle(
                        new Hakukohde(hakukohdeOid, hakukohteenValintaperusteet, korkeakouluhaku),
                        hw,
                        laskentahakemukset,
                        totuusarvofunktio.get(),
                        jonosijatHakemusOidinMukaan);
                  }
                }

                for (JonosijaJaSyotetytArvot js : jonosijatHakemusOidinMukaan.values()) {
                  hakijaryhma.jonosija.add(createJonosija(js));
                }
                List<Hakijaryhma> vanhatSamallaOidilla =
                    vanhatHakijaryhmat.stream()
                        .filter(vh -> vh.hakijaryhmaOid.equals(hakijaryhma.hakijaryhmaOid))
                        .toList();
                LOG.info(
                    "(Uuid={}) Persistoidaan hakijaryhmä {} ja poistetaan sen aiemmat versiot ({} kpl).",
                    uuid,
                    hakijaryhma.hakijaryhmaOid,
                    vanhatSamallaOidilla.size());
                vanhatSamallaOidilla.forEach(hakijaryhmaDAO::poistaHakijaryhma);
                hakijaryhmaDAO.createWithoutAuditLogging(hakijaryhma);
              });
    }
  }

  private Jonosija createJonosija(JonosijaJaSyotetytArvot js) {
    Jonosija jonosija = js.getJonosija();
    for (SyotettyArvo a : js.getSyotetytArvot().values()) {
      fi.vm.sade.valintalaskenta.domain.valinta.SyotettyArvo syotettyArvo =
          new fi.vm.sade.valintalaskenta.domain.valinta.SyotettyArvo();
      syotettyArvo.setArvo(a.getArvo());
      syotettyArvo.setLaskennallinenArvo(a.getLaskennallinenArvo());
      syotettyArvo.setOsallistuminen(a.getOsallistuminen().name());
      syotettyArvo.setTunniste(a.getTunniste());
      syotettyArvo.setTyypinKoodiUri(a.getTyypinKoodiUri());
      syotettyArvo.setTilastoidaan(a.isTilastoidaan());
      jonosija.getSyotetytArvot().add(syotettyArvo);
    }
    for (FunktioTulos a : js.getFunktioTulokset().values()) {
      fi.vm.sade.valintalaskenta.domain.valinta.FunktioTulos funktioTulos =
          new fi.vm.sade.valintalaskenta.domain.valinta.FunktioTulos();
      funktioTulos.setArvo(a.getArvo());
      funktioTulos.setTunniste(a.getTunniste());
      funktioTulos.setNimiFi(a.getNimiFi());
      funktioTulos.setNimiSv(a.getNimiSv());
      funktioTulos.setNimiEn(a.getNimiEn());
      funktioTulos.setOmaopintopolku(a.isOmaopintopolku());
      jonosija.getFunktioTulokset().funktioTulokset.add(funktioTulos);
    }
    return jonosija;
  }

  private void laskeValintatapajonot(
      String hakukohdeOid,
      String uuid,
      List<HakemusWrapper> hakemukset,
      List<Hakemus> laskentahakemukset,
      Map<String, String> hakukohteenValintaperusteet,
      ValintaperusteetValinnanVaiheDTO vaihe,
      int jarjestysnumero,
      Valinnanvaihe edellinenVaihe,
      Valinnanvaihe viimeisinVaihe,
      Valinnanvaihe valinnanvaihe,
      boolean edellinenValinnanvaiheOnOlemassa,
      boolean korkeakouluhaku) {
    for (ValintatapajonoJarjestyskriteereillaDTO j : vaihe.getValintatapajono()) {
      if (j.getEiLasketaPaivamaaranJalkeen() != null
          && beforeDate(j.getEiLasketaPaivamaaranJalkeen(), new Date())) {
        LOG.info(
            "(Uuid {}) Hakukohteen {} ja valinnanvaiheen {} valintatapajonoa {} tuloksia ei lasketa uudestaan, koska jonoa ei saa laskea päivämäärän {} jälkeen",
            uuid,
            hakukohdeOid,
            vaihe.getValinnanVaiheOid(),
            j.getOid(),
            j.getEiLasketaPaivamaaranJalkeen());
        continue;
      }
      if (j.getKaytetaanValintalaskentaa() == null || j.getKaytetaanValintalaskentaa()) {
        Valintatapajono jono = createValintatapajono(j);
        Map<String, JonosijaJaSyotetytArvot> jonosijatHakemusOidinMukaan = new HashMap<>();
        for (ValintaperusteetJarjestyskriteeriDTO jk : j.getJarjestyskriteerit()) {
          Funktiokutsu funktiokutsu = modelMapper.map(jk.getFunktiokutsu(), Funktiokutsu.class);
          Optional<Lukuarvofunktio> lukuarvofunktio = Optional.empty();
          Optional<Totuusarvofunktio> totuusarvofunktio = Optional.empty();
          try {
            if (Funktiotyyppi.LUKUARVOFUNKTIO.equals(funktiokutsu.getFunktionimi().getTyyppi())) {
              lukuarvofunktio =
                  Optional.ofNullable(
                      Laskentadomainkonvertteri.muodostaLukuarvolasku(funktiokutsu));
            } else {
              totuusarvofunktio =
                  Optional.ofNullable(
                      Laskentadomainkonvertteri.muodostaTotuusarvolasku(funktiokutsu));
            }
          } catch (LaskentakaavaEiOleValidiException e) {
            LOG.error(
                "(Uuid={}) Hakukohteen {} Valintatapajonon {} prioriteetilla {} olevan järjestyskriteerin funktiokutsu ei ole validi. Laskentaa ei voida suorittaa.",
                uuid,
                hakukohdeOid,
                j.getOid(),
                jk.getPrioriteetti());
            continue;
          }

          int processed = 0;
          for (HakemusWrapper hw : hakemukset) {
            LOG.debug("hakemus {}", new Object[] {hw.getHakemusDTO().getHakemusoid()});

            if (lukuarvofunktio.isPresent()) {
              hakemuslaskinService.suoritaLaskentaHakemukselle(
                  new Hakukohde(hakukohdeOid, hakukohteenValintaperusteet, korkeakouluhaku),
                  hw,
                  laskentahakemukset,
                  lukuarvofunktio.get(),
                  jk.getPrioriteetti(),
                  viimeisinVaihe,
                  jonosijatHakemusOidinMukaan,
                  jk.getNimi(),
                  jarjestysnumero,
                  edellinenValinnanvaiheOnOlemassa);
            } else {
              hakemuslaskinService.suoritaLaskentaHakemukselle(
                  new Hakukohde(hakukohdeOid, hakukohteenValintaperusteet, korkeakouluhaku),
                  hw,
                  laskentahakemukset,
                  totuusarvofunktio.get(),
                  jk.getPrioriteetti(),
                  viimeisinVaihe,
                  jonosijatHakemusOidinMukaan,
                  jk.getNimi(),
                  jarjestysnumero,
                  edellinenValinnanvaiheOnOlemassa);
            }
            processed++;
            if (processed % 100 == 0 || processed == hakemukset.size()) {
              LOG.info(
                  "Processed {}/{} hakemukses for järjestyskriteeri prio {} in jono {} for hakukohde {}.",
                  processed,
                  hakemukset.size(),
                  jk.getPrioriteetti(),
                  j.getOid(),
                  hakukohdeOid);
            }
          }
        }

        jono.setJonosijat(
            jonosijatHakemusOidinMukaan.values().stream()
                .map(this::createJonosija)
                .collect(Collectors.toSet()));

        if (j.isPoistetaankoHylatyt()) {
          Set<Jonosija> filteroity =
              jono.getJonosijat().stream()
                  .filter(
                      sija -> {
                        boolean tila =
                            !sija.getJarjestyskriteeritulokset()
                                .jarjestyskriteeritulokset
                                .get(0)
                                .getTila()
                                .equals(JarjestyskriteerituloksenTila.HYLATTY);
                        TilaJaSelite tilaJaSelite =
                            edellinenValinnanvaiheKasittelija
                                .hakemusHyvaksyttavissaEdellisenValinnanvaiheenMukaan(
                                    sija.getHakemusOid(), edellinenVaihe);
                        return tila
                            || tilaJaSelite.getTila().equals(JarjestyskriteerituloksenTila.HYLATTY);
                      })
                  .collect(Collectors.toSet());
          jono.setJonosijat(filteroity);
        }
        // Tässä vois ehkä poistella vähän myös passivoitujen hakemuksien tuloksia?
        valinnanvaihe.getValintatapajonot().add(jono);
      }
    }
  }

  private Valintatapajono createValintatapajono(ValintatapajonoJarjestyskriteereillaDTO j) {
    Valintatapajono jono = new Valintatapajono();
    jono.setAloituspaikat(j.getAloituspaikat());
    jono.setEiVarasijatayttoa(j.getEiVarasijatayttoa());
    jono.setNimi(j.getNimi());
    jono.setPrioriteetti(j.getPrioriteetti());
    jono.setSiirretaanSijoitteluun(j.getSiirretaanSijoitteluun());
    jono.setKaikkiEhdonTayttavatHyvaksytaan(j.getKaikkiEhdonTayttavatHyvaksytaan());
    jono.setTasasijasaanto(Tasasijasaanto.valueOf(j.getTasasijasaanto()));
    jono.setValintatapajonoOid(j.getOid());
    jono.setValmisSijoiteltavaksi(j.getValmisSijoiteltavaksi());
    jono.setKaytetaanValintalaskentaa(j.getKaytetaanValintalaskentaa());
    return jono;
  }

  private void jarjestaValinnanVaiheenJarjestysluvunMukaan(
      List<ValintaperusteetDTO> valintaperusteet) {
    valintaperusteet.sort(
        Comparator.comparingInt(o -> o.getValinnanVaihe().getValinnanVaiheJarjestysluku()));
  }

  private Map<String, String> muodostaHakukohteenValintaperusteetMap(
      List<HakukohteenValintaperusteDTO> hakukohteenValintaperuste) {
    Map<String, String> map = new HashMap<>();
    for (HakukohteenValintaperusteDTO vp : hakukohteenValintaperuste) {
      map.put(vp.getTunniste(), vp.getArvo());
    }
    return map;
  }

  private Map<String, Hakemukset> jarjestaHakemuksetHakukohteittain(List<HakemusDTO> hakemukset) {
    Map<String, Hakemukset> hakukohdeHakemukset = new HashMap<>();
    for (HakemusDTO hakemus : hakemukset) {
      for (HakukohdeDTO hakukohde : hakemus.getHakukohteet()) {
        String hakukohdeOid = hakukohde.getOid();
        if (!hakukohdeHakemukset.containsKey(hakukohdeOid)) {
          hakukohdeHakemukset.put(hakukohdeOid, new Hakemukset());
        }
        HakemusWrapper h = new HakemusWrapper();
        h.setHakemusDTO(hakemus);
        h.setLaskentahakemus(hakemusConverter.convert(hakemus));

        for (HakukohdeDTO hakutoive : hakemus.getHakukohteet()) {
          if (hakukohdeOid.equals(hakutoive.getOid())) {
            h.setHakutoiveprioriteetti(hakutoive.getPrioriteetti());
            break;
          }
        }

        hakukohdeHakemukset.get(hakukohdeOid).getHakemukset().add(h);
        hakukohdeHakemukset.get(hakukohdeOid).getLaskentahakemukset().add(h.getLaskentahakemus());
      }
    }
    return hakukohdeHakemukset;
  }

  private Valinnanvaihe haeTaiLuoValinnanvaihe(
      String valinnanvaiheOid,
      String hakuOid,
      String hakukohdeOid,
      int jarjestysnumero,
      ValintaperusteetValinnanVaiheDTO valinnanvaiheValintaperusteissa) {
    Valinnanvaihe valinnanvaihe = valinnanvaiheDAO.haeValinnanvaihe(valinnanvaiheOid);

    // Tarkistetaan ettei jää haamuvaiheita OVT-7668
    List<Valinnanvaihe> vaiheet =
        valinnanvaiheDAO.haeValinnanvaiheetJarjestysnumerolla(
            hakuOid, hakukohdeOid, jarjestysnumero);
    for (Valinnanvaihe vaihe : vaiheet) {
      if (!vaihe.getValinnanVaiheOid().equals(valinnanvaiheOid)) {
        valinnanvaiheDAO.poistaValinnanvaihe(vaihe);
      }
    }

    if (valinnanvaihe != null) {
      poistaVanhatJonotJaHistoriat(valinnanvaihe, valinnanvaiheValintaperusteissa);
    } else {
      valinnanvaihe = new Valinnanvaihe();
    }
    return valinnanvaihe;
  }

  private void poistaVanhatJonotJaHistoriat(
      Valinnanvaihe valinnanvaihe,
      ValintaperusteetValinnanVaiheDTO valinnanvaiheValintaperusteissa) {
    List<Valintatapajono> saastettavat = new ArrayList<>();
    List<Valintatapajono> poistettavat = new ArrayList<>();
    for (Valintatapajono jono : valinnanvaihe.getValintatapajonot()) {
      jono.setLastModified(new Date());
      if (!jononTulostaEiSaaLaskeaUudestaan(jono, valinnanvaiheValintaperusteissa)
          && (jono.getKaytetaanValintalaskentaa() == null || jono.getKaytetaanValintalaskentaa())) {
        for (Jonosija jonosija : jono.getJonosijat()) {
          jonosija.setLastModified(new Date());
          for (Jarjestyskriteeritulos tulos :
              jonosija.getJarjestyskriteeritulokset().jarjestyskriteeritulokset) {
            jarjestyskriteerihistoriaDAO.createVersionWithUpdate(tulos.getHistoria());
          }
        }
        poistettavat.add(jono);
      } else {
        saastettavat.add(jono);
      }
    }

    valinnanvaihe.getValintatapajonot().clear();
    valinnanvaihe.getValintatapajonot().addAll(saastettavat);
    valinnanvaiheDAO.saveOrUpdate(valinnanvaihe);
    poistettavat.forEach(valinnanvaiheDAO::poistaJono);
  }

  private boolean jononTulostaEiSaaLaskeaUudestaan(
      Valintatapajono valintatapajonoValintalaskennassa,
      ValintaperusteetValinnanVaiheDTO valinnanvaiheValintaperusteissa) {
    return valinnanvaiheValintaperusteissa.getValintatapajono().stream()
        .anyMatch(
            valintatapajonoValintaperusteissa ->
                valintatapajonoValintaperusteissa
                        .getOid()
                        .equals(valintatapajonoValintalaskennassa.getValintatapajonoOid())
                    && valintatapajonoValintaperusteissa.getEiLasketaPaivamaaranJalkeen() != null
                    && beforeDate(
                        valintatapajonoValintaperusteissa.getEiLasketaPaivamaaranJalkeen(),
                        new Date()));
  }

  private void poistaHaamuryhmat(
      List<ValintaperusteetHakijaryhmaDTO> hakijaryhmat, String hakukohdeOid, String uuid) {
    List<String> oidit =
        hakijaryhmat.stream()
            .map(ValintaperusteetHakijaryhmaDTO::getOid)
            .collect(Collectors.toList());

    hakijaryhmaDAO.haeHakijaryhmat(hakukohdeOid).stream()
        .filter(h -> !oidit.contains(h.hakijaryhmaOid))
        .forEach(
            h -> {
              LOG.info(
                  "(Uuid={}) Poistetaan hakukohteelta {} hakijaryhmä {}, jota ei löydy enää valintaperusteista.",
                  uuid,
                  hakukohdeOid,
                  h.hakijaryhmaOid);
              hakijaryhmaDAO.poistaHakijaryhma(h);
            });
  }

  private Hakijaryhma haeTaiLuoHakijaryhma(ValintaperusteetHakijaryhmaDTO dto) {
    Hakijaryhma hakijaryhma = hakijaryhmaDAO.haeHakijaryhma(dto.getOid()).orElse(new Hakijaryhma());
    hakijaryhma.hakijaryhmaOid = dto.getOid();
    hakijaryhma.hakukohdeOid = dto.getHakukohdeOid();
    hakijaryhma.kaytaKaikki = dto.isKaytaKaikki();
    hakijaryhma.kaytetaanRyhmaanKuuluvia = dto.isKaytetaanRyhmaanKuuluvia();
    hakijaryhma.kiintio = dto.getKiintio();
    hakijaryhma.kuvaus = dto.getKuvaus();
    hakijaryhma.nimi = dto.getNimi();
    hakijaryhma.prioriteetti = dto.getPrioriteetti();
    hakijaryhma.tarkkaKiintio = dto.isTarkkaKiintio();
    hakijaryhma.valintatapajonoOid = dto.getValintatapajonoOid();
    if (dto.getHakijaryhmatyyppikoodi() != null) {
      hakijaryhma.hakijaryhmatyyppiKoodiuri = dto.getHakijaryhmatyyppikoodi().getUri();
    }
    poistaVanhatHistoriat(hakijaryhma);
    hakijaryhma.jonosija.clear();

    return hakijaryhma;
  }

  private void poistaVanhatHistoriat(Hakijaryhma hakijaryhma) {
    for (Jonosija jonosija : hakijaryhma.jonosija) {
      for (Jarjestyskriteeritulos tulos :
          jonosija.getJarjestyskriteeritulokset().jarjestyskriteeritulokset) {
        jarjestyskriteerihistoriaDAO.createVersionWithUpdate(tulos.getHistoria());
      }
    }
  }
}
