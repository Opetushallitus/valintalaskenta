package fi.vm.sade.valinta.kooste.external.resource.harkinnanvaraisuus.impl;

import fi.vm.sade.valinta.kooste.external.resource.harkinnanvaraisuus.HarkinnanvaraisuudenSyy;
import fi.vm.sade.valinta.kooste.external.resource.harkinnanvaraisuus.HarkinnanvaraisuusAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.harkinnanvaraisuus.dto.HakemuksenHarkinnanvaraisuus;
import fi.vm.sade.valinta.kooste.external.resource.harkinnanvaraisuus.dto.HakutoiveenHarkinnanvaraisuus;
import fi.vm.sade.valinta.kooste.external.resource.suoritusrekisteri.dto.Oppija;
import fi.vm.sade.valinta.kooste.external.resource.suoritusrekisteri.dto.Suoritus;
import fi.vm.sade.valinta.kooste.external.resource.suoritusrekisteri.dto.SuoritusJaArvosanat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import org.joda.time.format.DateTimeFormat;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

@Service
public class HarkinnanvaraisuusAsyncResourceImpl implements HarkinnanvaraisuusAsyncResource {

  private static final Logger LOG =
      LoggerFactory.getLogger(HarkinnanvaraisuusAsyncResourceImpl.class);

  private final LocalDateTime suoritusValmisDeadline;

  public static final org.joda.time.format.DateTimeFormatter VALMISTUMINEN_DTF =
      DateTimeFormat.forPattern("dd.MM.yyyy");

  @Autowired
  public HarkinnanvaraisuusAsyncResourceImpl(
      @Value("${valintalaskentakoostepalvelu.harkinnanvaraisuus.paattely.leikkuripvm:2022-02-02}")
          String suoritusValmisDeadlinePvm) {
    this.suoritusValmisDeadline = LocalDate.parse(suoritusValmisDeadlinePvm).atStartOfDay();
  }

  public static final String PK_KOMO = "1.2.246.562.13.62959769647";

  private Boolean viimeisinPeruskoulusuoritusOnKeskeytynyt(List<Oppija> oppijas) {
    return oppijas.stream()
        .anyMatch(
            oppija -> {
              List<Suoritus> pkSuoritukset =
                  oppija.getSuoritukset().stream()
                      .map(SuoritusJaArvosanat::getSuoritus)
                      .filter(s -> PK_KOMO.equals(s.getKomo()) && s.isVahvistettu())
                      .collect(Collectors.toList());
              Optional<Suoritus> viimeisin =
                  pkSuoritukset.stream()
                      .max(
                          Comparator.comparing(
                              s -> VALMISTUMINEN_DTF.parseDateTime(s.getValmistuminen())));
              return viimeisin.map(s -> "KESKEYTYNYT".equals(s.getTila())).orElse(false);
            });
  }

  private Boolean hasPeruskouluWithTilas(List<Oppija> oppijas, List<String> tilas) {
    if (!oppijas.isEmpty()) {
      return oppijas.stream()
          .anyMatch(
              oppija ->
                  oppija.getSuoritukset().stream()
                      .anyMatch(
                          sa ->
                              PK_KOMO.equals(sa.getSuoritus().getKomo())
                                  && tilas.contains(sa.getSuoritus().getTila())
                                  && sa.getSuoritus().isVahvistettu()));
    } else {
      return false;
    }
  }

  private Boolean hasYksilollistettyMatAi(List<Oppija> oppijas) {
    if (!oppijas.isEmpty()) {
      return oppijas.stream()
          .anyMatch(oppija -> loytyyYksilollistettyMaAi(oppija.getSuoritukset()));
    } else {
      return false;
    }
  }

  private boolean loytyyYksilollistettyMaAi(List<SuoritusJaArvosanat> suoritukset) {
    return suoritukset.stream()
        .anyMatch(
            sa ->
                PK_KOMO.equals(sa.getSuoritus().getKomo())
                    && sa.getSuoritus().isYksilollistettyMaAi()
                    && sa.getSuoritus().isVahvistettu());
  }

  private Boolean hasPkSuoritusWithoutYksilollistettyMatAi2018Jalkeen(List<Oppija> oppijas) {
    if (!oppijas.isEmpty()) {
      return oppijas.stream()
          .anyMatch(
              oppija ->
                  oppija.getSuoritukset().stream()
                      .anyMatch(
                          sa ->
                              PK_KOMO.equals(sa.getSuoritus().getKomo())
                                  && VALMISTUMINEN_DTF
                                          .parseDateTime(sa.getSuoritus().getValmistuminen())
                                          .getYear()
                                      >= 2018
                                  && !sa.getSuoritus().isYksilollistettyMaAi()
                                  && sa.getSuoritus().isVahvistettu()));
    } else {
      return false;
    }
  }

  private void setHarkinnanvaraisuudenSyyForHakutoive(
      HakutoiveenHarkinnanvaraisuus ht, HarkinnanvaraisuudenSyy syy) {
    if (ht.getHarkinnanvaraisuudenSyy() != HarkinnanvaraisuudenSyy.EI_HARKINNANVARAINEN_HAKUKOHDE) {
      ht.setHarkinnanvaraisuudenSyy(syy);
    }
  }

  private HakemuksenHarkinnanvaraisuus syncHarkinnanvaraisuusForHakemus(
      String hakemusOid,
      String henkiloOidHakemukselta,
      List<HakutoiveenHarkinnanvaraisuus> tiedotAtarusta,
      List<Oppija> oppijas) {
    if (oppijas.isEmpty()) {
      LOG.warn(
          "Hakemuksen {} henkiloOidille {} ei löytynyt suresta Oppijaa!",
          hakemusOid,
          henkiloOidHakemukselta);
    } else if (oppijas.size() > 1) {
      LOG.warn(
          "Hakemuksen {} henkiloOidille {} löytyi suresta yhteensä {} oppijaa!",
          hakemusOid,
          henkiloOidHakemukselta,
          oppijas.size());
    } else {
      LOG.info(
          "Käsitellään hakemus {} henkilön {} sure-tiedoilla, suorituksia yhteensä {}",
          hakemusOid,
          oppijas.get(0).getOppijanumero(),
          oppijas.get(0).getSuoritukset().size());
    }
    HakemuksenHarkinnanvaraisuus result = null;
    if (viimeisinPeruskoulusuoritusOnKeskeytynyt(oppijas)) {
      LOG.info(
          "Hakemus {} on suren mukaan harkinnanvarainen, koska oppijan viimeisin peruskoulusuoritus on keskeytynyt",
          hakemusOid);
      tiedotAtarusta.forEach(
          ht ->
              setHarkinnanvaraisuudenSyyForHakutoive(
                  ht, HarkinnanvaraisuudenSyy.SURE_EI_PAATTOTODISTUSTA));
      result = new HakemuksenHarkinnanvaraisuus(hakemusOid, tiedotAtarusta);
    } else if (LocalDateTime.now().isAfter(suoritusValmisDeadline)
        && !hasPeruskouluWithTilas(oppijas, List.of("VALMIS"))
        && hasPeruskouluWithTilas(oppijas, List.of("KESKEN", "KESKEYTYNYT"))) {
      LOG.info(
          "Hakemus {} on suren mukaan harkinnanvarainen, koska suressa ei päättötodistusta ja deadline on ohitettu",
          hakemusOid);
      tiedotAtarusta.forEach(
          ht ->
              setHarkinnanvaraisuudenSyyForHakutoive(
                  ht, HarkinnanvaraisuudenSyy.SURE_EI_PAATTOTODISTUSTA));
      result = new HakemuksenHarkinnanvaraisuus(hakemusOid, tiedotAtarusta);
    } else if (hasYksilollistettyMatAi(oppijas)) {
      LOG.info("Hakemuksella {} on suressa yksilollistetty MA_AI!", hakemusOid);
      tiedotAtarusta.forEach(
          ht ->
              setHarkinnanvaraisuudenSyyForHakutoive(ht, HarkinnanvaraisuudenSyy.SURE_YKS_MAT_AI));
      result = new HakemuksenHarkinnanvaraisuus(hakemusOid, tiedotAtarusta);
    } else {
      LOG.info(
          "Käytetään hakemukselle {} atarun harkinnanvaraisuustietoja: {}",
          hakemusOid,
          tiedotAtarusta.stream()
              .map(ht -> ht.getHakukohdeOid() + " - " + ht.getHarkinnanvaraisuudenSyy())
              .collect(Collectors.toList()));
      tiedotAtarusta.forEach(
          hh -> {
            if (hh.getHarkinnanvaraisuudenSyy()
                    .equals(HarkinnanvaraisuudenSyy.ATARU_EI_PAATTOTODISTUSTA)
                || hh.getHarkinnanvaraisuudenSyy()
                    .equals(HarkinnanvaraisuudenSyy.ATARU_ULKOMAILLA_OPISKELTU)) {
              if (hasPeruskouluWithTilas(oppijas, List.of("VALMIS"))) {
                LOG.info(
                    "Hakemuksella {} harkinnanvaraiseksi merkitty hakutoive {} ei ole harkinnanvarainen, koska suresta löytyy valmis pohjakoulutus!",
                    hakemusOid,
                    hh.getHakukohdeOid());
                setHarkinnanvaraisuudenSyyForHakutoive(
                    hh, HarkinnanvaraisuudenSyy.EI_HARKINNANVARAINEN);
              } else if (LocalDateTime.now().isBefore(suoritusValmisDeadline)
                  && hasPeruskouluWithTilas(oppijas, List.of("KESKEN"))) {
                LOG.info(
                    "Hakemuksella {} harkinnanvaraiseksi merkitty hakutoive {} ei ole harkinnanvarainen, koska suresta löytyy kesken-tilainen peruskoulusuoritus ja deadlinea ei ole ohitettu!",
                    hakemusOid,
                    hh.getHakukohdeOid());
                setHarkinnanvaraisuudenSyyForHakutoive(
                    hh, HarkinnanvaraisuudenSyy.EI_HARKINNANVARAINEN);
              }
            }
            // yliajetaan vanha yksilöllistetyn mat/ai perusteella harkinnanvaraisuus
            // jos korotuksia, sekä atarun että varalta myös suren osalta
            if ((hh.getHarkinnanvaraisuudenSyy().equals(HarkinnanvaraisuudenSyy.ATARU_YKS_MAT_AI)
                    || hh.getHarkinnanvaraisuudenSyy()
                        .equals(HarkinnanvaraisuudenSyy.SURE_YKS_MAT_AI))
                && hasPkSuoritusWithoutYksilollistettyMatAi2018Jalkeen(oppijas)) {
              LOG.info(
                  "Hakemuksella {} harkinnanvaraiseksi merkitty hakutoive {} ei ole harkinnanvarainen, koska suresta löytyy 2018 "
                      + "tai myöhäisempi suoritus ilman yksilöllistettyä matematiikkaa ja äidinkieltä!",
                  hakemusOid,
                  hh.getHakukohdeOid());
              setHarkinnanvaraisuudenSyyForHakutoive(
                  hh, HarkinnanvaraisuudenSyy.EI_HARKINNANVARAINEN);
            }
          });
      result = new HakemuksenHarkinnanvaraisuus(hakemusOid, tiedotAtarusta);
    }
    LOG.info(
        "Harkinnanvaraisuuden tulos hakemukselle {}: {}",
        hakemusOid,
        result.getHakutoiveet().stream()
            .map(hh -> hh.getHakukohdeOid() + " - " + hh.getHarkinnanvaraisuudenSyy())
            .collect(Collectors.toList()));
    return result;
  }

  @Override
  public Boolean hasYksilollistettyMatAi(HakemuksenHarkinnanvaraisuus h, Oppija o) {
    boolean isYksMatAi =
        syncHarkinnanvaraisuusForHakemus(
                h.getHakemusOid(), o.getOppijanumero(), h.getHakutoiveet(), List.of(o))
            .hasYksilollistettyMatAi();
    LOG.info("hasYksilollistettyMatAi? {} : {} ", h.getHakemusOid(), isYksMatAi);
    return isYksMatAi;
  }
}