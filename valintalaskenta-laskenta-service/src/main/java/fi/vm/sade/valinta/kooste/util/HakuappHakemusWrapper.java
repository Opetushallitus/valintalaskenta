package fi.vm.sade.valinta.kooste.util;

import static fi.vm.sade.valinta.kooste.util.Converter.*;
import static java.util.Collections.emptyList;

import com.google.common.collect.Lists;
import fi.vm.sade.valinta.kooste.external.resource.hakuapp.dto.Maksuvelvollisuus;
import fi.vm.sade.valinta.kooste.external.resource.ataru.dto.AtaruHakutoive;
import fi.vm.sade.valinta.kooste.external.resource.hakuapp.dto.Eligibility;
import fi.vm.sade.valinta.kooste.external.resource.hakuapp.dto.Hakemus;
import fi.vm.sade.valinta.kooste.external.resource.hakuapp.dto.Yhteystiedot;
import fi.vm.sade.valinta.kooste.external.resource.valintapiste.dto.Valintapisteet;
import fi.vm.sade.valintalaskenta.domain.dto.AvainArvoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakukohdeDTO;
import java.util.*;
import java.util.Map.Entry;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.math.NumberUtils;

/** Hakemustietojen luku hakemustietueesta vikasietoisesti */
public class HakuappHakemusWrapper extends HakemusWrapper {
  private final Hakemus hakemus;
  private Map<String, String> henkilotiedot = null;
  private Map<String, String> lisatiedot = null;
  private Map<String, String> hakutoiveet = null;
  private Map<String, String> koulutustausta = null;
  public static final String ETUNIMET = "Etunimet";
  private static final String KUTSUMANIMI = "Kutsumanimi";
  public static final String SUKUNIMI = "Sukunimi";
  public static final String ASIOINTIKIELI = "asiointikieli";
  private static final String LUPAJULKAISUUN = "lupaJulkaisu";
  public static final String HETU = "Henkilotunnus";
  public static final String SAHKOPOSTI = "Sähköposti";
  public static final String SYNTYMAAIKA = "syntymaaika";
  private static final String KANSALLINEN_ID = "kansallinenIdTunnus";
  private static final String PASSINNUMERO = "passinnumero";
  private static final String KANSALAISUUS = "kansalaisuus";
  private static final String POSTINUMERO_ULKOMAA = "postinumeroUlkomaa";
  private static final String KAUPUNKI_ULKOMAA = "kaupunkiUlkomaa";
  private static final String ASUINMAA = "asuinmaa";
  private static final String SUOMALAINEN_LAHIOSOITE = "lahiosoite";
  private static final String SUOMALAINEN_POSTINUMERO = "Postinumero";
  private static final String OSOITE_ULKOMAA = "osoiteUlkomaa";
  private static final String SUKUPUOLI = "sukupuoli";
  private static final String AIDINKIELI = "aidinkieli";
  private static final String KOTIKUNTA = "kotikunta";
  public static final String TOISEN_ASTEEN_SUORITUS = "toisen_asteen_suoritus";
  public static final String TOISEN_ASTEEN_SUORITUSMAA = "toisen_asteen_suoritusmaa";
  public static final String LUPA_SAHKOISEEN_VIESTINTAAN = "lupatiedot-sahkoinen-viestinta";
  public static final String LUPA_SAHKOISEEN_ASIOINTIIN = "lupatiedot-sahkoinen-asiointi";
  public static final String LUPA_TULOS_EMAIL = "lupaTulosEmail";
  private static final String ULKOMAA_POSTITOIMIPAIKKA = "kaupunkiUlkomaa";
  private static final String POHJAKOULUTUS = "POHJAKOULUTUS";
  private static final String POHJAKOULUTUS_ULKOMAILLA = "0";
  private static final String POHJAKOULUTUS_KESKEYTETTY = "7";
  private static final String PREFERENCE = "preference";
  private static final String KOULUTUS_ID = "Koulutus-id";
  private static final String DISCRETIONARY = "discretionary";

  private Yhteystiedot yhteystiedot = null;

  public HakuappHakemusWrapper(Hakemus hakemus) {
    this.hakemus = Objects.requireNonNull(hakemus, "Hakuapp hakemus oli null.");
  }

  @Override
  public String getOid() {
    return hakemus.getOid();
  }

  @Override
  public String getUlkomainenLahiosoite() {
    getHenkilotiedot();
    return Optional.ofNullable(henkilotiedot.get(OSOITE_ULKOMAA)).orElse(StringUtils.EMPTY);
  }

  @Override
  public String getSukupuoli() {
    getHenkilotiedot();
    return Stream.of(Optional.ofNullable(henkilotiedot.get(SUKUPUOLI)).orElse(StringUtils.EMPTY))
        .map(
            s -> {
              if (NAINEN.equals(s)) {
                return "Nainen";
              } else if (MIES.equals(s)) {
                return "Mies";
              }
              return s;
            })
        .findAny()
        .get();
  }

  @Override
  public String getSukupuoliAsIs() {
    getHenkilotiedot();
    return Optional.ofNullable(henkilotiedot.get(SUKUPUOLI)).orElse(StringUtils.EMPTY);
  }

  @Override
  public String getAidinkieli() {
    getHenkilotiedot();
    return Optional.ofNullable(henkilotiedot.get(AIDINKIELI)).orElse(StringUtils.EMPTY);
  }

  @Override
  public String getKaupunkiUlkomaa() {
    getHenkilotiedot();
    return Optional.ofNullable(henkilotiedot.get(KAUPUNKI_ULKOMAA)).orElse(StringUtils.EMPTY);
  }

  @Override
  public String getUlkomainenPostinumero() {
    getHenkilotiedot();
    return Optional.ofNullable(henkilotiedot.get(POSTINUMERO_ULKOMAA)).orElse(StringUtils.EMPTY);
  }

  @Override
  public String getUlkomainenPostitoimipaikka() {
    getHenkilotiedot();
    return Optional.ofNullable(henkilotiedot.get(ULKOMAA_POSTITOIMIPAIKKA))
        .orElse(StringUtils.EMPTY);
  }

  @Override
  public String getSuomalainenLahiosoite() {
    getHenkilotiedot();
    return Optional.ofNullable(henkilotiedot.get(SUOMALAINEN_LAHIOSOITE)).orElse(StringUtils.EMPTY);
  }

  @Override
  public String getSuomalainenPostinumero() {
    getHenkilotiedot();
    return Optional.ofNullable(henkilotiedot.get(SUOMALAINEN_POSTINUMERO))
        .orElse(StringUtils.EMPTY);
  }

  @Override
  public String getAsuinmaa() {
    getHenkilotiedot();
    return Optional.ofNullable(henkilotiedot.get(ASUINMAA)).orElse(StringUtils.EMPTY);
  }

  @Override
  public String getKansallinenId() {
    getHenkilotiedot();
    return Optional.ofNullable(henkilotiedot.get(KANSALLINEN_ID)).orElse(StringUtils.EMPTY);
  }

  @Override
  public String getKansalaisuus() {
    getHenkilotiedot();
    return Optional.ofNullable(henkilotiedot.get(KANSALAISUUS)).orElse(StringUtils.EMPTY);
  }

  @Override
  public String getPassinnumero() {
    getHenkilotiedot();
    return Optional.ofNullable(henkilotiedot.get(PASSINNUMERO)).orElse(StringUtils.EMPTY);
  }

  @Override
  public String getKotikunta() {
    getHenkilotiedot();
    return Optional.ofNullable(henkilotiedot.get(KOTIKUNTA)).orElse(StringUtils.EMPTY);
  }

  @Override
  public String getPuhelinnumero() {
    if (yhteystiedot == null) {
      this.yhteystiedot = Yhteystiedot.yhteystiedotHakemukselta(this);
    }
    return yhteystiedot.getPuhelinnumerotAsString();
  }

  @Override
  public Collection<String> getPuhelinnumerot() {
    TreeMap<String, String> henkilotiedot =
        new TreeMap<String, String>(String.CASE_INSENSITIVE_ORDER);
    henkilotiedot.putAll(hakemus.getAnswers().getHenkilotiedot());
    Collection<String> nums = Lists.newArrayList();
    for (Entry<String, String> e :
        henkilotiedot.tailMap(Yhteystiedot.MATKAPUHELINNUMERO, true).entrySet()) {
      if (e.getKey().startsWith(Yhteystiedot.MATKAPUHELINNUMERO)) {
        nums.add(e.getValue());
      } else {
        break;
      }
    }
    return nums;
  }

  @Override
  public boolean isMaksuvelvollinen(String hakukohdeOid) {
    List<Eligibility> eligibilities =
        Optional.ofNullable(hakemus)
            .map(Hakemus::getPreferenceEligibilities)
            .filter(Objects::nonNull)
            .orElse(emptyList());
    Optional<Eligibility> eligibilityForHakukohde =
        eligibilities.stream().filter(e -> hakukohdeOid.equals(e.getAoId())).findAny();
    return eligibilityForHakukohde
        .filter(e -> Maksuvelvollisuus.REQUIRED.equals(e.getMaksuvelvollisuus()))
        .isPresent();
  }

  @Override
  public String getSahkopostiOsoite() {
    getHenkilotiedot();
    return Optional.ofNullable(henkilotiedot.get(SAHKOPOSTI)).orElse(StringUtils.EMPTY);
  }

  @Override
  public String getSyntymaaika() {
    getHenkilotiedot();
    return Optional.ofNullable(henkilotiedot.get(SYNTYMAAIKA)).orElse(StringUtils.EMPTY);
  }

  @Override
  public String getSyntymaaikaForErillishaku() {
    return getSyntymaaika();
  }

  @Override
  public String getHenkilotunnus() {
    getHenkilotiedot();
    return Optional.ofNullable(henkilotiedot.get(HETU)).orElse(StringUtils.EMPTY);
  }

  @Override
  public boolean hasHenkilotunnus() {
    getHenkilotiedot();
    return Optional.ofNullable(henkilotiedot.get(HETU)).isPresent();
  }

  @Override
  public String getPersonOid() {
    if (hakemus == null) {
      return null;
    }
    return hakemus.getPersonOid();
  }

  @Override
  public String getApplicationPersonOid() {
    if (hakemus == null) {
      return null;
    }
    return hakemus.getPersonOid();
  }

  @Override
  public Integer getHakutoiveenPrioriteetti(String hakukohdeOid) {
    getHakutoiveet();

    if (hakutoiveet.containsValue(hakukohdeOid)) {
      for (Entry<String, String> s : hakutoiveet.entrySet()) {
        if (hakukohdeOid.equals(s.getValue())) {
          String value = s.getKey().split("preference")[1].split("-")[0];
          return NumberUtils.isNumber(value) ? NumberUtils.toInt(value) : null;
        }
      }
    }
    return null;
  }

  @Override
  public Boolean getToisenAsteenSuoritus() {
    getKoulutustausta();
    if (koulutustausta.containsKey(TOISEN_ASTEEN_SUORITUS)) {
      String l = koulutustausta.get(TOISEN_ASTEEN_SUORITUS);
      return BooleanUtils.toBooleanObject(l);
    }
    return null;
  }

  @Override
  public String getToisenAsteenSuoritusmaa() {
    getKoulutustausta();
    if (koulutustausta.containsKey(TOISEN_ASTEEN_SUORITUSMAA)) {
      return koulutustausta.get(TOISEN_ASTEEN_SUORITUSMAA);
    }
    return StringUtils.EMPTY;
  }

  @Override
  public String getEtunimi() {
    getHenkilotiedot(); // lazy load henkilotiedot
    if (henkilotiedot.containsKey(KUTSUMANIMI)) {
      return henkilotiedot.get(KUTSUMANIMI);
    } else if (henkilotiedot.containsKey(ETUNIMET)) {
      return henkilotiedot.get(ETUNIMET);
    } else {
      return StringUtils.EMPTY;
    }
  }

  @Override
  public String getEtunimet() {
    getHenkilotiedot(); // lazy load henkilotiedot
    if (henkilotiedot.containsKey(ETUNIMET)) {
      return henkilotiedot.get(ETUNIMET);
    } else if (henkilotiedot.containsKey(KUTSUMANIMI)) {
      return henkilotiedot.get(KUTSUMANIMI);
    } else {
      return StringUtils.EMPTY;
    }
  }

  @Override
  public String getKutsumanimi() {
    getHenkilotiedot();
    return henkilotiedot.get("Kutsumanimi");
  }

  @Override
  public String getSukunimi() {
    getHenkilotiedot(); // lazy load henkilotiedot
    if (henkilotiedot.containsKey(SUKUNIMI)) {
      return henkilotiedot.get(SUKUNIMI);
    } else {
      return StringUtils.EMPTY;
    }
  }

  @Override
  public boolean getLupaJulkaisuun() {
    getLisatiedot(); // lazy load lisatiedot
    if (lisatiedot.containsKey(LUPAJULKAISUUN)) {
      String l = lisatiedot.get(LUPAJULKAISUUN);
      return Boolean.TRUE.equals(Boolean.valueOf(l));
    }
    return false;
  }

  @Override
  public boolean getVainSahkoinenViestinta() {
    getLisatiedot(); // lazy load lisätiedot
    if (lisatiedot.containsKey(LUPA_SAHKOISEEN_VIESTINTAAN)) {
      String l = lisatiedot.get(LUPA_SAHKOISEEN_VIESTINTAAN);
      return Boolean.TRUE.equals(Boolean.valueOf(l));
    }
    return false;
  }

  @Override
  public boolean hasAsiointikieli() {
    getLisatiedot();
    return lisatiedot.containsKey(ASIOINTIKIELI);
  }

  @Override
  public String getAsiointikieli() {
    getLisatiedot(); // lazy load lisatiedot
    if (lisatiedot.containsKey(ASIOINTIKIELI)) {
      return KieliUtil.normalisoiKielikoodi(lisatiedot.get(ASIOINTIKIELI));
    } else {
      return KieliUtil.SUOMI;
    }
  }

  /*
  @Override
  public boolean getLupaSahkoiseenAsiointiin() {
      getLisatiedot(); // lazy load lisatiedot
      if (lisatiedot.containsKey(LUPA_SAHKOISEEN_VIESTINTAAN)) {
          String lupa = lisatiedot.get(LUPA_SAHKOISEEN_VIESTINTAAN);
          return Boolean.TRUE.equals(Boolean.valueOf(lupa));
      }
      return false;
  }
   */

  @Override
  public boolean getLupaSahkoiseenAsiointiin() {
    getHenkilotiedot(); // lazy load henkilotiedot
    if (henkilotiedot.containsKey(LUPA_SAHKOISEEN_ASIOINTIIN)) {
      String lupa = henkilotiedot.get(LUPA_SAHKOISEEN_ASIOINTIIN);
      return Boolean.TRUE.equals(Boolean.valueOf(lupa));
    }
    return false;
  }

  @Override
  public boolean getLupaTulosEmail() {
    getLisatiedot();
    if (lisatiedot.containsKey(LUPA_TULOS_EMAIL)) {
      String lupa = lisatiedot.get(LUPA_TULOS_EMAIL);
      return Boolean.TRUE.equals(Boolean.valueOf(lupa));
    }
    return false;
  }

  @Override
  public Collection<String> getHakutoiveOids() {
    return getHakutoiveet().entrySet().stream()
        .filter(
            entry ->
                entry.getKey().startsWith("preference") && entry.getKey().endsWith("-Koulutus-id"))
        .map(entry -> StringUtils.trimToNull(entry.getValue()))
        .filter(Objects::nonNull)
        .collect(Collectors.toSet());
  }

  @Override
  public String getMaksuvelvollisuus(String hakukohdeOid) {
    String result = Maksuvelvollisuus.NOT_CHECKED;
    if (hakukohdeOid != null && hakemus.getPreferenceEligibilities() != null) {
      for (Eligibility e : hakemus.getPreferenceEligibilities()) {
        if (e.getAoId().equals(hakukohdeOid)) {
          result = e.getMaksuvelvollisuus();
          break;
        }
      }
    }
    return result;
  }

  @Override
  public boolean ulkomaillaSuoritettuKoulutusTaiOppivelvollisuudenKeskeyttanyt() {
    if (hakemus.getAnswers().getKoulutustausta() != null) {
      Map<String, String> koulutustausta =
          new TreeMap<String, String>(String.CASE_INSENSITIVE_ORDER);
      koulutustausta.putAll(hakemus.getAnswers().getKoulutustausta());
      String pohjakoulutus = koulutustausta.get(POHJAKOULUTUS);
      return POHJAKOULUTUS_ULKOMAILLA.equals(pohjakoulutus)
          || POHJAKOULUTUS_KESKEYTETTY.equals(pohjakoulutus);
    }
    return false;
  }

  @Override
  public String getHakuoid() {
    return hakemus.getApplicationSystemId();
  }

  @Override
  public String getState() {
    return hakemus.getState();
  }

  @Override
  public String getSyntymapaikka() {
    return null;
  }

  @Override
  public int hashCode() {
    return Optional.ofNullable(getOid()).orElse("").hashCode();
  }

  @Override
  public boolean equals(Object obj) {
    if (obj == null) {
      return false;
    } else if (this == obj) {
      return true;
    } else if (obj instanceof HakuappHakemusWrapper) {
      return this.getOid().equals(((HakuappHakemusWrapper) obj).getOid());
    } else {
      return false;
    }
  }

  @Override
  public HakemusDTO toHakemusDto(
      Valintapisteet valintapisteet,
      Map<String, List<String>> hakukohdeRyhmasForHakukohdes,
      boolean shouldUseApplicationPersonOid) {
    HakemusDTO hakemusTyyppi = new HakemusDTO();
    hakemusTyyppi.setHakemusoid(getOid());
    hakemusTyyppi.setHakijaOid(getPersonOid());
    hakemusTyyppi.setHakuoid(getHakuoid());
    if (hakemus.getAnswers() != null) {
      try {
        if (hakemus.getAnswers().getHenkilotiedot() != null) {
          hakemusTyyppi.setEtunimi(hakemus.getAnswers().getHenkilotiedot().get(ETUNIMET));
          hakemusTyyppi.setSukunimi(hakemus.getAnswers().getHenkilotiedot().get(SUKUNIMI));
          for (Entry<String, String> e : hakemus.getAnswers().getHenkilotiedot().entrySet()) {
            AvainArvoDTO aa = new AvainArvoDTO();
            aa.setAvain(e.getKey());
            aa.setArvo(sanitizeArvo(e.getValue()));
            hakemusTyyppi.getAvaimet().add(aa);
          }
        }
      } catch (Exception e) {
        throw e;
      }

      try {
        try {
          hakemus
              .getAnswers()
              .getHakutoiveet()
              .putAll(
                  mapEligibilityAndStatus(
                      hakemus.getPreferenceEligibilities(), hakemus.getAnswers().getHakutoiveet()));
        } catch (Exception e) {
          throw new RuntimeException(
              "Eligibilities statusten mappaus preferensseihin epaonnistui!", e);
        }
        Map<Integer, Converter.Hakutoive> hakutoiveet = new HashMap<>();
        if (hakemus.getAnswers().getHakutoiveet() != null) {
          for (Entry<String, String> e : hakemus.getAnswers().getHakutoiveet().entrySet()) {
            AvainArvoDTO aa = new AvainArvoDTO();
            aa.setAvain(e.getKey());
            aa.setArvo(sanitizeArvo(e.getValue()));
            hakemusTyyppi.getAvaimet().add(aa);
            if (e.getKey().startsWith(PREFERENCE)) {
              Integer prioriteetti;
              String numberAfterPreference;
              try {
                numberAfterPreference = e.getKey().replaceAll("\\D+", "");
                if (StringUtils.isBlank(numberAfterPreference)) {
                  continue;
                }
                prioriteetti = Integer.valueOf(numberAfterPreference);
              } catch (Exception ee) {
                throw ee;
              }
              Converter.Hakutoive hakutoive;
              if (!hakutoiveet.containsKey(prioriteetti)) {
                hakutoive = new Converter.Hakutoive();
                hakutoiveet.put(prioriteetti, hakutoive);
              } else {
                hakutoive = hakutoiveet.get(prioriteetti);
              }

              if (e.getKey().endsWith(KOULUTUS_ID)) {
                hakutoive.setHakukohdeOid(e.getValue());
              } else if (e.getKey().endsWith(DISCRETIONARY)) {
                Boolean discretionary = Boolean.valueOf(e.getValue());
                discretionary = discretionary == null ? false : discretionary;
                hakutoive.setHarkinnanvaraisuus(discretionary);
              }
            }
          }

          for (Entry<Integer, Converter.Hakutoive> e : hakutoiveet.entrySet()) {
            Converter.Hakutoive hakutoive = e.getValue();
            if (hakutoive != null) {
              if (hakutoive.getHakukohdeOid() != null
                  && !hakutoive.getHakukohdeOid().trim().isEmpty()) {
                HakukohdeDTO hk = new HakukohdeDTO();
                hk.setOid(hakutoive.getHakukohdeOid());
                hk.setHarkinnanvaraisuus(Boolean.TRUE.equals(hakutoive.getHarkinnanvaraisuus()));
                hk.setPrioriteetti(e.getKey());
                hk.setHakukohdeRyhmatOids(
                    hakukohdeRyhmasForHakukohdes.get(hakutoive.getHakukohdeOid()));
                hakemusTyyppi.getHakukohteet().add(hk);
              }
            }
          }
        }
      } catch (Exception e) {
        throw e;
      }

      try {
        if (hakemus.getAnswers().getKoulutustausta() != null) {
          for (Entry<String, String> e : hakemus.getAnswers().getKoulutustausta().entrySet()) {
            AvainArvoDTO aa = new AvainArvoDTO();
            aa.setAvain(e.getKey());
            aa.setArvo(sanitizeArvo(e.getValue()));
            hakemusTyyppi.getAvaimet().add(aa);
          }
        }
      } catch (Exception e) {
        throw e;
      }

      try {
        if (hakemus.getAnswers().getLisatiedot() != null) {
          for (Entry<String, String> e : hakemus.getAnswers().getLisatiedot().entrySet()) {
            AvainArvoDTO aa = new AvainArvoDTO();
            aa.setAvain(e.getKey());
            aa.setArvo(sanitizeArvo(e.getValue()));
            hakemusTyyppi.getAvaimet().add(aa);
          }
        }
      } catch (Exception e) {
        throw e;
      }

      try {
        if (hakemus.getAnswers().getOsaaminen() != null) {
          final List<AvainArvoDTO> osaaminenIlmanArvosanoja =
              hakemus.getAnswers().getOsaaminen().entrySet().stream()
                  .filter(
                      entry ->
                          !entry.getKey().startsWith("PK_") && !entry.getKey().startsWith("LK_"))
                  .map(
                      e -> {
                        AvainArvoDTO aa = new AvainArvoDTO();
                        aa.setAvain(e.getKey());
                        aa.setArvo(sanitizeArvo(e.getValue()));
                        return aa;
                      })
                  .collect(Collectors.toList());
          hakemusTyyppi.getAvaimet().addAll(osaaminenIlmanArvosanoja);
        }
      } catch (Exception e) {
        throw e;
      }
    }
    setHakemusDTOvalintapisteet(valintapisteet, hakemusTyyppi);

    return hakemusTyyppi;
  }

  @Override
  public List<AtaruHakutoive> ataruHakutoiveet() {
    return Collections.emptyList();
  }

  @Override
  public List<String> getHuoltajienSahkopostiosoitteet() {
    return new ArrayList<>();
  }

  private Map<String, String> getHakutoiveet() {
    if (hakutoiveet == null) {
      hakutoiveet = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);
      hakutoiveet.putAll(hakemus.getAnswers().getHakutoiveet());
    }
    return hakutoiveet;
  }

  private Map<String, String> getLisatiedot() {
    if (lisatiedot == null) {
      lisatiedot = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);
      lisatiedot.putAll(hakemus.getAnswers().getLisatiedot());
    }
    return lisatiedot;
  }

  private Map<String, String> getHenkilotiedot() {
    if (henkilotiedot == null) {
      henkilotiedot = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);
      henkilotiedot.putAll(hakemus.getAnswers().getHenkilotiedot());
    }
    return henkilotiedot;
  }

  private Map<String, String> getKoulutustausta() {
    if (koulutustausta == null) {
      koulutustausta = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);
      koulutustausta.putAll(hakemus.getAnswers().getKoulutustausta());
    }
    return koulutustausta;
  }
}
