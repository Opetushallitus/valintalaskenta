package fi.vm.sade.valinta.kooste.util;

import static fi.vm.sade.valinta.kooste.util.Converter.setHakemusDTOvalintapisteet;
import static fi.vm.sade.valinta.kooste.valintalaskenta.util.HakemuksetConverterUtil.ATARU_POHJAKOULUTUS_VUOSI;

import com.google.common.base.Strings;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Lists;
import fi.vm.sade.valinta.kooste.external.resource.ataru.dto.AtaruHakemus;
import fi.vm.sade.valinta.kooste.external.resource.ataru.dto.AtaruHakutoive;
import fi.vm.sade.valinta.kooste.external.resource.oppijanumerorekisteri.dto.HenkiloPerustietoDto;
import fi.vm.sade.valinta.kooste.external.resource.valintapiste.dto.Valintapisteet;
import fi.vm.sade.valintalaskenta.domain.dto.AvainArvoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakukohdeDTO;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import org.apache.commons.lang3.StringUtils;

public class AtaruHakemusWrapper extends HakemusWrapper {

  private final AtaruHakemus hakemus;
  private final Map<String, String> keyvalues;
  private final HenkiloPerustietoDto henkilo;
  private List<String> kansalaisuudet;
  private static final String PREFERENCE_REGEX = "preference\\d-Koulutus-id-eligibility";
  private static final ImmutableMap<String, String> ELIGIBILITIES =
      new ImmutableMap.Builder<String, String>()
          .put("eligible", "ELIGIBLE")
          .put("uneligible", "INELIGIBLE")
          .put("unreviewed", "NOT_CHECKED")
          .put("conditionally-eligible", "CONDITIONALLY_ELIGIBLE")
          .build();

  public AtaruHakemusWrapper(AtaruHakemus ataruHakemus, HenkiloPerustietoDto onrHenkilo) {
    hakemus = Objects.requireNonNull(ataruHakemus, "Ataruhakemus oli null.");
    keyvalues = ataruHakemus.getKeyValues();
    henkilo = Objects.requireNonNull(onrHenkilo, "Henkilo ataruhakemukselle oli null.");
  }

  @Override
  public String getOid() {
    return StringUtils.trimToEmpty(hakemus.getHakemusOid());
  }

  @Override
  public String getUlkomainenLahiosoite() {
    return getIfUlkomainenOsoiteOrEmpty("address");
  }

  @Override
  public String getSukupuoli() {
    return Stream.of(Optional.ofNullable(henkilo.getSukupuoli()).orElse(StringUtils.EMPTY))
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
    return StringUtils.trimToEmpty(henkilo.getSukupuoli());
  }

  @Override
  public String getAidinkieli() {
    if (null == henkilo.getAidinkieli()
        || StringUtils.isBlank(henkilo.getAidinkieli().getKieliKoodi())) {
      throw new IllegalStateException(
          String.format("Henkilöllä %s ei ole äidinkieltä", henkilo.getOidHenkilo()));
    }
    return henkilo.getAidinkieli().getKieliKoodi();
  }

  @Override
  public String getKaupunkiUlkomaa() {
    return getIfUlkomainenOsoiteOrEmpty("city");
  }

  @Override
  public String getUlkomainenPostinumero() {
    return getIfUlkomainenOsoiteOrEmpty("postal-code");
  }

  @Override
  public String getUlkomainenPostitoimipaikka() {
    return getIfUlkomainenOsoiteOrEmpty("city");
  }

  @Override
  public String getSuomalainenLahiosoite() {
    return getIfSuomalainenOsoiteOrEmpty("address");
  }

  @Override
  public String getSuomalainenPostinumero() {
    return getIfSuomalainenOsoiteOrEmpty("postal-code");
  }

  @Override
  public String getAsuinmaa() {
    return StringUtils.trimToEmpty(keyvalues.get("country-of-residence"));
  }

  @Override
  public String getKansallinenId() {
    return StringUtils.trimToEmpty(keyvalues.get("national-id-number"));
  }

  @Override
  public String getKansalaisuus() {
    return kansalaisuudet != null
        ? kansalaisuudet.iterator().next()
        : StringUtils.trimToEmpty(
            henkilo.getKansalaisuus().iterator().next().getKansalaisuusKoodi());
  }

  public void setKansalaisuus(List<String> kansalaisuudet) {
    this.kansalaisuudet = kansalaisuudet;
  }

  @Override
  public String getPassinnumero() {
    return StringUtils.trimToEmpty(keyvalues.get("passport-number"));
  }

  @Override
  public String getKotikunta() {
    return StringUtils.trimToEmpty(keyvalues.get("home-town"));
  }

  @Override
  public String getPuhelinnumero() {
    return StringUtils.trimToEmpty(keyvalues.get("phone"));
  }

  public Collection<String> getPuhelinnumerot() {
    return Lists.newArrayList(StringUtils.trimToEmpty(keyvalues.get("phone")));
  }

  @Override
  public String getSahkopostiOsoite() {
    return StringUtils.trimToEmpty(keyvalues.get("email"));
  }

  @Override
  public String getSyntymaaika() {
    return henkilo.getSyntymaaika().format(DateTimeFormatter.ISO_LOCAL_DATE);
  }

  @Override
  public String getSyntymaaikaForErillishaku() {
    return henkilo.getSyntymaaika().format(DateTimeFormatter.ofPattern("dd.MM.yyyy"));
  }

  @Override
  public String getHenkilotunnus() {
    return StringUtils.trimToEmpty(henkilo.getHetu());
  }

  @Override
  public boolean hasHenkilotunnus() {
    return StringUtils.isNotEmpty(getHenkilotunnus());
  }

  @Override
  public String getPersonOid() {
    return StringUtils.trimToEmpty(henkilo.getOidHenkilo());
  }

  @Override
  public String getApplicationPersonOid() {
    return StringUtils.trimToEmpty(hakemus.getPersonOid());
  }

  @Override
  public Integer getHakutoiveenPrioriteetti(String hakukohdeOid) {
    int i =
        hakemus.getHakutoiveet().stream()
            .map(ht -> ht.getHakukohdeOid())
            .collect(Collectors.toList())
            .indexOf(hakukohdeOid);
    return i < 0 ? null : i + 1;
  }

  @Override
  public Boolean getToisenAsteenSuoritus() {
    String value = StringUtils.trimToEmpty(keyvalues.get("secondary-completed-base-education"));
    if (value != null && value.equals("0"))
      return true; // Yes, this is how it goes: 0 = true, 1 = false...
    return false;
  }

  @Override
  public String getToisenAsteenSuoritusmaa() {
    return StringUtils.trimToEmpty(keyvalues.get("secondary-completed-base-education–country"));
  }

  @Override
  public String getEtunimi() {
    return StringUtils.trimToEmpty(henkilo.getEtunimet()).split("\\s+")[0];
  }

  @Override
  public String getKutsumanimi() {
    return StringUtils.trimToEmpty(keyvalues.get("preferred-name"));
  }

  @Override
  public String getEtunimet() {
    return StringUtils.trimToEmpty(henkilo.getEtunimet());
  }

  @Override
  public String getSukunimi() {
    return StringUtils.trimToEmpty(henkilo.getSukunimi());
  }

  @Override
  public boolean getLupaJulkaisuun() {
    if (keyvalues.containsKey("valintatuloksen-julkaisulupa")) {
      if (keyvalues.get("valintatuloksen-julkaisulupa").equals("Kyllä")) {
        return true;
      }
    }
    return false;
  }

  @Override
  public boolean getVainSahkoinenViestinta() {
    return false;
  }

  @Override
  public boolean getLupaTulosEmail() {
    return "Kyllä"
            .equals(
                StringUtils.trimToEmpty(keyvalues.get("paatos-opiskelijavalinnasta-sahkopostiin")))
        || "Kyllä".equals(StringUtils.trimToEmpty(keyvalues.get("sahkoisen-asioinnin-lupa")));
  }

  @Override
  public boolean hasAsiointikieli() {
    try {
      this.getAsiointikieli();
      return true;
    } catch (Exception e) {
      return false;
    }
  }

  @Override
  public String getAsiointikieli() {
    return hakemus.getAsiointikieli();
  }

  @Override
  public boolean getLupaSahkoiseenAsiointiin() {
    if (keyvalues.containsKey("sahkoisen-asioinnin-lupa")) {
      if (keyvalues.get("sahkoisen-asioinnin-lupa").equals("Kyllä")) {
        return true;
      }
    }
    return false;
  }

  @Override
  public Collection<String> getHakutoiveOids() {
    return hakemus.getHakutoiveet().stream()
        .map(AtaruHakutoive::getHakukohdeOid)
        .collect(Collectors.toSet());
  }

  @Override
  public List<AtaruHakutoive> ataruHakutoiveet() {
    return hakemus.getHakutoiveet();
  }

  @Override
  public List<String> getHuoltajienSahkopostiosoitteet() {
    List<String> result = new ArrayList<>();
    if (keyvalues.containsKey("guardian-email_0")
        && !Strings.isNullOrEmpty(keyvalues.get("guardian-email_0"))) {
      result.add(keyvalues.get("guardian-email_0"));
    }
    if (keyvalues.containsKey("guardian-email-secondary_0")
        && !Strings.isNullOrEmpty(keyvalues.get("guardian-email-secondary_0"))) {
      result.add(keyvalues.get("guardian-email-secondary_0"));
    }
    return result;
  }

  @Override
  public boolean isMaksuvelvollinen(String hakukohdeOid) {
    if (hakemus.getMaksuvelvollisuus().containsKey(hakukohdeOid)) {
      if (hakemus.getMaksuvelvollisuus().get(hakukohdeOid).equals("obligated")) {
        return true;
      }
    }
    return false;
  }

  @Override
  public String getMaksuvelvollisuus(String hakukohdeOid) {
    return null;
  }

  @Override
  public boolean ulkomaillaSuoritettuKoulutusTaiOppivelvollisuudenKeskeyttanyt() {
    return false;
  }

  @Override
  public String getHakuoid() {
    return hakemus.getHakuOid();
  }

  @Override
  public String getState() {
    return null;
  }

  public String getSyntymapaikka() {
    return StringUtils.trimToEmpty(keyvalues.get("birthplace"));
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
    } else if (obj instanceof AtaruHakemusWrapper) {
      return this.getOid().equals(((AtaruHakemusWrapper) obj).getOid());
    } else {
      return false;
    }
  }

  @Override
  public HakemusDTO toHakemusDto(
      Valintapisteet valintapisteet,
      Map<String, List<String>> hakukohdeRyhmasForHakukohdes,
      boolean shouldUseApplicationPersonOid) {
    HakemusDTO hakemusDto = new HakemusDTO();
    hakemusDto.setHakemusoid(getOid());
    hakemusDto.setHakijaOid(
        shouldUseApplicationPersonOid ? getApplicationPersonOid() : getPersonOid());
    hakemusDto.setHakuoid(getHakuoid());

    if (hakemus.getKeyValues() != null) {
      hakemus
          .getKeyValues()
          .forEach(
              (key, value) -> {
                if (!"language"
                    .equals(key)) { // FIXME Hakemuspalvelun ei tulisi palauttaa ONR dataa
                  AvainArvoDTO aa = new AvainArvoDTO();
                  aa.setAvain(key);
                  aa.setArvo(
                      ATARU_POHJAKOULUTUS_VUOSI.equals(key) && StringUtils.isEmpty(value)
                          ? resolvePerusopetuksenSuoritusvuosi(hakemus)
                          : value);
                  hakemusDto.getAvaimet().add(aa);
                }
              });
      if (hakemusDto.getAvaimet().stream()
          .noneMatch(aa -> ATARU_POHJAKOULUTUS_VUOSI.equals(aa.getAvain()))) {
        AvainArvoDTO aa =
            new AvainArvoDTO(
                ATARU_POHJAKOULUTUS_VUOSI, resolvePerusopetuksenSuoritusvuosi(hakemus));
        hakemusDto.getAvaimet().add(aa);
      }
    }

    hakemusDto.getAvaimet().add(new AvainArvoDTO("language", getAidinkieli()));

    IntStream.range(0, hakemus.getHakutoiveet().size())
        .forEach(
            i -> {
              HakukohdeDTO hk = new HakukohdeDTO();
              final AtaruHakutoive ataruHakutoive = hakemus.getHakutoiveet().get(i);
              String oid = ataruHakutoive.getHakukohdeOid();
              hk.setOid(oid);
              hk.setHakuoid(hakemus.getHakuOid());
              hk.setPrioriteetti(i + 1);
              hk.setHakukohdeRyhmatOids(hakukohdeRyhmasForHakukohdes.get(oid));
              hk.setHarkinnanvaraisuus(false);
              hakemusDto.getHakukohteet().add(hk);

              final String eligibilityState = ataruHakutoive.getEligibilityState();
              if (!ELIGIBILITIES.containsKey(eligibilityState)) {
                throw new IllegalArgumentException(
                    String.format(
                        "Could not parse hakemus preference value: %s", eligibilityState));
              }

              addAvainArvo(hakemusDto, "preference" + hk.getPrioriteetti() + "-Koulutus-id", oid);
              addAvainArvo(
                  hakemusDto,
                  "preference" + hk.getPrioriteetti() + "-Koulutus-id-eligibility",
                  ELIGIBILITIES.get(eligibilityState));
              addAvainArvo(
                  hakemusDto,
                  "preference" + hk.getPrioriteetti() + "-Koulutus-id-processingState",
                  upperCase(ataruHakutoive.getProcessingState()));
              addAvainArvo(
                  hakemusDto,
                  "preference" + hk.getPrioriteetti() + "-Koulutus-id-paymentObligation",
                  upperCase(ataruHakutoive.getPaymentObligation()));
              addAvainArvo(
                  hakemusDto,
                  "preference" + hk.getPrioriteetti() + "-Koulutus-id-languageRequirement",
                  upperCase(ataruHakutoive.getLanguageRequirement()));
              addAvainArvo(
                  hakemusDto,
                  "preference" + hk.getPrioriteetti() + "-Koulutus-id-degreeRequirement",
                  upperCase(ataruHakutoive.getDegreeRequirement()));
            });

    setHakemusDTOvalintapisteet(valintapisteet, hakemusDto);

    return hakemusDto;
  }

  public String resolvePerusopetuksenSuoritusvuosi(AtaruHakemus hakemus) {
    Optional<String> perusopetuksenSuoritusvuosiOpt =
        Stream.of(
                "b5a683d9-21aa-419f-a6d9-a65c42ff1b29",
                "ebb7fd12-e762-40e3-ad40-a1f9136728d5",
                "bc159ab3-2f23-41ca-8b05-4b8573d408e7",
                "42725ecd-95c4-4ec8-bdd0-a7ad881ee5f1")
            .map(k -> hakemus.getKeyValues().get(k))
            .filter(Objects::nonNull)
            .findFirst();
    return perusopetuksenSuoritusvuosiOpt.orElse(null);
  }

  private static String upperCase(String str) {
    return str != null ? str.toUpperCase() : null;
  }

  private static void addAvainArvo(HakemusDTO hakemusDto, String avain, String arvo) {
    hakemusDto.getAvaimet().add(new AvainArvoDTO(avain, arvo));
  }

  private String getIfSuomalainenOsoiteOrEmpty(String key) {
    if (keyvalues.get("home-town") == null) return "";
    return StringUtils.trimToEmpty(keyvalues.get(key));
  }

  private String getIfUlkomainenOsoiteOrEmpty(String key) {
    if (keyvalues.get("home-town") != null) return "";
    return StringUtils.trimToEmpty(keyvalues.get(key));
  }
}
