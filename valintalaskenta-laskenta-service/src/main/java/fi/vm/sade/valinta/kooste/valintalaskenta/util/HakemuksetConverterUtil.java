package fi.vm.sade.valinta.kooste.valintalaskenta.util;

import static fi.vm.sade.valinta.kooste.external.resource.suoritusrekisteri.dto.SuoritusJaArvosanatWrapper.*;
import static java.util.Collections.emptyList;
import static java.util.Collections.sort;
import static java.util.Optional.*;
import static java.util.stream.Collectors.*;

import com.google.common.collect.Maps;
import fi.vm.sade.valinta.kooste.external.resource.harkinnanvaraisuus.HarkinnanvaraisuusAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.harkinnanvaraisuus.dto.HakemuksenHarkinnanvaraisuus;
import fi.vm.sade.valinta.kooste.external.resource.harkinnanvaraisuus.dto.HakutoiveenHarkinnanvaraisuus;
import fi.vm.sade.valinta.kooste.external.resource.ohjausparametrit.dto.ParametritDTO;
import fi.vm.sade.valinta.kooste.external.resource.suoritusrekisteri.dto.Oppija;
import fi.vm.sade.valinta.kooste.external.resource.suoritusrekisteri.dto.SuoritusJaArvosanat;
import fi.vm.sade.valinta.kooste.external.resource.suoritusrekisteri.dto.SuoritusJaArvosanatWrapper;
import fi.vm.sade.valinta.kooste.external.resource.tarjonta.Haku;
import fi.vm.sade.valinta.kooste.external.resource.valintapiste.dto.Valintapisteet;
import fi.vm.sade.valinta.kooste.util.AtaruArvosanaParser;
import fi.vm.sade.valinta.kooste.util.HakemusWrapper;
import fi.vm.sade.valinta.kooste.util.OppijaToAvainArvoDTOConverter;
import fi.vm.sade.valinta.kooste.util.sure.AmmatillisenKielikoetuloksetSurestaConverter;
import fi.vm.sade.valinta.kooste.util.sure.YoToAvainSuoritustietoDTOConverter;
import fi.vm.sade.valintalaskenta.domain.dto.AvainArvoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.domain.dto.Lisapistekoulutus;
import fi.vm.sade.valintalaskenta.domain.dto.PohjakoulutusToinenAste;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.commons.lang.StringUtils;
import org.joda.time.DateTime;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component("HakemuksetConverterUtil")
public class HakemuksetConverterUtil {
  public static final String PK_PAATTOTODISTUSVUOSI = "PK_PAATTOTODISTUSVUOSI";
  public static final String LK_PAATTOTODISTUSVUOSI = "lukioPaattotodistusVuosi";
  public static final String PERUSOPETUS_KIELI = "perusopetuksen_kieli";
  public static final String ATARU_POHJAKOULUTUS_KIELI = "pohjakoulutus_kieli";
  public static final String ATARU_POHJAKOULUTUS_VUOSI = "pohjakoulutus_vuosi";
  public static final String LUKIO_KIELI = "lukion_kieli";
  public static final String POHJAKOULUTUS = "POHJAKOULUTUS";
  public static final String POHJAKOULUTUS_ATARU = "base-education-2nd";
  public static final String ENSIKERTALAINEN = "ensikertalainen";
  public static final String PREFERENCE_PREFIX = "preference";
  public static final String DISCRETIONARY_POSTFIX = "discretionary";
  public static final String KOHDEJOUKKO_AMMATILLINEN_JA_LUKIO = "haunkohdejoukko_11";

  public static final String TUVA_SUORITUSVUOSI_KEY = "LISAPISTEKOULUTUS_TUVA_SUORITUSVUOSI";

  public static final String VALMA_SUORITUSVUOSI_KEY = "LISAPISTEKOULUTUS_VALMA_SUORITUSVUOSI";

  public static final String LUVA_SUORITUSVUOSI_KEY = "LISAPISTEKOULUTUS_LUVA_SUORITUSVUOSI";

  public static final String KYMPPI_SUORITUSVUOSI_KEY = "LISAPISTEKOULUTUS_KYMPPI_SUORITUSVUOSI";

  private static final Map<String, String> LISAPISTEKOULUTUS_SUORITUSVUOSI_MAP =
      Map.of(
          Lisapistekoulutus.LISAKOULUTUS_KYMPPI.komoOid, KYMPPI_SUORITUSVUOSI_KEY,
          Lisapistekoulutus.LISAKOULUTUS_TUVA.komoOid, TUVA_SUORITUSVUOSI_KEY,
          Lisapistekoulutus.LISAKOULUTUS_VALMA.komoOid, VALMA_SUORITUSVUOSI_KEY,
          Lisapistekoulutus.LISAKOULUTUS_MAAHANMUUTTO_LUKIO.komoOid, LUVA_SUORITUSVUOSI_KEY);

  private static final Logger LOG = LoggerFactory.getLogger(HakemuksetConverterUtil.class);

  private final LocalDateTime abienPohjaKoulutusPaattelyLeikkuriPvm;

  private final LocalDateTime harkinnanvaraisuusPaattelyLeikkuriPvm;

  private final HarkinnanvaraisuusAsyncResource harkinnanvaraisuusAsyncResource;

  @Autowired
  public HakemuksetConverterUtil(
      @Value("${valintalaskentakoostepalvelu.abi.pohjakoulutus.paattely.leikkuripvm:2020-06-01}")
          String abienPohjaKoulutusPaattelyLeikkuriPvm,
      @Value("${valintalaskentakoostepalvelu.harkinnanvaraisuus.paattely.leikkuripvm:2020-06-01}")
          String harkinnanvaraisuusPaattelyLeikkuriPvm,
      HarkinnanvaraisuusAsyncResource harkinnanvaraisuusAsyncResource) {
    this.abienPohjaKoulutusPaattelyLeikkuriPvm =
        LocalDate.parse(abienPohjaKoulutusPaattelyLeikkuriPvm).atStartOfDay();
    this.harkinnanvaraisuusPaattelyLeikkuriPvm =
        LocalDate.parse(harkinnanvaraisuusPaattelyLeikkuriPvm).atStartOfDay();
    this.harkinnanvaraisuusAsyncResource = harkinnanvaraisuusAsyncResource;
  }

  private void tryToMergeKeysOfOppijaAndHakemus(
      Haku haku,
      String hakukohdeOid,
      ParametritDTO parametritDTO,
      Boolean fetchEnsikertalaisuus,
      Map<String, Exception> errors,
      Map<String, Oppija> personOidToOppija,
      Map<String, Boolean> hasHetu,
      HakemusDTO h,
      HakemuksenHarkinnanvaraisuus hakemuksenHarkinnanvaraisuus) {
    try {
      String personOid = h.getHakijaOid();
      if (personOidToOppija.containsKey(personOid)) {
        Oppija oppija = personOidToOppija.get(personOid);
        mergeKeysOfOppijaAndHakemus(
            hasHetu.get(h.getHakemusoid()),
            haku,
            hakukohdeOid,
            parametritDTO,
            errors,
            oppija,
            h,
            fetchEnsikertalaisuus,
            hakemuksenHarkinnanvaraisuus);
      } else {
        LOG.warn(
            String.format(
                "BUG-2034 : Oppijatietoa ei löytynyt oppijanumerolla %s (Hakemus: {}).",
                personOid, h.getHakemusoid()));
      }
    } catch (Exception e) {
      errors.put(h.getHakemusoid(), e);
    }
  }

  public List<HakemusDTO> muodostaHakemuksetDTOfromHakemukset(
      Haku haku,
      String hakukohdeOid,
      Map<String, List<String>> hakukohdeRyhmasForHakukohdes,
      List<HakemusWrapper> hakemukset,
      List<Valintapisteet> valintapisteet,
      List<Oppija> oppijat,
      ParametritDTO parametritDTO,
      Boolean fetchEnsikertalaisuus,
      boolean shouldUseApplicationPersonOid) {
    ensurePersonOids(hakemukset, hakukohdeOid);

    Map<String, HakemuksenHarkinnanvaraisuus> harkinnanvaraisuusByHakemus = new HashMap<>();
    if (haku.isKoutaHaku() && haku.isHakemuspalvelu() && haku.isAmmatillinenJaLukio()) {
      List<HakemuksenHarkinnanvaraisuus> hhs =
          hakemukset.stream()
              .map(
                  hakemus ->
                      new HakemuksenHarkinnanvaraisuus(
                          hakemus.getOid(),
                          hakemus.ataruHakutoiveet().stream()
                              .map(
                                  ht ->
                                      new HakutoiveenHarkinnanvaraisuus(
                                          ht.getHakukohdeOid(), ht.getHarkinnanvaraisuus()))
                              .collect(Collectors.toList())))
              .collect(Collectors.toList());

      for (HakemuksenHarkinnanvaraisuus h : hhs) {
        harkinnanvaraisuusByHakemus.put(h.getHakemusOid(), h);
      }
    }

    List<HakemusDTO> hakemusDtot =
        hakemuksetToHakemusDTOs(
            hakukohdeOid,
            hakemukset,
            ofNullable(valintapisteet).orElse(emptyList()),
            hakukohdeRyhmasForHakukohdes,
            shouldUseApplicationPersonOid);
    Map<String, Boolean> hasHetu =
        hakemukset.stream()
            .collect(toMap(HakemusWrapper::getOid, HakemusWrapper::hasHenkilotunnus));
    Map<String, Exception> errors = Maps.newHashMap();
    return getHakemusDTOS(
        haku,
        hakukohdeOid,
        oppijat,
        parametritDTO,
        fetchEnsikertalaisuus,
        hakemusDtot,
        harkinnanvaraisuusByHakemus,
        hasHetu,
        errors);
  }

  private List<HakemusDTO> getHakemusDTOS(
      Haku haku,
      String hakukohdeOid,
      List<Oppija> oppijat,
      ParametritDTO parametritDTO,
      Boolean fetchEnsikertalaisuus,
      List<HakemusDTO> hakemusDtot,
      Map<String, HakemuksenHarkinnanvaraisuus> harkinnanvaraisuusByHakemus,
      Map<String, Boolean> hasHetu,
      Map<String, Exception> errors) {
    try {
      if (oppijat != null) {
        LOG.info(
            String.format(
                "Got %d oppijat is in getHakemusDTOS for haku %s (\"%s\"), hakukohde %s for %d applications.",
                oppijat.size(), haku.oid, haku.nimi, hakukohdeOid, hakemusDtot.size()));
        Map<String, Oppija> personOidToOppija =
            oppijat.stream().collect(toMap(Oppija::getOppijanumero, Function.identity()));
        hakemusDtot.forEach(
            h ->
                tryToMergeKeysOfOppijaAndHakemus(
                    haku,
                    hakukohdeOid,
                    parametritDTO,
                    fetchEnsikertalaisuus,
                    errors,
                    personOidToOppija,
                    hasHetu,
                    h,
                    harkinnanvaraisuusByHakemus.get(h.getHakemusoid())));
      } else {
        LOG.warn(
            String.format(
                "oppijat is null when calling getHakemusDTOS for haku %s (\"%s\"), hakukohde %s for %d applications.",
                haku.oid, haku.nimi, hakukohdeOid, hakemusDtot.size()));
      }
    } catch (Exception e) {
      LOG.error(
          "SURE arvosanojen konversiossa (hakukohde=" + hakukohdeOid + ") odottamaton virhe", e);
      throw e;
    }
    if (!errors.isEmpty()) {
      errors.forEach(
          (key, value) ->
              LOG.error(
                  String.format(
                      "SURE arvosanojen konversiossa (hakukohde=%s, hakemus=%s) odottamaton virhe",
                      hakukohdeOid, key),
                  value));
      throw new RuntimeException(errors.entrySet().iterator().next().getValue());
    }
    return hakemusDtot;
  }

  private boolean hasPKVuosiBefore2018(List<AvainArvoDTO> vals) {
    return vals.stream()
        .anyMatch(
            aa ->
                PK_PAATTOTODISTUSVUOSI.equals(aa.getAvain())
                    && Integer.parseInt(aa.getArvo()) < 2018);
  }

  private Optional<String> getKieliForKomo(Oppija o, String komo) {
    return o.getSuoritukset().stream()
        .filter(
            s ->
                komo.equals(s.getSuoritus().getKomo())
                    && s.getSuoritus().isVahvistettu()
                    && "VALMIS".equals(s.getSuoritus().getTila()))
        .findFirst()
        .map(sa -> sa.getSuoritus().getSuoritusKieli());
  }

  public void mergeKeysOfOppijaAndHakemus(
      boolean hakijallaOnHenkilotunnus,
      Haku haku,
      String hakukohdeOid,
      ParametritDTO parametritDTO,
      Map<String, Exception> errors,
      Oppija oppija,
      HakemusDTO hakemusDTO,
      Boolean fetchEnsikertalaisuus,
      HakemuksenHarkinnanvaraisuus hakemuksenHarkinnanvaraisuus) {

    hakemusDTO.setAvainMetatiedotDTO(YoToAvainSuoritustietoDTOConverter.convert(oppija));
    Map<String, AvainArvoDTO> hakemuksenArvot =
        toAvainMap(hakemusDTO.getAvaimet(), hakemusDTO.getHakemusoid(), hakukohdeOid, errors);
    List<AvainArvoDTO> convertedSureArvosanas =
        OppijaToAvainArvoDTOConverter.convert(
            oppija.getOppijanumero(), oppija.getSuoritukset(), hakemusDTO, parametritDTO);
    LOG.info(
        "Hakemuksen {} konvertoidut sure-arvosanat: {}",
        hakemusDTO.getHakemusoid(),
        convertedSureArvosanas);
    Map<String, AvainArvoDTO> surenArvosanat =
        toAvainMap(convertedSureArvosanas, hakemusDTO.getHakemusoid(), hakukohdeOid, errors);
    Map<String, AvainArvoDTO> ammatillisenKielikokeetSuresta =
        toAvainMap(
            AmmatillisenKielikoetuloksetSurestaConverter.convert(
                oppija.getSuoritukset(), parametritDTO, hakemusDTO),
            hakemusDTO.getHakemusoid(),
            hakukohdeOid,
            errors);

    Map<String, AvainArvoDTO> merge = Maps.newHashMap();
    merge.putAll(hakemuksenArvot);

    if (haku.isHakemuspalvelu() && haku.isKoutaHaku() && haku.isAmmatillinenJaLukio()) {
      if (hakemuksenHarkinnanvaraisuus == null) {
        LOG.error(
            "Hakemuksen {} harkinnanvaraisuus on null, vaikka tieto pitäisi olla saatavilla!",
            hakemusDTO.getHakemusoid());
      } else {
        boolean hasHarkinnanvarainenMatAi =
            harkinnanvaraisuusAsyncResource.hasYksilollistettyMatAi(
                hakemuksenHarkinnanvaraisuus, oppija);

        Map<String, AvainArvoDTO> harkinnanvaraisuustieto =
            Map.of(
                "yks_mat_ai",
                new AvainArvoDTO("yks_mat_ai", String.valueOf(hasHarkinnanvarainenMatAi)));
        merge.putAll(harkinnanvaraisuustieto);
      }
    }

    Optional<String> yoSuoritusKieli = getKieliForKomo(oppija, YO_KOMO);
    if (yoSuoritusKieli.isPresent()) {
      LOG.info(
          "YOKIELI Löydettiin YO-suorituksen kieli hakemukselle {}: {}",
          hakemusDTO.getHakemusoid(),
          yoSuoritusKieli.get());
      Map<String, AvainArvoDTO> yoKieliTieto =
          Map.of("YO_TUTKINTO_KIELI", new AvainArvoDTO("YO_TUTKINTO_KIELI", yoSuoritusKieli.get()));
      merge.putAll(yoKieliTieto);
    } else {
      LOG.info("YOKIELI Ei löydetty YO-kieltä hakemukselle {}", hakemusDTO.getHakemusoid());
    }

    Optional<String> ammSuoritusKieli = getKieliForKomo(oppija, AM_KOMO);
    if (ammSuoritusKieli.isEmpty()) {
      ammSuoritusKieli = getKieliForKomo(oppija, AM_TUTKINTO_KOMO);
      if (ammSuoritusKieli.isPresent()) {
        LOG.info(
            "AMMKIELI Käytetään hakemukselle {} ammatillisen tutkinnon kieltä",
            hakemusDTO.getHakemusoid());
      } else {
        ammSuoritusKieli = getKieliForKomo(oppija, AM_ERIKOISTUTKINTO_KOMO);
        if (ammSuoritusKieli.isPresent()) {
          LOG.info(
              "AMMKIELI Käytetään hakemukselle {} erikoisammattitutkinnon kieltä",
              hakemusDTO.getHakemusoid());
        }
      }
    }
    if (ammSuoritusKieli.isPresent()) {
      LOG.info(
          "AMMKIELI Löydettiin AMM-suorituksen kieli hakemukselle {}: {}",
          hakemusDTO.getHakemusoid(),
          ammSuoritusKieli.get());
      Map<String, AvainArvoDTO> ammKieliTieto =
          Map.of(
              "AMM_TUTKINTO_KIELI", new AvainArvoDTO("AMM_TUTKINTO_KIELI", ammSuoritusKieli.get()));
      merge.putAll(ammKieliTieto);
    } else {
      LOG.info("AMMKIELI Ei löydetty AMM-kieltä hakemukselle {}", hakemusDTO.getHakemusoid());
    }

    if (fetchEnsikertalaisuus)
      ensikertalaisuus(hakijallaOnHenkilotunnus, haku, hakukohdeOid, oppija, hakemusDTO, merge);
    Map<String, AvainArvoDTO> suoritustenTiedot =
        suoritustenTiedot(haku, hakemusDTO, oppija.getSuoritukset(), LocalDate.now())
            .entrySet()
            .stream()
            .collect(
                Collectors.toMap(
                    Map.Entry::getKey, e -> new AvainArvoDTO(e.getKey(), e.getValue())));

    LOG.info("Hakemuksen {} suoritusten tiedot {}", hakemusDTO.getHakemusoid(), suoritustenTiedot);
    LOG.info("Hakemuksen {} suren arvosanat {}", hakemusDTO.getHakemusoid(), surenArvosanat);
    merge.putAll(suoritustenTiedot);

    List<AvainArvoDTO> suoritusValues = new ArrayList<>(suoritustenTiedot.values());

    List<AvainArvoDTO> arvosanatHakemukselta =
        AtaruArvosanaParser.convertAtaruArvosanas(hakemuksenArvot, hakemusDTO.getHakemusoid());

    if (hasPKVuosiBefore2018(suoritusValues)) {
      Map<String, AvainArvoDTO> ataruArvosanat =
          toAvainMap(arvosanatHakemukselta, hakemusDTO.getHakemusoid(), hakukohdeOid, errors);

      LOG.info(
          "Käytetään hakemuksen arvosanoja hakemukselle {}: {}",
          hakemusDTO.getHakemusoid(),
          ataruArvosanat.values());
      merge.putAll(ataruArvosanat);
      if (!surenArvosanat.isEmpty()) {
        LOG.warn(
            "Käytetään hakemukselle {} atarun arvosanoja {}, vaikka surestakin löytyi: {}. Osa tiedoista saatetaan yliajaa.",
            hakemusDTO.getHakemusoid(),
            ataruArvosanat,
            surenArvosanat);
        Set<String> ataruArvosanaAvaimet =
            ataruArvosanat.values().stream()
                .map(AvainArvoDTO::getAvain)
                .collect(Collectors.toSet());
        Map<String, AvainArvoDTO> surenArvosanatJoitaEiAtarussa =
            surenArvosanat.entrySet().stream()
                .filter(
                    entry ->
                        !ataruArvosanaAvaimet.contains(
                            StringUtils.substringBefore(
                                entry.getValue().getAvain(), "_SUORITETTU")))
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
        if (surenArvosanatJoitaEiAtarussa.keySet().size() < surenArvosanat.keySet().size()) {
          LOG.warn(
              "Filtteröitiin hakemukselta {} osa surearvosanoista pois, koska ne saatiin jo atarusta! Arvosanoja ennen filtteröintiä {}, jälkeen {}",
              hakemusDTO.getHakemusoid(),
              surenArvosanat.keySet().size(),
              surenArvosanatJoitaEiAtarussa.keySet().size());
        }
        merge.putAll(surenArvosanatJoitaEiAtarussa);
      }
    } else {
      if (!arvosanatHakemukselta.isEmpty()) {
        LOG.warn(
            "Ataruhakemuksella on arvosanoja, vaikka pohjakoulutusvuosi on 2018 tai sen jälkeen. Ei käytetä hakemuksen {} arvosanoja {}!",
            hakemusDTO.getHakemusoid(),
            arvosanatHakemukselta);
      }
      merge.putAll(surenArvosanat);
    }

    merge.putAll(ammatillisenKielikokeetSuresta);
    hakemusDTO.setAvaimet(
        merge.entrySet().stream()
            .map(Map.Entry::getValue)
            .filter(val -> val != null && val.getAvain() != null && val.getArvo() != null)
            .collect(Collectors.toList()));
  }

  private void ensikertalaisuus(
      boolean hakijallaOnHenkilotunnus,
      Haku haku,
      String hakukohdeOid,
      Oppija oppija,
      HakemusDTO hakemusDTO,
      Map<String, AvainArvoDTO> merge) {

    // Vain korkeakouluhauille
    if (haku.isKorkeakouluhaku()) {
      Boolean ensikertalainen = oppija.isEnsikertalainen();
      if (oppija.isEnsikertalainen() == null) {
        if (!hakijallaOnHenkilotunnus) {
          ensikertalainen = true;
        } else {
          LOG.error(
              "Hakijalta {} (hakemusOid={}) puuttui ensikertalaisuustieto hakukohteen {} laskennassa.",
              hakemusDTO.getHakijaOid(),
              hakemusDTO.getHakemusoid(),
              hakukohdeOid);
          throw new RuntimeException(
              "Hakijalta "
                  + hakemusDTO.getHakijaOid()
                  + " (hakemusOid="
                  + hakemusDTO.getHakemusoid()
                  + ") puuttui ensikertalaisuustieto hakukohteen "
                  + hakukohdeOid
                  + " laskennassa.");
        }
      }
      merge.put(
          ENSIKERTALAINEN, new AvainArvoDTO(ENSIKERTALAINEN, String.valueOf(ensikertalainen)));
    }
  }

  private List<HakemusDTO> hakemuksetToHakemusDTOs(
      String hakukohdeOid,
      List<HakemusWrapper> hakemukset,
      List<Valintapisteet> valintapisteet,
      Map<String, List<String>> hakukohdeRyhmasForHakukohdes,
      boolean shouldUseApplicationPersonOid) {
    List<HakemusDTO> hakemusDtot;
    Map<String, Valintapisteet> hakemusOIDtoValintapisteet =
        valintapisteet.stream().collect(Collectors.toMap(Valintapisteet::getHakemusOID, v -> v));
    Map<String, Exception> epaonnistuneetKonversiot = Maps.newConcurrentMap();
    hakemusDtot =
        getHakemusDTOS(
            hakukohdeOid,
            hakemukset,
            hakukohdeRyhmasForHakukohdes,
            hakemusOIDtoValintapisteet,
            epaonnistuneetKonversiot,
            shouldUseApplicationPersonOid);
    if (!epaonnistuneetKonversiot.isEmpty()) {
      RuntimeException e =
          new RuntimeException(
              String.format(
                  "Hakemukset to hakemusDTO mappauksessa virhe hakukohteelle %s ja hakemuksille %s. Esimerkiksi %s!",
                  hakukohdeOid,
                  Arrays.toString(epaonnistuneetKonversiot.keySet().toArray()),
                  epaonnistuneetKonversiot.values().iterator().next().getMessage()));
      LOG.error("hakemuksetToHakemusDTOs", e);
      throw e;
    }
    return hakemusDtot;
  }

  private List<HakemusDTO> getHakemusDTOS(
      String hakukohdeOid,
      List<HakemusWrapper> hakemukset,
      Map<String, List<String>> hakukohdeRyhmasForHakukohdes,
      Map<String, Valintapisteet> hakemusOIDtoValintapisteet,
      Map<String, Exception> epaonnistuneetKonversiot,
      boolean shouldUseApplicationPersonOid) {
    List<HakemusDTO> hakemusDtot;
    try {
      hakemusDtot =
          hakemukset.parallelStream()
              .filter(Objects::nonNull)
              .map(
                  h -> {
                    try {
                      return h.toHakemusDto(
                          hakemusOIDtoValintapisteet.get(h.getOid()),
                          hakukohdeRyhmasForHakukohdes, // TODO there maybe come null pisteet here
                          shouldUseApplicationPersonOid);
                    } catch (Exception e) {
                      epaonnistuneetKonversiot.put(h.getOid(), e);
                      return null;
                    }
                  })
              .collect(toList());
    } catch (Exception e) {
      LOG.error(
          String.format(
              "Hakemukset to hakemusDTO mappauksessa virhe hakukohteelle %s ja null hakemukselle.",
              hakukohdeOid),
          e);
      throw e;
    }
    return hakemusDtot;
  }

  private void ensurePersonOids(List<HakemusWrapper> hakemukset, String hakukohdeOid) {
    final List<HakemusWrapper> noPersonOid =
        hakemukset.stream().filter(h -> StringUtils.isBlank(h.getPersonOid())).collect(toList());
    if (!noPersonOid.isEmpty()) {
      String hakemusOids =
          noPersonOid.stream().map(HakemusWrapper::getOid).collect(Collectors.joining(", "));
      RuntimeException e =
          new RuntimeException(
              String.format(
                  "Hakukohteessa %s hakemuksilta %s puuttui personOid! Jalkikasittely ehka tekematta! Tarkista hakemusten tiedot!",
                  hakukohdeOid, hakemusOids));
      LOG.error("ensurePersonOids", e);
      throw e;
    }
  }

  public Map<String, AvainArvoDTO> toAvainMap(
      List<AvainArvoDTO> avaimet,
      String hakemusOid,
      String hakukohdeOid,
      Map<String, Exception> poikkeukset) {
    return ofNullable(avaimet).orElse(emptyList()).stream()
        .filter(Objects::nonNull)
        .filter(a -> StringUtils.isNotBlank(a.getAvain()))
        .collect(groupingBy(a -> a.getAvain(), mapping(a -> a, toList())))
        .entrySet()
        .stream()
        .map(
            a -> {
              if (a.getValue().size() != 1) {
                RuntimeException e =
                    new RuntimeException(
                        String.format(
                            "Duplikaattiavain (avain=%s) hakemuksella tai suoritusrekisterin arvosanassa (hakemusOid=%s) hakukohteessa (hakukohdeOid=%s). Jos kyseessä on osakokeen tunnus niin tarkista ettei samalla suorituksella/arvosanalla ole osakoeduplikaatteja.",
                            a.getKey(), hakemusOid, hakukohdeOid));
                LOG.error("toAvainMap", e);
                poikkeukset.put(hakemusOid, e);
              }
              return a.getValue().iterator().next();
            })
        .collect(toMap(a -> a.getAvain(), a -> a));
  }

  public List<SuoritusJaArvosanat> filterKeskenDeadlinenJalkeenSuoritukset(
      List<SuoritusJaArvosanat> suoritukset, LocalDate dateToCompare) {
    try {
      return suoritukset.stream()
          .map(SuoritusJaArvosanatWrapper::wrap)
          .filter(
              s ->
                  !s.isKesken()
                      || (s.isKesken()
                          && !dateToCompare.isAfter(
                              harkinnanvaraisuusPaattelyLeikkuriPvm.toLocalDate())))
          .map(SuoritusJaArvosanatWrapper::getSuoritusJaArvosanat)
          .collect(toList());
    } catch (Exception e) {
      LOG.error("Error when checking cut date: ", e);
      throw new RuntimeException("Error when checking cut date: ", e);
    }
  }

  public List<SuoritusJaArvosanat> filterUnrelevantSuoritukset(
      Haku haku, HakemusDTO hakemus, List<SuoritusJaArvosanat> suoritukset) {
    return suoritukset.stream()
        .map(SuoritusJaArvosanatWrapper::wrap)
        .filter(
            s ->
                !(s.isSuoritusMistaSyntyyPeruskoulunArvosanoja()
                    && !s.isVahvistettu()
                    && !s.onTaltaHakemukselta(hakemus)))
        .filter(
            s ->
                !(s.isSuoritusMistaSyntyyPeruskoulunArvosanoja()
                    && s.isVahvistettu()
                    && !hakukaudella(haku, s)))
        .filter(s -> !(s.isPerusopetus() && s.isKeskeytynyt() && !hakukaudella(haku, s)))
        .filter(s -> !(s.isLukio() && !s.isVahvistettu() && !s.onTaltaHakemukselta(hakemus)))
        .filter(s -> !(s.isLukio() && s.isKeskeytynyt()))
        .filter(s -> !(s.isYoTutkinto() && (s.isKesken() || s.isKeskeytynyt())))
        .map(SuoritusJaArvosanatWrapper::getSuoritusJaArvosanat)
        .collect(toList());
  }

  public Optional<String> pohjakoulutus(
      Haku haku, HakemusDTO hakemusDTO, List<SuoritusJaArvosanat> sureSuoritukset) {
    Optional<String> hakuAppPk =
        hakemusDTO.getAvaimet().stream()
            .filter(a -> POHJAKOULUTUS.equals(a.getAvain()))
            .map(AvainArvoDTO::getArvo)
            .findFirst();
    Optional<String> ataruPk =
        hakemusDTO.getAvaimet().stream()
            .filter(a -> POHJAKOULUTUS_ATARU.equals(a.getAvain()))
            .map(AvainArvoDTO::getArvo)
            .findFirst();
    Optional<String> ataruPohjakoulutusVuosi =
        hakemusDTO.getAvaimet().stream()
            .filter(a -> ATARU_POHJAKOULUTUS_VUOSI.equals(a.getAvain()))
            .map(AvainArvoDTO::getArvo)
            .filter(Objects::nonNull)
            .findFirst();

    Optional<String> pk = hakuAppPk;
    if (pk.isEmpty() && ataruPk.isPresent()) {
      LOG.info("Ataru-hakemuksen {} pohjakoulutus: {}", hakemusDTO.getHakemusoid(), ataruPk.get());
      pk = ataruPk;
    }
    if (pk.isEmpty()) {
      return empty();
    }

    final String pohjakoulutusHakemukselta = pk.get();

    final List<SuoritusJaArvosanatWrapper> suorituksetRekisterista =
        sureSuoritukset.stream()
            .map(SuoritusJaArvosanatWrapper::wrap)
            .filter(suoritusJaArvosanatWrapper -> !suoritusJaArvosanatWrapper.onHakemukselta())
            .collect(toList());

    if (suorituksetRekisterista.stream()
        .anyMatch(
            s ->
                (s.isLukio() && s.isValmis())
                    || (s.isYoTutkinto() && s.isVahvistettu() && s.isValmis()))) {
      // suressa lukio tai yo-tutkinto
      return of(PohjakoulutusToinenAste.YLIOPPILAS);
    }

    final Predicate<SuoritusJaArvosanatWrapper> vahvistettuKeskeytynytPerusopetus =
        s -> s.isPerusopetus() && s.isVahvistettu() && s.isKeskeytynyt();

    if (suorituksetRekisterista.stream().anyMatch(vahvistettuKeskeytynytPerusopetus)
        && suorituksetRekisterista.stream()
            .filter(SuoritusJaArvosanatWrapper::isPerusopetus)
            .allMatch(vahvistettuKeskeytynytPerusopetus)) {
      // suressa kaikki perusopetukset keskeytyneitä
      return of(PohjakoulutusToinenAste.KESKEYTYNYT);
    }

    if (PohjakoulutusToinenAste.YLIOPPILAS.equals(pohjakoulutusHakemukselta)) {
      if (LocalDateTime.now().isBefore(abienPohjaKoulutusPaattelyLeikkuriPvm)
          || !isHakijaAbiturientti(haku, hakemusDTO)) {
        // hakemuksella yo-tutkinto, mutta ei löytynyt suresta ylempänä,
        // luotetaan silti hakemukseen koska poikkeussääntö
        return of(PohjakoulutusToinenAste.YLIOPPILAS);
      }
      LOG.warn(
          "Hakemuksella {} pohjakoulutus lukio, mutta valmista ja vahvistettua lukiosuoritusta ei löydy suoritusrekisteristä.",
          hakemusDTO.getHakemusoid());
    }

    final Optional<SuoritusJaArvosanatWrapper> perusopetus =
        suorituksetRekisterista.stream()
            .filter(s -> s.isPerusopetus() && s.isVahvistettu() && !s.isKeskeytynyt())
            .findFirst();
    if (perusopetus.isPresent()) {
      // suressa perusopetus
      return of(paattelePerusopetuksenPohjakoulutus(perusopetus.get()));
    }

    // TODO: Onko tämä enää tarpeellista kun hakemuksilta ei tule vahvistamattomia suorituksia?
    final Optional<SuoritusJaArvosanatWrapper> perusopetusVahvistamaton =
        suorituksetRekisterista.stream()
            .filter(s -> s.isPerusopetus() && !s.isVahvistettu())
            .findFirst();
    if (perusopetusVahvistamaton.isPresent()) {
      // suressa vahvistamaton perusopetus
      return of(paattelePerusopetuksenPohjakoulutus(perusopetusVahvistamaton.get()));
    }

    if (PohjakoulutusToinenAste.PERUSKOULU.equals(pohjakoulutusHakemukselta)
        && suorituksetRekisterista.stream()
            .anyMatch(s -> s.isUlkomainenKorvaava() && s.isVahvistettu() && s.isValmis())) {
      // suressa korvaava ulkomaalainen suoritus, käytetään hakemuksella olevaa perusopetusta
      LOG.warn(
          "Hakija {} ilmoittanut peruskoulun, mutta löytyi vahvistettu ulkomainen korvaava suoritus. "
              + "Käytetään hakemuksen pohjakoulutusta {}.",
          hakemusDTO.getHakijaOid(),
          pohjakoulutusHakemukselta);
      return of(pohjakoulutusHakemukselta);
    }

    if (PohjakoulutusToinenAste.ULKOMAINEN_TUTKINTO.equals(pohjakoulutusHakemukselta)
        || suorituksetRekisterista.stream()
            .anyMatch(s -> s.isUlkomainenKorvaava() && s.isVahvistettu() && s.isValmis())) {
      // hakemuksella TAI suressa ulkomainen tutkinto
      return of(PohjakoulutusToinenAste.ULKOMAINEN_TUTKINTO);
    }

    if (ataruPk.isPresent()
        && ataruPohjakoulutusVuosi.isPresent()
        && StringUtils.isNotEmpty(ataruPohjakoulutusVuosi.get())) {
      Integer vuosi = Integer.valueOf(ataruPohjakoulutusVuosi.get());
      String pohjakoulutus = ataruPk.get();
      List<String> pkPohjakoulutukset =
          List.of(
              PohjakoulutusToinenAste.PERUSKOULU,
              PohjakoulutusToinenAste.OSITTAIN_YKSILOLLISTETTY,
              PohjakoulutusToinenAste.ALUEITTAIN_YKSILOLLISTETTY,
              PohjakoulutusToinenAste.YKSILOLLISTETTY);
      if (vuosi <= 2017 && pkPohjakoulutukset.contains(pohjakoulutus)) {
        LOG.info(
            "Hakijalle ei löytynyt pohjakoulutusta suoritusrekisteristä mutta hakemuksella on perusopetus ennen vuotta 2018. Käytetään sitä.");
        return of(ataruPk.get());
      }
    }

    LOG.warn(
        "Hakijan {} pohjakoulutusta ei saatu pääteltyä suoritusrekisterin perusteella, palautetaan keskeytynyt.",
        hakemusDTO.getHakijaOid());
    return of(PohjakoulutusToinenAste.KESKEYTYNYT);
  }

  private boolean isHakijaAbiturientti(Haku haku, HakemusDTO hakemusDTO) {
    if (hakemusDTO.getAvaimet().stream()
        .noneMatch(dto -> LK_PAATTOTODISTUSVUOSI.equals(dto.getAvain()))) {
      throw new RuntimeException(
          String.format(
              "Hakijalta %s (hakemusOid=%s) puuttui päättötodistusvuosi hakukohdeOid laskennassa.",
              hakemusDTO.getHakijaOid(), hakemusDTO.getHakemusoid()));
    }
    return hakemusDTO.getAvaimet().stream()
        .anyMatch(
            dto ->
                LK_PAATTOTODISTUSVUOSI.equals(dto.getAvain())
                    && Integer.toString(haku.hakukausiVuosi).equals(dto.getArvo()));
  }

  private String paattelePerusopetuksenPohjakoulutus(SuoritusJaArvosanatWrapper perusopetus) {
    switch (perusopetus.getSuoritusJaArvosanat().getSuoritus().getYksilollistaminen()) {
      case "Kokonaan":
        return PohjakoulutusToinenAste.YKSILOLLISTETTY;
      case "Osittain":
        return PohjakoulutusToinenAste.OSITTAIN_YKSILOLLISTETTY;
      case "Alueittain":
        return PohjakoulutusToinenAste.ALUEITTAIN_YKSILOLLISTETTY;
      default:
        return PohjakoulutusToinenAste.PERUSKOULU;
    }
  }

  public Map<String, String> suoritustenTiedot(
      Haku haku,
      HakemusDTO hakemus,
      List<SuoritusJaArvosanat> sureSuoritukset,
      LocalDate dateToCompare) {
    final Map<String, Predicate<SuoritusJaArvosanat>> predicates =
        new HashMap<String, Predicate<SuoritusJaArvosanat>>() {
          {
            put("PK", s -> wrap(s).isPerusopetus());
            put("AM", s -> wrap(s).isAmmatillinen());
            put("LK", s -> wrap(s).isLukio());
            put("YO", s -> wrap(s).isYoTutkinto());
          }
        };
    final Map<String, String> tiedot = new HashMap<>();
    List<SuoritusJaArvosanat> sureSuorituksetKeskenOlevatPoistettu =
        filterKeskenDeadlinenJalkeenSuoritukset(sureSuoritukset, dateToCompare);
    final List<SuoritusJaArvosanat> suoritukset =
        filterUnrelevantSuoritukset(haku, hakemus, sureSuorituksetKeskenOlevatPoistettu);
    sort(suoritukset);

    Optional<String> pohjakoulutus =
        pohjakoulutus(
            haku,
            hakemus,
            sureSuorituksetKeskenOlevatPoistettu.stream()
                .map(SuoritusJaArvosanatWrapper::wrap)
                // TODO: Onko tämä enää tarpeellista kun hakemuksilta ei tule vahvistamattomia
                // suorituksia?
                .filter(
                    s ->
                        !(s.onTaltaHakemukselta(hakemus)
                            && !s.isVahvistettu()
                            && !hakukaudella(haku, s)))
                .map(SuoritusJaArvosanatWrapper::getSuoritusJaArvosanat)
                .collect(toList()));
    pohjakoulutus.ifPresent(pk -> tiedot.put(POHJAKOULUTUS, pk));

    pkPaattotodistusvuosi(hakemus, suoritukset)
        .ifPresent(vuosi -> tiedot.put(PK_PAATTOTODISTUSVUOSI, String.valueOf(vuosi)));

    if (pohjakoulutus.isPresent()
        && pohjakoulutus.get().equals(PohjakoulutusToinenAste.YLIOPPILAS)) {
      lukioOpetuskieliAtaru(hakemus, suoritukset)
          .ifPresent(kieli -> tiedot.put(LUKIO_KIELI, kieli));
    }
    lukioOpetuskieli(hakemus, suoritukset).ifPresent(kieli -> tiedot.put(LUKIO_KIELI, kieli));
    pkOpetuskieli(hakemus, suoritukset).ifPresent(kieli -> tiedot.put(PERUSOPETUS_KIELI, kieli));

    pohjakoulutus.ifPresent(pk -> tiedot.putAll(automaticDiscretionaryOptions(pk, haku, hakemus)));
    suoritustilat(predicates, suoritukset)
        .entrySet()
        .forEach(e -> tiedot.put(e.getKey(), String.valueOf(e.getValue())));
    suoritusajat(predicates, suoritukset)
        .entrySet()
        .forEach(e -> tiedot.put(e.getKey(), String.valueOf(e.getValue())));
    pohjakoulutus.ifPresent(
        pk ->
            lisapistekoulutukset(pk, haku, hakemus, suoritukset)
                .entrySet()
                .forEach(e -> tiedot.put(e.getKey(), String.valueOf(e.getValue()))));
    return tiedot;
  }

  private Map<String, String> automaticDiscretionaryOptions(
      String pohjakoulutus, Haku haku, HakemusDTO hakemus) {
    Map<String, String> tiedot = new HashMap<>();
    if (haku.isAmmatillinenJaLukio()
        && (PohjakoulutusToinenAste.KESKEYTYNYT.equals(pohjakoulutus)
            || PohjakoulutusToinenAste.ULKOMAINEN_TUTKINTO.equals(pohjakoulutus))) {
      for (int preferenceIndex = 1;
          preferenceIndex <= hakemus.getHakukohteet().size();
          preferenceIndex++) {
        String discretionaryQuestionId =
            PREFERENCE_PREFIX + preferenceIndex + "-" + DISCRETIONARY_POSTFIX;
        tiedot.put(discretionaryQuestionId, "true");
        tiedot.put(discretionaryQuestionId + "-follow-up", "todistustenpuuttuminen");
      }
    }
    return tiedot;
  }

  private Map<String, Integer> suoritusajat(
      Map<String, Predicate<SuoritusJaArvosanat>> predicates,
      List<SuoritusJaArvosanat> suoritukset) {
    Map<String, Integer> vuodet = new HashMap<>();
    predicates.entrySet().stream()
        .forEach(
            e -> {
              String prefix = e.getKey();
              Predicate<SuoritusJaArvosanat> p = e.getValue();
              suoritukset.stream()
                  .filter(s -> p.test(s) && wrap(s).isValmis())
                  .map(s -> wrap(s).getValmistuminenAsDateTime())
                  .findFirst()
                  .ifPresent(
                      date -> {
                        vuodet.put(prefix + "_SUORITUSVUOSI", date.getYear());
                        vuodet.put(prefix + "_SUORITUSLUKUKAUSI", suorituskausi(date));
                      });
            });
    return vuodet;
  }

  private Integer suorituskausi(DateTime valmistumisPvm) {
    if (valmistumisPvm.isBefore(
        SuoritusJaArvosanatWrapper.VALMISTUMIS_DTF.parseDateTime(
            "01.08." + valmistumisPvm.getYear()))) {
      return 2;
    } else {
      return 1;
    }
  }

  private Map<String, Boolean> suoritustilat(
      Map<String, Predicate<SuoritusJaArvosanat>> predicates,
      List<SuoritusJaArvosanat> suoritukset) {
    return predicates.keySet().stream()
        .collect(
            toMap(
                prefix -> prefix + "_TILA",
                prefix ->
                    suoritukset.stream()
                        .anyMatch(
                            s ->
                                predicates.get(prefix).test(s)
                                    && wrap(s).isValmis()
                                    && (wrap(s).isVahvistettu() || wrap(s).isLukio()))));
  }

  private Map<String, Object> lisapistekoulutukset(
      String pohjakoulutus, Haku haku, HakemusDTO hakemus, List<SuoritusJaArvosanat> suoritukset) {
    if (PohjakoulutusToinenAste.KESKEYTYNYT.equals(pohjakoulutus)
        || PohjakoulutusToinenAste.YLIOPPILAS.equals(pohjakoulutus)
        || PohjakoulutusToinenAste.ULKOMAINEN_TUTKINTO.equals(pohjakoulutus)) {
      return Arrays.stream(Lisapistekoulutus.values()).collect(toMap(Enum::name, lpk -> false));
    }
    Map<String, Object> suoritusvuosiMap = new HashMap<>();
    Map<String, Object> koulutusMap =
        Arrays.stream(Lisapistekoulutus.values())
            .collect(
                toMap(
                    Enum::name,
                    lpk ->
                        suoritukset.stream()
                            .filter(s -> lpk.komoOid.equals(s.getSuoritus().getKomo()))
                            .filter(
                                s ->
                                    !(wrap(s).isKeskeytynyt()
                                        && !lisapistekoulutusHuomioidaan(haku, wrap(s))))
                            .findFirst()
                            .map(
                                s -> {
                                  SuoritusJaArvosanatWrapper wrapper = wrap(s);
                                  if (!wrapper.isKeskeytynyt()
                                      && LISAPISTEKOULUTUS_SUORITUSVUOSI_MAP.containsKey(
                                          s.getSuoritus().getKomo())
                                      && wrapper.getValmistuminenAsDateTime() != null) {
                                    suoritusvuosiMap.put(
                                        LISAPISTEKOULUTUS_SUORITUSVUOSI_MAP.get(
                                            s.getSuoritus().getKomo()),
                                        wrapper.getValmistuminenAsDateTime().getYear());
                                  }
                                  return !wrapper.isKeskeytynyt();
                                })
                            .orElse(
                                hakemus.getAvaimet().stream()
                                    .filter(
                                        a ->
                                            lpk.name().equals(a.getAvain())
                                                && lpk.equals(
                                                    Lisapistekoulutus.LISAKOULUTUS_OPISTOVUOSI))
                                    .map(a -> Boolean.valueOf(a.getArvo()))
                                    .findFirst()
                                    .orElse(false))));
    suoritusvuosiMap.putAll(koulutusMap);
    return suoritusvuosiMap;
  }

  private Optional<Integer> pkPaattotodistusvuosi(
      HakemusDTO hakemus, List<SuoritusJaArvosanat> suoritukset) {
    return Stream.concat(
            suoritukset.stream()
                .filter(s -> wrap(s).isPerusopetus() && (wrap(s).isValmis() || wrap(s).isKesken()))
                .map(s -> wrap(s).getValmistuminenAsDateTime().getYear()),
            hakemus.getAvaimet().stream()
                .filter(
                    a ->
                        PK_PAATTOTODISTUSVUOSI.equals(a.getAvain())
                            || (ATARU_POHJAKOULUTUS_VUOSI.equals(a.getAvain())
                                && a.getArvo() != null))
                .map(a -> Integer.valueOf(a.getArvo())))
        .findFirst();
  }

  private Optional<String> pkOpetuskieli(
      HakemusDTO hakemus, List<SuoritusJaArvosanat> suoritukset) {
    return Stream.concat(
            suoritukset.stream()
                .filter(s -> wrap(s).isPerusopetus() && (wrap(s).isValmis() || wrap(s).isKesken()))
                .map(s -> wrap(s).getSuoritusJaArvosanat().getSuoritus().getSuoritusKieli())
                .filter(s -> !StringUtils.isEmpty(s)),
            hakemus.getAvaimet().stream()
                .filter(
                    a ->
                        PERUSOPETUS_KIELI.equals(a.getAvain())
                            || (ATARU_POHJAKOULUTUS_KIELI.equals(a.getAvain())
                                && a.getArvo() != null))
                .map(a -> a.getArvo()))
        .findFirst();
  }

  private Optional<String> lukioOpetuskieliAtaru(
      HakemusDTO hakemus, List<SuoritusJaArvosanat> suoritukset) {
    return Stream.concat(
            suoritukset.stream()
                .filter(s -> wrap(s).isLukio() && (wrap(s).isValmis() || wrap(s).isKesken()))
                .map(s -> wrap(s).getSuoritusJaArvosanat().getSuoritus().getSuoritusKieli())
                .filter(s -> !StringUtils.isEmpty(s)),
            hakemus.getAvaimet().stream()
                .filter(a -> ATARU_POHJAKOULUTUS_KIELI.equals(a.getAvain()) && a.getArvo() != null)
                .map(a -> a.getArvo()))
        .findFirst();
  }

  private Optional<String> lukioOpetuskieli(
      HakemusDTO hakemus, List<SuoritusJaArvosanat> suoritukset) {
    return Stream.concat(
            suoritukset.stream()
                .filter(s -> wrap(s).isLukio() && (wrap(s).isValmis() || wrap(s).isKesken()))
                .map(s -> wrap(s).getSuoritusJaArvosanat().getSuoritus().getSuoritusKieli())
                .filter(s -> !StringUtils.isEmpty(s)),
            hakemus.getAvaimet().stream()
                .filter(a -> LUKIO_KIELI.equals(a.getAvain()))
                .map(a -> a.getArvo()))
        .findFirst();
  }

  private boolean lisapistekoulutusHuomioidaan(Haku haku, SuoritusJaArvosanatWrapper s) {
    if (haku.isKoutaHaku()) {
      int edellinenVuosi;
      if (haku.hakukausiVuosi != null) {
        edellinenVuosi = haku.hakukausiVuosi - 1;
      } else {
        LOG.warn(
            "Haulla {} ei ole hakukausiVuotta, käytetään nykyistä vuotta henkilöOidille {}",
            haku.oid,
            s.getSuoritusJaArvosanat().getSuoritus().getHenkiloOid());
        edellinenVuosi = new DateTime().getYear() - 1;
      }
      DateTime relevantStart = new DateTime(edellinenVuosi, 1, 1, 0, 0);
      return s.getValmistuminenAsDateTime().isAfter(relevantStart);
    } else {
      return hakukaudella(haku, s);
    }
  }

  private boolean hakukaudella(Haku haku, SuoritusJaArvosanatWrapper s) {
    if (haku.isKoutaHaku()) {
      return true; // TODO: Saako Koutan hakujen kanssa huomioida myös sellaiset suoritukset jotka
      // eivät ole hakukaudella?
    }
    DateTime valmistuminen = s.getValmistuminenAsDateTime();
    int hakuvuosi = haku.hakukausiVuosi;
    DateTime kStart = new DateTime(hakuvuosi, 1, 1, 0, 0).minus(1);
    DateTime kEnd = new DateTime(hakuvuosi, 7, 31, 0, 0).plusDays(1);
    DateTime sStart = new DateTime(hakuvuosi, 8, 1, 0, 0).minus(1);
    DateTime sEnd = new DateTime(hakuvuosi, 12, 31, 0, 0).plusDays(1);
    if (haku.hakukausiUri.startsWith("kausi_k#")) {
      return valmistuminen.isAfter(kStart) && valmistuminen.isBefore(kEnd);
    } else if (haku.hakukausiUri.startsWith("kausi_s#")) {
      return valmistuminen.isAfter(sStart) && valmistuminen.isBefore(sEnd);
    } else {
      throw new RuntimeException(String.format("Tuntematon hakukausi %s", haku.hakukausiUri));
    }
  }
}
