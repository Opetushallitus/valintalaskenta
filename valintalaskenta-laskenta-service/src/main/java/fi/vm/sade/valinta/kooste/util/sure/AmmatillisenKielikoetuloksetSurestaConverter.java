package fi.vm.sade.valinta.kooste.util.sure;

import static fi.vm.sade.valinta.kooste.external.resource.suoritusrekisteri.dto.SuoritusJaArvosanatWrapper.wrap;
import static fi.vm.sade.valinta.kooste.util.sure.AmmatillisenKielikoetuloksetSurestaConverter.SureHyvaksyttyArvosana.*;

import fi.vm.sade.service.valintaperusteet.dto.model.Osallistuminen;
import fi.vm.sade.valinta.kooste.external.resource.ohjausparametrit.dto.ParametritDTO;
import fi.vm.sade.valinta.kooste.external.resource.suoritusrekisteri.dto.Arvio;
import fi.vm.sade.valinta.kooste.external.resource.suoritusrekisteri.dto.Arvosana;
import fi.vm.sade.valinta.kooste.external.resource.suoritusrekisteri.dto.SuoritusJaArvosanat;
import fi.vm.sade.valinta.kooste.util.OppijaToAvainArvoDTOConverter;
import fi.vm.sade.valintalaskenta.domain.dto.AvainArvoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class AmmatillisenKielikoetuloksetSurestaConverter {
  public static final String SURE_ASTEIKKO_HYVAKSYTTY = "HYVAKSYTTY";

  public static List<AvainArvoDTO> convert(
      List<SuoritusJaArvosanat> oppijanSuorituksetJaArvosanat,
      ParametritDTO parametritDTO,
      HakemusDTO hakemusDTO) {
    if (oppijanSuorituksetJaArvosanat == null) {
      return Collections.emptyList();
    }

    Stream<SuoritusJaArvosanat> suorituksetEnnenLaskennanAlkamistaMyonnettyjenArvosanojenKanssa =
        OppijaToAvainArvoDTOConverter.removeLaskennanAlkamisenJalkeenMyonnetytArvosanat(
            oppijanSuorituksetJaArvosanat.stream(), parametritDTO);
    List<SuoritusJaArvosanat> kielikoesuoritukset =
        suorituksetEnnenLaskennanAlkamistaMyonnettyjenArvosanojenKanssa
            .filter(Objects::nonNull)
            .filter(s -> s.getSuoritus() != null)
            .filter(s -> wrap(s).isAmmatillisenKielikoe())
            .filter(s -> s.getSuoritus().isVahvistettu())
            .filter(s -> s.getArvosanat() != null)
            .filter(s -> !s.getArvosanat().isEmpty())
            .collect(Collectors.toList());
    return suorituksetKielikohtaisiksiAvainArvoiksi(
        kielikoesuoritukset, hakemusDTO.getHakemusoid());
  }

  private static List<AvainArvoDTO> suorituksetKielikohtaisiksiAvainArvoiksi(
      List<SuoritusJaArvosanat> oppijanKielikoesuoritukset, String hakemusoid) {
    List<AvainArvoDTO> relevanttiArvoKullekinKielelle = new LinkedList<>();
    Stream<String> kaikkiKielet =
        oppijanKielikoesuoritukset.stream()
            .map(SuoritusJaArvosanat::getArvosanat)
            .flatMap(arvosanat -> arvosanat.stream().map(Arvosana::getLisatieto))
            .distinct();
    kaikkiKielet.forEach(
        kieli -> {
          List<SuoritusJaArvosanat> kaikkiKielenSuoritukset =
              oppijanKielikoesuoritukset.stream()
                  .filter(
                      sja ->
                          sja.getArvosanat().stream()
                              .anyMatch(arvosana -> kieli.equals(arvosana.getLisatieto())))
                  .collect(Collectors.toList());

          List<SuoritusJaArvosanat> kielenTamanHakemuksenSuoritukset =
              kaikkiKielenSuoritukset.stream()
                  .filter(sja -> hakemusoid.equals(sja.getSuoritus().getMyontaja()))
                  .collect(Collectors.toList());

          boolean hyvaksyttyTallaHakemuksella =
              containsSuoritusWithValue(kielenTamanHakemuksenSuoritukset, kieli, hyvaksytty);
          boolean hyvaksyttyMillaVainHakemuksella =
              containsSuoritusWithValue(kaikkiKielenSuoritukset, kieli, hyvaksytty);
          boolean hylattyTallaHakemuksella =
              containsSuoritusWithValue(kielenTamanHakemuksenSuoritukset, kieli, hylatty);
          boolean hylattyMillaVainHakemuksella =
              containsSuoritusWithValue(kaikkiKielenSuoritukset, kieli, hylatty);
          boolean eiOsallistunutTallaHakemuksella =
              containsSuoritusWithValue(kielenTamanHakemuksenSuoritukset, kieli, ei_osallistunut);
          boolean eiOsallistunutMillaVainHakemuksella =
              containsSuoritusWithValue(kaikkiKielenSuoritukset, kieli, ei_osallistunut);

          if (hyvaksyttyTallaHakemuksella) {
            relevanttiArvoKullekinKielelle.addAll(
                createAmmatillisenKielikoeAvainArvoDtoCompatibleWithOldHakuAppData(
                    kieli, hyvaksytty, Osallistuminen.OSALLISTUI));
          } else if (hyvaksyttyMillaVainHakemuksella) {
            relevanttiArvoKullekinKielelle.addAll(
                createAmmatillisenKielikoeAvainArvoDtoCompatibleWithOldHakuAppData(
                    kieli, hyvaksytty, Osallistuminen.MERKITSEMATTA));
          } else if (hylattyTallaHakemuksella) {
            relevanttiArvoKullekinKielelle.addAll(
                createAmmatillisenKielikoeAvainArvoDtoCompatibleWithOldHakuAppData(
                    kieli, hylatty, Osallistuminen.OSALLISTUI));
          } else if (hylattyMillaVainHakemuksella) {
            relevanttiArvoKullekinKielelle.addAll(
                createAmmatillisenKielikoeAvainArvoDtoCompatibleWithOldHakuAppData(
                    kieli, hylatty, Osallistuminen.MERKITSEMATTA));
          } else if (eiOsallistunutTallaHakemuksella) {
            relevanttiArvoKullekinKielelle.addAll(
                createAmmatillisenKielikoeAvainArvoDtoCompatibleWithOldHakuAppData(
                    kieli, ei_osallistunut, Osallistuminen.EI_OSALLISTUNUT));
          } else if (eiOsallistunutMillaVainHakemuksella) {
            relevanttiArvoKullekinKielelle.addAll(
                createAmmatillisenKielikoeAvainArvoDtoCompatibleWithOldHakuAppData(
                    kieli, ei_osallistunut, Osallistuminen.MERKITSEMATTA));
          } else {
            relevanttiArvoKullekinKielelle.addAll(
                createAmmatillisenKielikoeAvainArvoDtoCompatibleWithOldHakuAppData(
                    kieli, null, null));
          }
        });
    return relevanttiArvoKullekinKielelle;
  }

  private static boolean containsSuoritusWithValue(
      List<SuoritusJaArvosanat> oppijanSuoritusJaArvosanaParit,
      String kieli,
      SureHyvaksyttyArvosana expectedValue) {
    return oppijanSuoritusJaArvosanaParit.stream()
        .anyMatch(s -> hasValueInAnyArvosana(s, kieli, expectedValue));
  }

  private static boolean hasValueInAnyArvosana(
      SuoritusJaArvosanat suoritusJaArvosanat, String kieli, SureHyvaksyttyArvosana expectedValue) {
    return suoritusJaArvosanat.getArvosanat().stream()
        .anyMatch(
            a -> {
              Arvio arvio = a.getArvio();
              if (!SURE_ASTEIKKO_HYVAKSYTTY.equals(arvio.getAsteikko())) {
                throw new IllegalArgumentException(
                    String.format(
                        "Suorituksen %s arvosanan %s asteikko on '%s'",
                        suoritusJaArvosanat.getSuoritus(), a, arvio.getAsteikko()));
              }
              return kieli.equals(a.getLisatieto())
                  && expectedValue.name().equals(arvio.getArvosana());
            });
  }

  private static List<AvainArvoDTO>
      createAmmatillisenKielikoeAvainArvoDtoCompatibleWithOldHakuAppData(
          String kieli,
          SureHyvaksyttyArvosana valueForAvainArvoDto,
          Osallistuminen osallistuminen) {
    if (valueForAvainArvoDto == null && osallistuminen == null) {
      return Collections.singletonList(new AvainArvoDTO("kielikoe_" + kieli.toLowerCase(), ""));
    }
    return Arrays.asList(
        new AvainArvoDTO("kielikoe_" + kieli.toLowerCase(), valueForAvainArvoDto.arvoForHakuApp),
        new AvainArvoDTO(
            "kielikoe_" + kieli.toLowerCase() + "-OSALLISTUMINEN", osallistuminen.name()));
  }

  public enum SureHyvaksyttyArvosana {
    hyvaksytty("true"),
    hylatty("false"),
    ei_osallistunut(""),
    tyhja("");

    public final String arvoForHakuApp;

    SureHyvaksyttyArvosana(String arvoForHakuApp) {
      this.arvoForHakuApp = arvoForHakuApp;
    }
  }
}
