package fi.vm.sade.valinta.kooste.valintalaskenta.service;

import static fi.vm.sade.service.valintaperusteet.dto.model.Funktionimi.ITEROIAMMATILLISETTUTKINNOT;
import static fi.vm.sade.service.valintaperusteet.dto.model.Funktionimi.ITEROIAMMATILLISETTUTKINNOT_LEIKKURIPVM_PARAMETRI;
import static java.io.File.separator;
import static java.nio.charset.StandardCharsets.UTF_8;

import com.google.common.collect.Lists;
import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import fi.vm.sade.service.valintaperusteet.dto.SyoteparametriDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetFunktiokutsuDTO;
import fi.vm.sade.valinta.kooste.external.resource.koski.KoskiAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.koski.KoskiOppija;
import fi.vm.sade.valinta.kooste.external.resource.koski.KoskiOppija.OpiskeluoikeusJsonUtil;
import fi.vm.sade.valinta.kooste.util.CompletableFutureUtil;
import java.io.File;
import java.io.IOException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class KoskiOpiskeluoikeusHistoryService {
  private static final Logger LOG =
      LoggerFactory.getLogger(KoskiOpiskeluoikeusHistoryService.class);

  private static final DateTimeFormatter FINNISH_DATE_FORMAT =
      DateTimeFormatter.ofPattern("d.M.yyyy");
  private final int koskiPyyntojenRinnakkaisuus;
  private final KoskiAsyncResource koskiAsyncResource;
  private final Map<String, JsonElement> ohittavatOpiskeluoikeudetOideittain;
  private static final Gson GSON = new Gson();

  public KoskiOpiskeluoikeusHistoryService(
      int koskiPyyntojenRinnakkaisuus, KoskiAsyncResource koskiAsyncResource) {
    this.koskiPyyntojenRinnakkaisuus = koskiPyyntojenRinnakkaisuus;
    this.koskiAsyncResource = koskiAsyncResource;
    this.ohittavatOpiskeluoikeudetOideittain = lueOhitettavatOpiskeluoikeudetKonfiguraatiosta();
    LOG.info(
        String.format(
            "Saatiin %d ohitettavaa opiskeluoikeutta: %s",
            ohittavatOpiskeluoikeudetOideittain.size(),
            ohittavatOpiskeluoikeudetOideittain.keySet()));
  }

  LocalDate etsiKoskiDatanLeikkuriPvm(
      List<ValintaperusteetDTO> valintaperusteetDTOS, String hakukohdeOid) {
    List<String> leikkuriPvmMerkkijonot =
        etsiTutkintojenIterointiFunktioKutsut(valintaperusteetDTOS)
            .flatMap(
                tutkintojenIterointiFunktio ->
                    tutkintojenIterointiFunktio.getSyoteparametrit().stream()
                        .filter(
                            parametri ->
                                ITEROIAMMATILLISETTUTKINNOT_LEIKKURIPVM_PARAMETRI.equals(
                                        parametri.getAvain())
                                    && StringUtils.isNotBlank(parametri.getArvo()))
                        .map(SyoteparametriDTO::getArvo))
            .collect(Collectors.toList());
    LocalDate kaytettavaLeikkuriPvm =
        leikkuriPvmMerkkijonot.stream()
            .map(pvm -> LocalDate.parse(pvm, FINNISH_DATE_FORMAT))
            .max(Comparator.naturalOrder())
            .orElse(LocalDate.now());
    LOG.info(
        String.format(
            "Saatiin hakukohteen %s valintaperusteista Koski-datan leikkuripäivämäärät %s. Käytetään leikkuripäivämääränä arvoa %s.",
            hakukohdeOid,
            leikkuriPvmMerkkijonot,
            FINNISH_DATE_FORMAT.format(kaytettavaLeikkuriPvm)));
    return kaytettavaLeikkuriPvm;
  }

  static Stream<ValintaperusteetFunktiokutsuDTO> etsiTutkintojenIterointiFunktioKutsut(
      List<ValintaperusteetDTO> valintaperusteetDTOS) {
    return valintaperusteetDTOS.stream()
        .flatMap(
            valintaperusteetDTO ->
                valintaperusteetDTO.getValinnanVaihe().getValintatapajono().stream())
        .flatMap(jono -> jono.getJarjestyskriteerit().stream())
        .flatMap(
            kriteeri ->
                etsiFunktiokutsutRekursiivisesti(
                    kriteeri.getFunktiokutsu(),
                    fk -> ITEROIAMMATILLISETTUTKINNOT.equals(fk.getFunktionimi()))
                    .stream());
  }

  private static List<ValintaperusteetFunktiokutsuDTO> etsiFunktiokutsutRekursiivisesti(
      ValintaperusteetFunktiokutsuDTO juuriFunktioKutsu,
      Predicate<ValintaperusteetFunktiokutsuDTO> predikaatti) {
    List<ValintaperusteetFunktiokutsuDTO> tulokset = new LinkedList<>();
    if (predikaatti.test(juuriFunktioKutsu)) {
      tulokset.add(juuriFunktioKutsu);
    }
    tulokset.addAll(
        juuriFunktioKutsu.getFunktioargumentit().stream()
            .flatMap(
                argumentti ->
                    etsiFunktiokutsutRekursiivisesti(argumentti.getFunktiokutsu(), predikaatti)
                        .stream())
            .collect(Collectors.toSet()));
    return tulokset;
  }

  void haeVanhemmatOpiskeluoikeudetTarvittaessa(
      Set<KoskiOppija> koskioppijat, LocalDate leikkuriPvm) {
    List<KoskiOppija> oppijatJoillaOnLiianUusiaOpiskeluoikeuksia =
        koskioppijat.stream()
            .filter(
                o ->
                    Lists.newArrayList(o.getOpiskeluoikeudet()).stream()
                        .anyMatch(
                            opiskeluoikeus -> {
                              return OpiskeluoikeusJsonUtil.onUudempiKuin(
                                  leikkuriPvm, opiskeluoikeus);
                            }))
            .collect(Collectors.toList());
    if (!oppijatJoillaOnLiianUusiaOpiskeluoikeuksia.isEmpty()) {
      LOG.info(
          String.format(
              "%d oppijasta %d:lla on opiskeluoikeuksia, joita on päivitetty leikkuripäivämäärän %s jälkeen. "
                  + "Haetaan näille riittävän vanhat versiot, %d opiskeluoikeutta rinnakkain.",
              koskioppijat.size(),
              oppijatJoillaOnLiianUusiaOpiskeluoikeuksia.size(),
              leikkuriPvm,
              koskiPyyntojenRinnakkaisuus));
      Lists.partition(oppijatJoillaOnLiianUusiaOpiskeluoikeuksia, koskiPyyntojenRinnakkaisuus)
          .forEach(
              yhdenEränOppijat ->
                  haeLiianUusienOpiskeluoikeuksienTilalleRiittävänVanhat(
                      leikkuriPvm, yhdenEränOppijat));
    }
  }

  private void haeLiianUusienOpiskeluoikeuksienTilalleRiittävänVanhat(
      LocalDate leikkuriPvm, List<KoskiOppija> yhdenEränOppijat) {
    CompletableFutureUtil.sequence(
            yhdenEränOppijat.stream()
                .map(
                    koskiOppija ->
                        haeOpiskeluoikeuksistaPaivanMukainenVersio(koskiOppija, leikkuriPvm)
                            .thenApplyAsync(
                                päivämäärärajanTäyttävätOpiskeluoikeudet -> {
                                  koskiOppija.setOpiskeluoikeudet(
                                      päivämäärärajanTäyttävätOpiskeluoikeudet);
                                  return "OK";
                                }))
                .collect(Collectors.toList()))
        .join();
  }

  private CompletableFuture<JsonArray> haeOpiskeluoikeuksistaPaivanMukainenVersio(
      KoskiOppija koskiOppija, LocalDate leikkuriPvm) {
    return CompletableFutureUtil.sequence(
            Lists.newArrayList(koskiOppija.getOpiskeluoikeudet()).stream()
                .map(
                    opiskeluoikeus -> {
                      if (OpiskeluoikeusJsonUtil.onUudempiKuin(leikkuriPvm, opiskeluoikeus)) {
                        String opiskeluoikeudenOid = OpiskeluoikeusJsonUtil.oid(opiskeluoikeus);
                        int versionumero = OpiskeluoikeusJsonUtil.versionumero(opiskeluoikeus);
                        LocalDateTime aikaleima = OpiskeluoikeusJsonUtil.aikaleima(opiskeluoikeus);
                        LOG.info(
                            String.format(
                                "Koskesta haetun oppijan %s opiskeluoikeuden %s uusimman version %d aikaleima on %s eli leikkuripäivämäärän %s jälkeen."
                                    + " Etsitään opiskeluoikeudesta versioita, jotka olisi tallennettu ennen leikkuripäivämäärää.",
                                koskiOppija.getOppijanumero(),
                                opiskeluoikeudenOid,
                                versionumero,
                                aikaleima,
                                FINNISH_DATE_FORMAT.format(leikkuriPvm)));
                        return haePaivamaaranMukainenVersio(
                            koskiOppija,
                            leikkuriPvm,
                            CompletableFuture.completedFuture(opiskeluoikeus));
                      } else {
                        return CompletableFuture.completedFuture(Optional.of(opiskeluoikeus));
                      }
                    })
                .collect(Collectors.toList()))
        .thenApplyAsync(
            opiskeluoikeusOptiot -> {
              JsonArray result = new JsonArray();
              opiskeluoikeusOptiot.forEach(o -> o.ifPresent(result::add));
              return result;
            });
  }

  private CompletableFuture<Optional<JsonElement>> haePaivamaaranMukainenVersio(
      KoskiOppija koskiOppija,
      LocalDate leikkuriPvm,
      CompletableFuture<JsonElement> opiskeluoikeusF) {
    return opiskeluoikeusF.thenComposeAsync(
        opiskeluoikeus -> {
          int versionumero = OpiskeluoikeusJsonUtil.versionumero(opiskeluoikeus);
          LocalDateTime aikaleima = OpiskeluoikeusJsonUtil.aikaleima(opiskeluoikeus);
          String opiskeluoikeudenOid = OpiskeluoikeusJsonUtil.oid(opiskeluoikeus);

          if (ohittavatOpiskeluoikeudetOideittain.containsKey(opiskeluoikeudenOid)) {
            LOG.warn(
                String.format(
                    "Ohitetaan oppijan %s opiskeluoikeus %s konfiguraatiosta tulevalla arvolla.",
                    koskiOppija.getOppijanumero(), opiskeluoikeudenOid));
            return CompletableFuture.completedFuture(
                Optional.of(ohittavatOpiskeluoikeudetOideittain.get(opiskeluoikeudenOid)));
          }

          if (!OpiskeluoikeusJsonUtil.onUudempiKuin(leikkuriPvm, aikaleima)) {
            LOG.info(
                String.format(
                    "Koskesta haetun oppijan %s opiskeluoikeuden %s version %d aikaleima on %s eli leikkuripäivämäärään %s mennessä, joten huomioidaan tämä opiskeluoikeus.",
                    koskiOppija.getOppijanumero(),
                    opiskeluoikeudenOid,
                    versionumero,
                    aikaleima,
                    FINNISH_DATE_FORMAT.format(leikkuriPvm)));
            return CompletableFuture.completedFuture(Optional.of(opiskeluoikeus));
          }
          if (versionumero <= 1) {
            LOG.info(
                String.format(
                    "Koskesta haetun oppijan %s opiskeluoikeuden %s aikaleima on %s eli leikkuripäivämäärän %s jälkeen,"
                        + " ja versio %d, joten vanhempia versioita ei ole. Jätetään opiskeluoikeus huomioimatta.",
                    koskiOppija.getOppijanumero(),
                    opiskeluoikeudenOid,
                    aikaleima,
                    FINNISH_DATE_FORMAT.format(leikkuriPvm),
                    versionumero));
            return CompletableFuture.completedFuture(Optional.empty());
          }
          CompletableFuture<JsonElement> edellisenVersionOpiskeluoikeus =
              koskiAsyncResource.findVersionOfOpiskeluoikeus(opiskeluoikeudenOid, versionumero - 1);
          return haePaivamaaranMukainenVersio(
              koskiOppija, leikkuriPvm, edellisenVersionOpiskeluoikeus);
        });
  }

  private static Map<String, JsonElement> lueOhitettavatOpiskeluoikeudetKonfiguraatiosta() {
    try {
      String kotihakemisto = System.getProperty("user.home");
      File ohitusTiedostojenHakemisto =
          new File(
              String.format(
                  "%s%soph-configuration%svalintalaskentakoostepalvelu%skoski-ohitukset",
                  kotihakemisto, separator, separator, separator));
      if (!ohitusTiedostojenHakemisto.exists()) {
        LOG.info(
            String.format(
                "Ei löydy ohitettavien opiskeluoikeuksien hakemistoa polusta '%s', "
                    + "joten käytetään Koski-data sellaisena kuin se tulee.",
                ohitusTiedostojenHakemisto.getAbsolutePath()));
        return Collections.emptyMap();
      }
      final File[] ohitustiedostot = ohitusTiedostojenHakemisto.listFiles();
      if (ohitustiedostot == null || ohitustiedostot.length == 0) {
        LOG.info(
            String.format(
                "Ohitettavien opiskeluoikeuksien hakemistoa polussa '%s' on tyhjä, "
                    + "joten käytetään Koski-data sellaisena kuin se tulee.",
                ohitusTiedostojenHakemisto.getAbsolutePath()));
        return Collections.emptyMap();
      }

      return Arrays.stream(ohitustiedostot)
          .collect(
              Collectors.toMap(
                  t -> t.getName().replace(".json", ""),
                  t -> {
                    LOG.info(String.format("Luetaan tiedosto '%s'", t.getAbsolutePath()));
                    return lueJsoniksi(t);
                  }));
    } catch (Exception e) {
      LOG.error(
          "Ongelma luettaessa JSON-ohitustiedostoja. Käytetään Koski-data sellaisena kuin se tulee");
      return Collections.emptyMap();
    }
  }

  private static JsonElement lueJsoniksi(File tiedosto) {
    try {
      return GSON.fromJson(FileUtils.readFileToString(tiedosto, UTF_8), JsonElement.class);
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }
}
