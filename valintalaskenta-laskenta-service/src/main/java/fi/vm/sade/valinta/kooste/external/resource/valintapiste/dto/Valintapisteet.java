package fi.vm.sade.valinta.kooste.external.resource.valintapiste.dto;

import fi.vm.sade.valinta.kooste.external.resource.hakuapp.dto.ApplicationAdditionalDataDTO;
import java.util.*;
import java.util.function.BiFunction;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.commons.lang3.tuple.Pair;

public class Valintapisteet {

  private String hakemusOID;
  private String oppijaOID;
  private String etunimet;
  private String sukunimi;
  private List<Piste> pisteet;

  public Valintapisteet() {}

  /**
   * @param a Tallettaja and AdditionalData
   */
  public Valintapisteet(Pair<String, ApplicationAdditionalDataDTO> a) {
    this(
        a.getRight().getOid(),
        a.getRight().getPersonOid(),
        a.getRight().getFirstNames(),
        a.getRight().getLastName(),
        ADDITIONAL_INFO_TO_PISTEET.apply(a.getRight().getAdditionalData(), a.getKey()));
  }

  public Valintapisteet(
      String hakemusOID, String oppijaOID, String etunimet, String sukunimi, List<Piste> pisteet) {
    this.hakemusOID = hakemusOID;
    this.oppijaOID = oppijaOID;
    this.etunimet = etunimet;
    this.sukunimi = sukunimi;
    this.pisteet = pisteet;
  }

  public List<Piste> getPisteet() {
    return Optional.ofNullable(pisteet).orElse(Collections.emptyList());
  }

  public String getHakemusOID() {
    return hakemusOID;
  }

  public String getEtunimet() {
    return etunimet;
  }

  public String getOppijaOID() {
    return oppijaOID;
  }

  public String getSukunimi() {
    return sukunimi;
  }

  public static ApplicationAdditionalDataDTO toAdditionalData(Valintapisteet v) {
    Map<String, String> immutableAdditionalData =
        v.getPisteet().stream()
            .flatMap(
                p ->
                    Stream.concat(
                        Optional.ofNullable(p.getArvo())
                            .map(a -> Stream.of(Pair.of(p.getTunniste(), a)))
                            .orElse(Stream.empty()),
                        Stream.of(
                            Pair.of(
                                withOsallistuminenSuffix(p.getTunniste()),
                                p.getOsallistuminen().toString()))))
            .collect(Collectors.toMap(Pair::getKey, Pair::getValue));

    return new ApplicationAdditionalDataDTO(
        v.getHakemusOID(),
        v.getOppijaOID(),
        v.getEtunimet(),
        v.getSukunimi(),
        new HashMap<>(immutableAdditionalData));
  }

  public static String withOsallistuminenSuffix(String tunniste) {
    return tunniste + "-OSALLISTUMINEN";
  }

  private static BiFunction<Map<String, String>, String, List<Piste>> ADDITIONAL_INFO_TO_PISTEET =
      (additionalInfo, tallettajaOid) ->
          additionalInfo.entrySet().stream()
              .flatMap(
                  entry -> {
                    String k = entry.getKey();
                    Object v = entry.getValue();
                    if (k.endsWith("-OSALLISTUMINEN")) {
                      String tunniste = k.replaceAll("-OSALLISTUMINEN", "");
                      Osallistuminen osallistuminen = Osallistuminen.valueOf(v.toString());
                      String arvo = additionalInfo.get(tunniste);
                      return Stream.of(new Piste(tunniste, arvo, osallistuminen, tallettajaOid));
                    } else {
                      return Stream.empty();
                    }
                  })
              .collect(Collectors.toList());
}
