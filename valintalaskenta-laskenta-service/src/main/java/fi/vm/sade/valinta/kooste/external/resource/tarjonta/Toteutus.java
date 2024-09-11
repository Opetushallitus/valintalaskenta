package fi.vm.sade.valinta.kooste.external.resource.tarjonta;

import fi.vm.sade.tarjonta.service.resources.v1.dto.koulutus.KoulutusV1RDTO;
import fi.vm.sade.valinta.kooste.external.resource.tarjonta.dto.KoutaToteutus;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Toteutus {
  public final String oid;
  public final String koulutusOid;
  public final String alkamiskausiUri;
  public final Integer alkamisvuosi;
  public final Set<String> opetuskielet;
  public final Set<String> osaamisalaUris;

  public Toteutus(
      String oid,
      String koulutusOid,
      String alkamiskausiUri,
      Integer alkamisvuosi,
      Set<String> opetuskielet,
      Set<String> osaamisalaUris) {
    this.oid = oid;
    this.koulutusOid = koulutusOid;
    this.alkamiskausiUri = alkamiskausiUri;
    this.alkamisvuosi = alkamisvuosi;
    this.opetuskielet = opetuskielet;
    this.osaamisalaUris = osaamisalaUris;
  }

  public Toteutus(KoulutusV1RDTO dto) {
    this.oid = dto.getOid();
    this.koulutusOid = dto.getKomoOid();
    this.alkamiskausiUri =
        dto.getKoulutuksenAlkamiskausi() == null ? null : dto.getKoulutuksenAlkamiskausi().getUri();
    this.alkamisvuosi = dto.getKoulutuksenAlkamisvuosi();
    this.opetuskielet = dto.getOpetuskielis().getUris().keySet();
    this.osaamisalaUris = new HashSet<>();
  }

  public Toteutus(KoutaToteutus dto) {
    this.oid = dto.oid;
    this.koulutusOid = dto.koulutusOid;
    this.alkamiskausiUri = dto.getKoulutuksenAlkamiskausi();
    this.alkamisvuosi = dto.getKoulutuksenAlkamisvuosi();
    this.opetuskielet =
        dto.metadata.opetus.opetuskieliKoodiUrit.stream()
            .flatMap(
                oppilaitoksenopetuskieliUri -> {
                  switch (oppilaitoksenopetuskieliUri.split("#")[0]) {
                    case "oppilaitoksenopetuskieli_1":
                      return Stream.of("kieli_fi");
                    case "oppilaitoksenopetuskieli_2":
                      return Stream.of("kieli_sv");
                    case "oppilaitoksenopetuskieli_3":
                      return Stream.of("kieli_fi", "kieli_sv");
                    case "oppilaitoksenopetuskieli_4":
                      return Stream.of("kieli_en");
                    case "oppilaitoksenopetuskieli_5":
                      return Stream.of("kieli_se");
                    case "oppilaitoksenopetuskieli_9":
                      return Stream.of("kieli_xx");
                    default:
                      throw new IllegalArgumentException(
                          String.format(
                              "Tuntematon oppilaitoksenopetuskieli koodi %s",
                              oppilaitoksenopetuskieliUri));
                  }
                })
            .collect(Collectors.toSet());
    this.osaamisalaUris = dto.getOsaamisalaUris();
  }
}
