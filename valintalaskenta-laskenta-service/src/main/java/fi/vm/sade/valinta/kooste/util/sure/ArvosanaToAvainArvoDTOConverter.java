package fi.vm.sade.valinta.kooste.util.sure;

import com.codepoetics.protonpack.StreamUtils;
import fi.vm.sade.valinta.kooste.external.resource.suoritusrekisteri.dto.Arvosana;
import fi.vm.sade.valinta.kooste.external.resource.suoritusrekisteri.dto.SuoritusJaArvosanat;
import fi.vm.sade.valintalaskenta.domain.dto.AvainArvoDTO;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.math.Fraction;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** Prefiksi PK_ */
public class ArvosanaToAvainArvoDTOConverter {
  private static final Logger LOG = LoggerFactory.getLogger(ArvosanaToAvainArvoDTOConverter.class);
  private static final String SUORITUSMERKINTA = "S";
  private static final String OPPIAINE = "_OPPIAINE";
  private static final String SUORITETTU = "_SUORITETTU";
  private static final String VALINNAINEN = "_VAL";

  public static Set<AvainArvoDTO> convert(
      List<SuoritusJaArvosanat> sureSuoritukset,
      String prefix,
      String suffix,
      final String hakemusOid) {
    List<List<OppiaineArvosana>> suoritukset =
        sureSuoritukset.stream()
            .map(
                s ->
                    s.getArvosanat().stream()
                        .map(
                            arvosana -> {
                              validateArvosanaForHakemus(hakemusOid, arvosana);
                              return new OppiaineArvosana(arvosana);
                            })
                        .collect(Collectors.toList()))
            .collect(Collectors.toList());
    Stream<OppiaineArvosana> parhaatArvosanat =
        Stream.concat(
            aineidenValinnaisetArvosanatSuorituksittain(suoritukset.stream())
                .flatMap(
                    aineenArvosanatSuorituksittain ->
                        parhaanSuorituksenArvosanat(aineenArvosanatSuorituksittain)),
            arvosanatAineittain(varsinaisetArvosanat(suoritukset.stream())).values().stream()
                .map(aineenArvosanat -> parasArvosana(aineenArvosanat)));
    return palautaAinenumerointi(parhaatArvosanat)
        .flatMap(a -> arvosanaToAvainArvo(a, prefix, suffix))
        .collect(Collectors.toSet());
  }

  private static void validateArvosanaForHakemus(String hakemusOid, Arvosana arvosana) {
    if (arvosana.getAine().matches("[AB]\\d+")) {
      String oppiaine = arvosana.getLisatieto();
      if (oppiaine == null) {
        throw new RuntimeException(
            String.format(
                "(Hakemus %s) Arvosanalta %s puuttuu oppiaine", hakemusOid, arvosana.getAine()));
      }
    }
  }

  public static Stream<OppiaineArvosana> palautaAinenumerointi(Stream<OppiaineArvosana> arvosanat) {
    Map<String, String> oppiainenumero = new HashMap<>();
    Map<String, Integer> vapaaOppiainenumero = new HashMap<>();

    return arvosanat.map(
        a -> {
          if (a.lisatieto != null && (a.aine.endsWith(a.lisatieto) || a.aine.equals("AI"))) {
            String aine = a.aine.substring(0, 2);
            if (!oppiainenumero.containsKey(a.aine)) {
              int numero = vapaaOppiainenumero.getOrDefault(aine, 1);
              vapaaOppiainenumero.put(aine, numero + 1);
              oppiainenumero.put(a.aine, numero == 1 ? "" : String.valueOf(numero));
            }
            return new OppiaineArvosana(
                aine + oppiainenumero.get(a.aine),
                a.lisatieto,
                a.valinnainen,
                a.jarjestys,
                a.arvosana,
                a.asteikko);
          } else {
            return a;
          }
        });
  }

  private static Optional<Fraction> keskiarvo(List<OppiaineArvosana> arvosanat) {
    List<Integer> numeeriset = numeerisetArvosanat(arvosanat);
    if (numeeriset.isEmpty()) {
      return Optional.empty();
    }
    int summa = numeeriset.stream().mapToInt(i -> i).sum();
    return Optional.of(Fraction.getFraction(summa, numeeriset.size()));
  }

  private static List<Integer> numeerisetArvosanat(List<OppiaineArvosana> l) {
    return l.stream()
        .filter(a -> !SUORITUSMERKINTA.equalsIgnoreCase(a.arvosana))
        .map(a -> Integer.parseInt(a.arvosana))
        .collect(Collectors.toList());
  }

  private static Stream<OppiaineArvosana> parhaanSuorituksenArvosanat(
      List<List<OppiaineArvosana>> suoritustenArvosanat) {
    return suoritustenArvosanat.stream()
        .sorted(
            (v, w) -> {
              Fraction minusOne = Fraction.ONE.negate();
              int r = keskiarvo(w).orElse(minusOne).compareTo(keskiarvo(v).orElse(minusOne));
              // jos keskiarvot samat, tai kumpaakaan ei voitu laskea, valitse enemmän suorituksia
              return r == 0 ? Integer.compare(w.size(), v.size()) : r;
            })
        .findFirst()
        .get()
        .stream();
  }

  private static Stream<List<List<OppiaineArvosana>>> aineidenValinnaisetArvosanatSuorituksittain(
      Stream<List<OppiaineArvosana>> suoritukset) {
    return suoritukset
        .flatMap(
            s -> {
              Map<String, List<OppiaineArvosana>> aineittain =
                  arvosanatAineittain(valinnaisetArvosanat(s));
              aineittain.values().forEach(aineenArvosanat -> normalisoiJarjestys(aineenArvosanat));
              return aineittain.entrySet().stream();
            })
        .collect(
            Collectors.groupingBy(
                e -> e.getKey(), Collectors.mapping(e -> e.getValue(), Collectors.toList())))
        .values()
        .stream();
  }

  private static void normalisoiJarjestys(List<OppiaineArvosana> arvosanat) {
    StreamUtils.zipWithIndex(
            arvosanat.stream().sorted((a0, a1) -> a0.jarjestys.compareTo(a1.jarjestys)))
        .forEach(zip -> zip.getValue().jarjestys = Math.toIntExact(zip.getIndex() + 1));
  }

  private static Map<String, List<OppiaineArvosana>> arvosanatAineittain(
      Stream<OppiaineArvosana> arvosanat) {
    return arvosanat.collect(Collectors.groupingBy(a -> a.aine));
  }

  private static Stream<OppiaineArvosana> varsinaisetArvosanat(
      Stream<List<OppiaineArvosana>> suoritukset) {
    return suoritukset.flatMap(s -> s.stream()).filter(a -> !a.valinnainen);
  }

  private static Stream<OppiaineArvosana> valinnaisetArvosanat(List<OppiaineArvosana> suoritus) {
    return suoritus.stream().filter(a -> a.valinnainen);
  }

  public static OppiaineArvosana parasArvosana(List<OppiaineArvosana> arvosanat) {
    return arvosanat.stream()
        .sorted(
            (c0, c1) -> {
              varmistaYhteensopivatAsteikot(c0, c1);
              if (SUORITUSMERKINTA.equals(c0.arvosana)) {
                return 1;
              }
              if (SUORITUSMERKINTA.equals(c1.arvosana)) {
                return -1;
              }
              Integer i0 = Integer.parseInt(c0.arvosana);
              Integer i1 = Integer.parseInt(c1.arvosana);
              return i1.compareTo(i0);
            })
        .findFirst()
        .get();
  }

  private static void varmistaYhteensopivatAsteikot(OppiaineArvosana c0, OppiaineArvosana c1) {
    if (!StringUtils.equals(c0.asteikko, c1.asteikko)) {
      String msg = String.format("Asteikot ei täsmää: %s %s", c0.asteikko, c1.asteikko);
      LOG.error(msg);
      throw new RuntimeException(msg);
    }
  }

  private static Stream<AvainArvoDTO> arvosanaToAvainArvo(
      OppiaineArvosana arvosana, String prefix, String suffix) {
    AvainArvoDTO a;
    if (arvosana.valinnainen) {
      a =
          new AvainArvoDTO(
              prefix + arvosana.aine + VALINNAINEN + arvosana.jarjestys + suffix,
              arvosana.arvosana);
    } else {
      a = new AvainArvoDTO(prefix + arvosana.aine + suffix, arvosana.arvosana);
    }
    if (SUORITUSMERKINTA.equals(a.getArvo())) {
      a.setAvain(a.getAvain() + SUORITETTU);
      a.setArvo("true");
    }
    if (arvosana.lisatieto != null) {
      return Stream.of(
          a, new AvainArvoDTO(prefix + arvosana.aine + suffix + OPPIAINE, arvosana.lisatieto));
    }
    return Stream.of(a);
  }

  public static class OppiaineArvosana {
    public final String aine;
    public final String lisatieto;
    public final boolean valinnainen;
    public Integer jarjestys;
    public final String arvosana;
    public final String asteikko;

    public OppiaineArvosana(
        String aine,
        String lisatieto,
        boolean valinnainen,
        Integer jarjestys,
        String arvosana,
        String asteikko) {
      this.aine = aine;
      this.lisatieto = lisatieto;
      this.valinnainen = valinnainen;
      this.jarjestys = jarjestys;
      this.arvosana = arvosana;
      this.asteikko = asteikko;
    }

    public OppiaineArvosana(Arvosana arvosana) {
      if (arvosana.getAine().matches("[AB]\\d+")
          || (arvosana.getAine().equals("AI") && arvosana.getLisatieto() != null)) {
        this.aine = arvosana.getAine().substring(0, 2) + arvosana.getLisatieto();
      } else {
        this.aine = arvosana.getAine();
      }
      this.lisatieto = arvosana.getLisatieto();
      this.valinnainen = arvosana.isValinnainen();
      this.jarjestys = arvosana.getJarjestys();
      this.arvosana = arvosana.getArvio().getArvosana();
      this.asteikko = arvosana.getArvio().getAsteikko();
    }

    @Override
    public String toString() {
      return "OppiaineArvosana{"
          + "aine='"
          + aine
          + '\''
          + ", lisatieto='"
          + lisatieto
          + '\''
          + ", valinnainen="
          + valinnainen
          + ", jarjestys="
          + jarjestys
          + ", arvosana='"
          + arvosana
          + '\''
          + ", asteikko='"
          + asteikko
          + '\''
          + '}';
    }
  }
}
