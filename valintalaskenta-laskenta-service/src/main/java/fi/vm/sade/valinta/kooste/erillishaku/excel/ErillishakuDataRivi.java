package fi.vm.sade.valinta.kooste.erillishaku.excel;

import static fi.vm.sade.valinta.kooste.erillishaku.dto.Hakutyyppi.KORKEAKOULU;
import static fi.vm.sade.valinta.kooste.erillishaku.dto.Hakutyyppi.TOISEN_ASTEEN_OPPILAITOS;

import fi.vm.sade.sijoittelu.tulos.dto.HakemuksenTila;
import fi.vm.sade.sijoittelu.tulos.dto.IlmoittautumisTila;
import fi.vm.sade.sijoittelu.tulos.dto.ValintatuloksenTila;
import fi.vm.sade.valinta.kooste.erillishaku.dto.Hakutyyppi;
import fi.vm.sade.valinta.kooste.excel.DataRivi;
import fi.vm.sade.valinta.kooste.excel.ExcelValidointiPoikkeus;
import fi.vm.sade.valinta.kooste.excel.Rivi;
import fi.vm.sade.valinta.kooste.excel.arvo.Arvo;
import fi.vm.sade.valinta.kooste.excel.arvo.MonivalintaArvo;
import fi.vm.sade.valinta.kooste.external.resource.koodisto.KoodistoCachedAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.koodisto.dto.Koodi;
import fi.vm.sade.valinta.kooste.external.resource.valintatulosservice.dto.Maksuntila;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.commons.lang.StringUtils;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.slf4j.Logger;

public class ErillishakuDataRivi extends DataRivi {

  private static final Logger LOG = org.slf4j.LoggerFactory.getLogger(ErillishakuDataRivi.class);
  static final DateTimeFormatter LAHETETTYFORMAT = DateTimeFormat.forPattern("dd.MM.yyyy HH:mm");
  private final ErillishakuRiviKuuntelija kuuntelija;
  private final Hakutyyppi tyyppi;

  ErillishakuDataRivi(
      Hakutyyppi tyyppi, ErillishakuRiviKuuntelija kuuntelija, Collection<Collection<Arvo>> s) {
    super(s);
    this.tyyppi = tyyppi;
    this.kuuntelija = kuuntelija;
  }

  @Override
  public boolean validoi(Rivi rivi) throws ExcelValidointiPoikkeus {
    int index = 0;
    String sukunimi = rivi.getArvoAt(index++);
    String etunimi = rivi.getArvoAt(index++);
    String henkilotunnus = rivi.getArvoAt(index++);
    String sahkoposti = rivi.getArvoAt(index++);
    String syntymaAika = rivi.getArvoAt(index++);
    Sukupuoli sukupuoli = Sukupuoli.fromString(rivi.getArvoAt(index++));
    String oid = rivi.getArvoAt(index++);
    String aidinkieli = rivi.getArvoAt(index++);

    String hakemuksenTila = rivi.getArvoAt(index++);
    boolean ehdollisestiHyvaksytty = tyyppi == KORKEAKOULU && TOSI.equals(rivi.getArvoAt(index++));
    String ehdollisestiHyvaksymisenEhtoKoodi =
        tyyppi == KORKEAKOULU ? rivi.getArvoAt(index++) : null;
    String ehdollisestiHyvaksymisenEhtoFI = tyyppi == KORKEAKOULU ? rivi.getArvoAt(index++) : null;
    String ehdollisestiHyvaksymisenEhtoSV = tyyppi == KORKEAKOULU ? rivi.getArvoAt(index++) : null;
    String ehdollisestiHyvaksymisenEhtoEN = tyyppi == KORKEAKOULU ? rivi.getArvoAt(index++) : null;
    Date hyvaksymiskirjeLahetetty = parseLahetettyDate(rivi.getArvoAt(index++));
    String vastaanottoTila = rivi.getArvoAt(index++);
    String ilmoittautumisTila = rivi.getArvoAt(index++);
    String maksuvelvollisuus =
        tyyppi == KORKEAKOULU ? rivi.getArvoAt(index++) : Maksuvelvollisuus.NOT_CHECKED;
    Maksuntila maksuntila =
        tyyppi == KORKEAKOULU ? Maksuntila.fromString(rivi.getArvoAt(index++)) : null;
    boolean julkaistaankoTiedot = LUPA_JULKAISUUN.equals(rivi.getArvoAt(index++));

    String asiointikieli = rivi.getArvoAt(index++);
    String puhelinnumero = rivi.getArvoAt(index++);
    String osoite = rivi.getArvoAt(index++);
    String postinumero = rivi.getArvoAt(index++);
    String postitoimipaikka = rivi.getArvoAt(index++);
    String asuinmaa = rivi.getArvoAt(index++);
    String kansalaisuus = rivi.getArvoAt(index++);
    String kotikunta = rivi.getArvoAt(index++);
    Boolean toisenAsteenSuoritus =
        tyyppi == KORKEAKOULU ? getBoolean(rivi.getArvoAt(index++)) : null;
    String toisenAsteenSuoritusmaa = tyyppi == KORKEAKOULU ? rivi.getArvoAt(index++) : "";

    String kutsumanimi = rivi.getArvoAt(index++);
    String syntymapaikka = rivi.getArvoAt(index++);
    String passinNumero = rivi.getArvoAt(index++);
    String idTunnus = rivi.getArvoAt(index++);
    String kaupunkiJaMaa = rivi.getArvoAt(index++);
    String hakemusOid = rivi.getArvoAt(index);

    if (isDataRow(rivi, sukunimi, etunimi, oid)) {
      kuuntelija.erillishakuRiviTapahtuma(
          new ErillishakuRiviBuilder()
              .sukunimi(sukunimi)
              .etunimi(etunimi)
              .henkilotunnus(henkilotunnus)
              .sahkoposti(sahkoposti)
              .syntymaAika(syntymaAika)
              .sukupuoli(sukupuoli)
              .personOid(oid)
              .aidinkieli(aidinkieli)
              .hakemuksenTila(hakemuksenTila)
              .ehdollisestiHyvaksyttavissa(ehdollisestiHyvaksytty)
              .ehdollisenHyvaksymisenEhtoKoodi(ehdollisestiHyvaksymisenEhtoKoodi)
              .ehdollisenHyvaksymisenEhtoFI(ehdollisestiHyvaksymisenEhtoFI)
              .ehdollisenHyvaksymisenEhtoSV(ehdollisestiHyvaksymisenEhtoSV)
              .ehdollisenHyvaksymisenEhtoEN(ehdollisestiHyvaksymisenEhtoEN)
              .hyvaksymiskirjeLahetetty(hyvaksymiskirjeLahetetty)
              .vastaanottoTila(vastaanottoTila)
              .ilmoittautumisTila(ilmoittautumisTila)
              .julkaistaankoTiedot(julkaistaankoTiedot)
              .poistetaankoRivi(false)
              .asiointikieli(asiointikieli)
              .puhelinnumero(puhelinnumero)
              .osoite(osoite)
              .postinumero(postinumero)
              .postitoimipaikka(postitoimipaikka)
              .asuinmaa(asuinmaa)
              .kansalaisuus(kansalaisuus)
              .kotikunta(kotikunta)
              .toisenAsteenSuoritus(toisenAsteenSuoritus)
              .toisenAsteenSuoritusmaa(toisenAsteenSuoritusmaa)
              .maksuvelvollisuus(maksuvelvollisuus)
              .maksuntila(maksuntila)
              .kutsumanimi(kutsumanimi)
              .syntymapaikka(syntymapaikka)
              .passinNumero(passinNumero)
              .idTunnus(idTunnus)
              .kaupunkiJaMaa(kaupunkiJaMaa)
              .hakemusOid(hakemusOid)
              .build());
    }
    return true;
  }

  private Date parseLahetettyDate(String arvo) {
    if (StringUtils.isNotEmpty(arvo)
        && !ErillishakuExcel.HEADER_HYVAKSYMISKIRJE_LAHETETTY.equals(arvo)) {
      try {
        return LAHETETTYFORMAT.parseDateTime(arvo).toDate();
      } catch (Exception e) {
        LOG.warn("Could not parse hyvaksymiskirjeLahetetty '{}'", arvo);
      }
    }
    return null;
  }

  private boolean isDataRow(Rivi rivi, String sukunimi, String etunimi, String oid) {
    return !rivi.isTyhja()
        && rivi.getSolut().size() >= 14 // Copy-paste easily creates extra columns for excel doc
        && !"Sukunimi".equals(sukunimi)
        && ((StringUtils.isNotBlank(sukunimi) && StringUtils.isNotBlank(etunimi))
            || StringUtils.isNotBlank(oid));
  }

  static final String TOSI = "Kyllä";
  static final String EPATOSI = "Ei";
  static final Collection<String> TOTUUSARVO = Arrays.asList(EPATOSI, TOSI);

  public static String getTotuusarvoString(Boolean b) {
    if (b == null) {
      return "";
    }
    return getTotuusarvoString(b.booleanValue());
  }

  private static Boolean getBoolean(String totuusarvo) {
    if (TOSI.equals(totuusarvo)) {
      return Boolean.TRUE;
    }
    if (EPATOSI.equals(totuusarvo)) {
      return Boolean.FALSE;
    }
    return null;
  }

  static String getTotuusarvoString(boolean b) {
    return b ? TOSI : EPATOSI;
  }

  static final Collection<String> SUKUPUOLEN_ARVOT =
      Arrays.stream(Sukupuoli.values()).map(Object::toString).collect(Collectors.toList());
  private static final Collection<String> HAKEMUKSENTILA_ARVOT =
      Stream.concat(Stream.of("KESKEN"), Arrays.stream(HakemuksenTila.values()).map(Enum::toString))
          .collect(Collectors.toList());
  private static final Collection<String> HAKEMUKSENTILA_ARVOT_TOINEN_ASTE =
      Stream.concat(Stream.of("KESKEN"), Arrays.stream(HakemuksenTila.values()).map(Enum::toString))
          .collect(Collectors.toList());
  private static final Collection<String> HAKEMUKSENTILA_ARVOT_KK =
      Stream.concat(
              Stream.of("KESKEN"),
              Arrays.stream(HakemuksenTila.values())
                  .filter(t -> !HakemuksenTila.HARKINNANVARAISESTI_HYVAKSYTTY.equals(t))
                  .map(Enum::toString))
          .collect(Collectors.toList());
  private static final Collection<String> VASTAANOTTOTILA_ARVOT =
      Arrays.stream(ValintatuloksenTila.values()).map(Enum::toString).collect(Collectors.toList());
  private static final Collection<String> VASTAANOTTOTILA_ARVOT_KK =
      Stream.of(
              // KORKEAKOULUJEN VALINTATULOKSEN TILAT
              ValintatuloksenTila.EI_VASTAANOTETTU_MAARA_AIKANA,
              ValintatuloksenTila.PERUNUT,
              ValintatuloksenTila.PERUUTETTU,
              ValintatuloksenTila.VASTAANOTTANUT_SITOVASTI,
              ValintatuloksenTila.KESKEN
              //
              )
          .map(Enum::toString)
          .collect(Collectors.toList());
  private static final Collection<String> VASTAANOTTOTILA_ARVOT_TOINEN_ASTE =
      Stream.of(
              // TOISEN ASTEEN VALINTATULOKSEN TILAT
              ValintatuloksenTila.VASTAANOTTANUT_SITOVASTI,
              ValintatuloksenTila.EI_VASTAANOTETTU_MAARA_AIKANA,
              ValintatuloksenTila.PERUNUT,
              ValintatuloksenTila.KESKEN
              //
              )
          .map(Enum::toString)
          .collect(Collectors.toList());
  private static final Collection<String> ILMOITTAUTUMISTILA_ARVOT =
      Arrays.stream(IlmoittautumisTila.values()).map(Enum::toString).collect(Collectors.toList());
  private static final String LUPA_JULKAISUUN = "JULKAISTAVISSA";
  private static final String EI_LUPAA_JULKAISUUN = "EI JULKAISTAVISSA";
  private static final Collection<String> JULKAISU_LUPA_ARVOT =
      Arrays.asList(LUPA_JULKAISUUN, StringUtils.EMPTY, EI_LUPAA_JULKAISUUN);
  public static final Collection<String> ASIONTIKIELEN_ARVOT = Arrays.asList("fi", "sv", "en");

  public static MonivalintaArvo hakemuksenTila(Hakutyyppi hakutyyppi, String arvo) {
    return getMonivalintaArvo(
        hakutyyppi,
        arvo,
        HAKEMUKSENTILA_ARVOT_TOINEN_ASTE,
        HAKEMUKSENTILA_ARVOT_KK,
        HAKEMUKSENTILA_ARVOT);
  }

  static MonivalintaArvo vastaanottoTila(Hakutyyppi hakutyyppi, String arvo) {
    return getMonivalintaArvo(
        hakutyyppi,
        arvo,
        VASTAANOTTOTILA_ARVOT_TOINEN_ASTE,
        VASTAANOTTOTILA_ARVOT_KK,
        VASTAANOTTOTILA_ARVOT);
  }

  private static MonivalintaArvo getMonivalintaArvo(
      Hakutyyppi hakutyyppi,
      String arvo,
      Collection<String> toinenAste,
      Collection<String> kk,
      Collection<String> other) {
    if (TOISEN_ASTEEN_OPPILAITOS.equals(hakutyyppi)) return new MonivalintaArvo(arvo, toinenAste);
    if (KORKEAKOULU.equals(hakutyyppi)) return new MonivalintaArvo(arvo, kk);
    return new MonivalintaArvo(arvo, other);
  }

  static MonivalintaArvo julkaisuLupa(boolean arvo) {
    return new MonivalintaArvo(arvo ? LUPA_JULKAISUUN : EI_LUPAA_JULKAISUUN, JULKAISU_LUPA_ARVOT);
  }

  public static MonivalintaArvo ilmoittautumisTila(String arvo) {
    return new MonivalintaArvo(arvo, ILMOITTAUTUMISTILA_ARVOT);
  }

  static MonivalintaArvo asiointiKieli(String arvo) {
    return new MonivalintaArvo(arvo, ASIONTIKIELEN_ARVOT);
  }

  private static final Collection<String> MAKSUVELVOLLISUUS_ARVOT =
      Arrays.asList(
          Maksuvelvollisuus.NOT_CHECKED,
          Maksuvelvollisuus.REQUIRED,
          Maksuvelvollisuus.NOT_REQUIRED);
  private static final Collection<String> MAKSUNTILA_ARVOT =
      Arrays.asList(
          "",
          Maksuntila.MAKSAMATTA.toString(),
          Maksuntila.MAKSETTU.toString(),
          Maksuntila.VAPAUTETTU.toString());

  static MonivalintaArvo maksuvelvollisuus(String arvo) {
    return new MonivalintaArvo(arvo, MAKSUVELVOLLISUUS_ARVOT);
  }

  static MonivalintaArvo maksuntila(String arvo) {
    return new MonivalintaArvo(arvo, MAKSUNTILA_ARVOT);
  }

  public static MonivalintaArvo ehdollisenHyvaksymisenEhtoKoodi(
      String ehdollisenHyvaksymisenEhtoKoodi,
      KoodistoCachedAsyncResource koodistoCachedAsyncResource) {
    Map<String, Koodi> hyvaksymisenEhdot =
        koodistoCachedAsyncResource.haeKoodisto(KoodistoCachedAsyncResource.HYVAKSYNNAN_EHDOT);
    Collection<String> ehdollisenHyvaksymisenEhdot = new ArrayList<>();
    ehdollisenHyvaksymisenEhdot.add("");
    for (Koodi koodi : hyvaksymisenEhdot.values()) {
      ehdollisenHyvaksymisenEhdot.add(
          koodi.getKoodiArvo()
              + " "
              + KoodistoCachedAsyncResource.haeKoodistaArvo(koodi, "FI", null));
    }
    return new MonivalintaArvo(ehdollisenHyvaksymisenEhtoKoodi, ehdollisenHyvaksymisenEhdot);
  }

  public static MonivalintaArvo aidinkieli(
      String aidinkieli, KoodistoCachedAsyncResource koodistoCachedAsyncResource) {
    Map<String, Koodi> kielet =
        koodistoCachedAsyncResource.haeKoodisto(KoodistoCachedAsyncResource.KIELI);
    Collection<String> aidinkielet = new ArrayList<>();
    aidinkielet.add("");
    for (Koodi koodi : kielet.values()) {
      aidinkielet.add(koodi.getKoodiArvo());
    }
    return new MonivalintaArvo(aidinkieli, aidinkielet);
  }
}
