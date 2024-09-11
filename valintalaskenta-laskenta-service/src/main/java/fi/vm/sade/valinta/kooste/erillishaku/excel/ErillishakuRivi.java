package fi.vm.sade.valinta.kooste.erillishaku.excel;

import static org.apache.commons.lang.StringUtils.*;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import fi.vm.sade.valinta.kooste.external.resource.oppijanumerorekisteri.dto.HenkiloCreateDTO;
import fi.vm.sade.valinta.kooste.external.resource.oppijanumerorekisteri.dto.HenkiloTyyppi;
import fi.vm.sade.valinta.kooste.external.resource.valintatulosservice.dto.Maksuntila;
import fi.vm.sade.valinta.kooste.util.HenkilotunnusTarkistusUtil;
import io.swagger.v3.oas.annotations.media.Schema;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Date;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Schema
public class ErillishakuRivi {
  private static final Logger LOG = LoggerFactory.getLogger(ErillishakuRivi.class);
  public static final DateTimeFormatter SYNTYMAAIKAFORMAT =
      DateTimeFormatter.ofPattern("dd.MM.yyyy");
  public static final DateTimeFormatter SYNTYMAAIKAFORMAT_JSON =
      DateTimeFormatter.ofPattern("yyyy-MM-dd");

  private final String etunimi;
  private final String sukunimi;
  private final String hakemusOid;

  private final String henkilotunnus;
  private final String sahkoposti;
  private final String syntymaAika;

  @JsonDeserialize(using = SukupuoliDeserializer.class)
  private final Sukupuoli sukupuoli;

  @Schema(required = true)
  private final String aidinkieli;

  private final String personOid;

  @Schema(required = true)
  private final String hakemuksenTila;

  private final boolean ehdollisestiHyvaksyttavissa;
  private String ehdollisenHyvaksymisenEhtoKoodi;
  private final String ehdollisenHyvaksymisenEhtoFI;
  private final String ehdollisenHyvaksymisenEhtoSV;
  private final String ehdollisenHyvaksymisenEhtoEN;
  private final String valinnantilanKuvauksenTekstiFI;
  private final String valinnantilanKuvauksenTekstiSV;
  private final String valinnantilanKuvauksenTekstiEN;
  private final Date hyvaksymiskirjeLahetetty;
  private final String vastaanottoTila;
  private final String ilmoittautumisTila;

  @Schema(required = true)
  private final boolean julkaistaankoTiedot;

  private final boolean poistetaankoRivi;
  private final String maksuvelvollisuus;
  private final Maksuntila maksuntila;

  private final String asiointikieli;
  private final String puhelinnumero;
  private final String osoite;
  private final String postinumero;
  private final String postitoimipaikka;
  private final String asuinmaa;
  private final String kansalaisuus;
  private final String kotikunta;
  private final Boolean toisenAsteenSuoritus;
  private final String toisenAsteenSuoritusmaa;

  private final String kutsumanimi;
  private final String syntymapaikka;
  private final String passinNumero;
  private final String idTunnus;
  private final String kaupunkiJaMaa;

  // Empty constructor for Jackson JSON library. Deserialization fails without this!
  public ErillishakuRivi() {
    this(
        null,
        null,
        null,
        null,
        null,
        null,
        Sukupuoli.EI_SUKUPUOLTA,
        null,
        null,
        null,
        false,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        false,
        false,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null,
        null);
  }

  public ErillishakuRivi(
      String hakemusOid,
      String sukunimi,
      String etunimi,
      String henkilotunnus,
      String sahkoposti,
      String syntymaAika,
      Sukupuoli sukupuoli,
      String personOid,
      String aidinkieli,
      String hakemuksenTila,
      boolean ehdollisestiHyvaksyttavissa,
      String ehdollisenHyvaksymisenEhtoKoodi,
      String ehdollisenHyvaksymisenEhtoFI,
      String ehdollisenHyvaksymisenEhtoSV,
      String ehdollisenHyvaksymisenEhtoEN,
      String valinnantilanKuvauksenTekstiFI,
      String valinnantilanKuvauksenTekstiSV,
      String valinnantilanKuvauksenTekstiEN,
      Date hyvaksymiskirjeLahetetty,
      String vastaanottoTila,
      String ilmoittautumisTila,
      boolean julkaistaankoTiedot,
      boolean poistetaankoRivi,
      String asiointikieli,
      String puhelinnumero,
      String osoite,
      String postinumero,
      String postitoimipaikka,
      String asuinmaa,
      String kansalaisuus,
      String kotikunta,
      Boolean toisenAsteenSuoritus,
      String toisenAsteenSuoritusmaa,
      String maksuvelvollisuus,
      Maksuntila maksuntila,
      String kutsumanimi,
      String syntymapaikka,
      String passinNumero,
      String idTunnus,
      String kaupunkiJaMaa) {
    this.hakemusOid = hakemusOid;
    this.etunimi = etunimi;
    this.sukunimi = sukunimi;
    this.henkilotunnus = henkilotunnus;
    this.sahkoposti = sahkoposti;
    this.syntymaAika = syntymaAika;
    this.sukupuoli = sukupuoli;
    this.aidinkieli = aidinkieli;
    this.personOid = personOid;
    this.hakemuksenTila = hakemuksenTila;
    this.ehdollisestiHyvaksyttavissa = ehdollisestiHyvaksyttavissa;
    this.ehdollisenHyvaksymisenEhtoKoodi = ehdollisenHyvaksymisenEhtoKoodi;
    this.ehdollisenHyvaksymisenEhtoFI = ehdollisenHyvaksymisenEhtoFI;
    this.ehdollisenHyvaksymisenEhtoSV = ehdollisenHyvaksymisenEhtoSV;
    this.ehdollisenHyvaksymisenEhtoEN = ehdollisenHyvaksymisenEhtoEN;
    this.valinnantilanKuvauksenTekstiFI = valinnantilanKuvauksenTekstiFI;
    this.valinnantilanKuvauksenTekstiSV = valinnantilanKuvauksenTekstiSV;
    this.valinnantilanKuvauksenTekstiEN = valinnantilanKuvauksenTekstiEN;
    this.hyvaksymiskirjeLahetetty = hyvaksymiskirjeLahetetty;
    this.vastaanottoTila = vastaanottoTila;
    this.ilmoittautumisTila = ilmoittautumisTila;
    this.julkaistaankoTiedot = julkaistaankoTiedot;
    this.poistetaankoRivi = poistetaankoRivi;
    this.asiointikieli = asiointikieli;
    this.puhelinnumero = puhelinnumero;
    this.osoite = osoite;
    this.postinumero = postinumero;
    this.postitoimipaikka = postitoimipaikka;
    this.asuinmaa = asuinmaa;
    this.kansalaisuus = kansalaisuus;
    this.kotikunta = kotikunta;
    this.toisenAsteenSuoritus = toisenAsteenSuoritus;
    this.toisenAsteenSuoritusmaa = toisenAsteenSuoritusmaa;
    this.maksuvelvollisuus = maksuvelvollisuus;
    this.maksuntila = maksuntila;
    this.kutsumanimi = kutsumanimi;
    this.syntymapaikka = syntymapaikka;
    this.passinNumero = passinNumero;
    this.idTunnus = idTunnus;
    this.kaupunkiJaMaa = kaupunkiJaMaa;
  }

  public String getHakemusOid() {
    return hakemusOid;
  }

  public boolean isJulkaistaankoTiedot() {
    return julkaistaankoTiedot;
  }

  public String getEtunimi() {
    return trimToEmpty(etunimi);
  }

  public String getPersonOid() {
    return trimToEmpty(personOid);
  }

  public String getHenkilotunnus() {
    return trimToEmpty(henkilotunnus);
  }

  public String getSahkoposti() {
    return trimToEmpty(sahkoposti);
  }

  public String getSukunimi() {
    return trimToEmpty(sukunimi);
  }

  public String getSyntymaAika() {
    return trimToEmpty(syntymaAika);
  }

  public Sukupuoli getSukupuoli() {
    if (!StringUtils.isBlank(henkilotunnus)) {
      // palauta hetun sukupuoli
      return HenkilotunnusTarkistusUtil.palautaSukupuoli(henkilotunnus);
    }
    return sukupuoli;
  }

  public String getAidinkieli() {
    return aidinkieli;
  }

  public String getHakemuksenTila() {
    return trimToEmpty(hakemuksenTila);
  }

  public boolean getEhdollisestiHyvaksyttavissa() {
    return ehdollisestiHyvaksyttavissa;
  }

  public String getEhdollisenHyvaksymisenEhtoKoodi() {
    return ehdollisenHyvaksymisenEhtoKoodi;
  }

  public void setEhdollisenHyvaksymisenEhtoKoodi(String ehdollisenHyvaksymisenEhtoKoodi) {
    this.ehdollisenHyvaksymisenEhtoKoodi = ehdollisenHyvaksymisenEhtoKoodi;
  }

  public String getEhdollisenHyvaksymisenEhtoFI() {
    return ehdollisenHyvaksymisenEhtoFI;
  }

  public String getEhdollisenHyvaksymisenEhtoSV() {
    return ehdollisenHyvaksymisenEhtoSV;
  }

  public String getEhdollisenHyvaksymisenEhtoEN() {
    return ehdollisenHyvaksymisenEhtoEN;
  }

  public String getValinnantilanKuvauksenTekstiFI() {
    return valinnantilanKuvauksenTekstiFI;
  }

  public String getValinnantilanKuvauksenTekstiSV() {
    return valinnantilanKuvauksenTekstiSV;
  }

  public String getValinnantilanKuvauksenTekstiEN() {
    return valinnantilanKuvauksenTekstiEN;
  }

  public Date getHyvaksymiskirjeLahetetty() {
    return hyvaksymiskirjeLahetetty;
  }

  public String getIlmoittautumisTila() {
    return trimToEmpty(ilmoittautumisTila);
  }

  public String getVastaanottoTila() {
    return trimToEmpty(vastaanottoTila);
  }

  private String suojaaHenkilotunnusLogeilta(String hetu) {
    if (trimToNull(hetu) == null) {
      return "***HENKILOTUNNUS***";
    } else {
      return EMPTY;
    }
  }

  public String getAsiointikieli() {
    return asiointikieli;
  }

  public String getPuhelinnumero() {
    return puhelinnumero;
  }

  public String getOsoite() {
    return osoite;
  }

  public String getPostinumero() {
    return postinumero;
  }

  public String getPostitoimipaikka() {
    return postitoimipaikka;
  }

  public String getAsuinmaa() {
    return asuinmaa;
  }

  public String getKansalaisuus() {
    return kansalaisuus;
  }

  public String getKotikunta() {
    return kotikunta;
  }

  public Boolean getToisenAsteenSuoritus() {
    return toisenAsteenSuoritus;
  }

  public String getToisenAsteenSuoritusmaa() {
    return toisenAsteenSuoritusmaa;
  }

  @Override
  public String toString() {
    return etunimi
        + ", "
        + sukunimi
        + ", "
        + sahkoposti
        + ", "
        + hakemuksenTila
        + ", "
        + ErillishakuDataRivi.getTotuusarvoString(ehdollisestiHyvaksyttavissa)
        + ", "
        + suojaaHenkilotunnusLogeilta(henkilotunnus)
        + ", "
        + syntymaAika
        + ", "
        + sukupuoli
        + ", "
        + aidinkieli
        + ", "
        + ilmoittautumisTila
        + ", "
        + ehdollisestiHyvaksyttavissa
        + ", "
        + hyvaksymiskirjeLahetetty
        + ", "
        + vastaanottoTila
        + ", "
        + maksuvelvollisuus
        + ", "
        + maksuntila
        + ", "
        + julkaistaankoTiedot
        + ", "
        + asiointikieli
        + ", "
        + puhelinnumero
        + ", "
        + osoite
        + ", "
        + postinumero
        + ", "
        + postitoimipaikka
        + ", "
        + asuinmaa
        + ", "
        + kansalaisuus
        + ", "
        + kotikunta
        + ", "
        + ErillishakuDataRivi.getTotuusarvoString(toisenAsteenSuoritus)
        + ", "
        + toisenAsteenSuoritusmaa
        + ", "
        + kutsumanimi
        + ", "
        + syntymapaikka
        + ", "
        + passinNumero
        + ", "
        + idTunnus
        + ", "
        + kaupunkiJaMaa;
  }

  public ErillishakuRivi withAidinkieli(String aidinkieli) {
    return ErillishakuRiviBuilder.fromRivi(this).aidinkieli(aidinkieli).build();
  }

  public ErillishakuRivi withHakemusOid(String hakemusOid) {
    return ErillishakuRiviBuilder.fromRivi(this).hakemusOid(hakemusOid).build();
  }

  public ErillishakuRivi withHakemusAndPersonOid(String hakemusOid, String personOid) {
    return ErillishakuRiviBuilder.fromRivi(this)
        .hakemusOid(hakemusOid)
        .personOid(personOid)
        .build();
  }

  public HenkiloCreateDTO toHenkiloCreateDTO(String kansalaisuus) {
    return new HenkiloCreateDTO(
        getAidinkieli(),
        Sukupuoli.toHenkiloString(getSukupuoli()),
        getEtunimi(),
        getSukunimi(),
        getHenkilotunnus(),
        formatSyntymaAikaAsString(),
        getPersonOid(),
        HenkiloTyyppi.OPPIJA,
        getAsiointikieli(),
        kansalaisuus);
  }

  public Date formatSyntymaAikaAsDate() {
    final String syntymaAika = getSyntymaAika();
    try {
      String s = trimToNull(syntymaAika);
      if (s == null) {
        return null;
      }

      LocalDate localDate = LocalDate.parse(s, SYNTYMAAIKAFORMAT);
      return Date.from(localDate.atStartOfDay().atZone(ZoneId.systemDefault()).toInstant());
    } catch (Exception e) {
      LOG.error("Syntymäaikaa {} ei voitu parsia muodossa dd.MM.yyyy", syntymaAika);
      return null;
    }
  }

  private String formatSyntymaAikaAsString() {
    final String syntymaAika = getSyntymaAika();
    try {
      String s = trimToNull(syntymaAika);
      if (s == null) {
        return null;
      }

      LocalDate localDate = LocalDate.parse(s, SYNTYMAAIKAFORMAT);
      return localDate.format(SYNTYMAAIKAFORMAT_JSON);
    } catch (Exception e) {
      LOG.error("Syntymäaikaa {} ei voitu parsia muodossa dd.MM.yyyy", syntymaAika);
      return null;
    }
  }

  public boolean isKesken() {
    return "KESKEN".equalsIgnoreCase(hakemuksenTila) || StringUtils.isBlank(hakemuksenTila);
  }

  public boolean isPoistetaankoRivi() {
    return poistetaankoRivi;
  }

  public String getMaksuvelvollisuus() {
    return maksuvelvollisuus;
  }

  public Maksuntila getMaksuntila() {
    return maksuntila;
  }

  public String getKutsumanimi() {
    return kutsumanimi;
  }

  public String getSyntymapaikka() {
    return syntymapaikka;
  }

  public String getPassinNumero() {
    return passinNumero;
  }

  public String getIdTunnus() {
    return idTunnus;
  }

  public String getKaupunkiJaMaa() {
    return kaupunkiJaMaa;
  }
}
