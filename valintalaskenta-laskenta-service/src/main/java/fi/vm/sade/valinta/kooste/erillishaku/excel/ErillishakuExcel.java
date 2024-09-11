package fi.vm.sade.valinta.kooste.erillishaku.excel;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.Lists;
import fi.vm.sade.sijoittelu.domain.HakemuksenTila;
import fi.vm.sade.valinta.kooste.erillishaku.dto.Hakutyyppi;
import fi.vm.sade.valinta.kooste.excel.Excel;
import fi.vm.sade.valinta.kooste.excel.Rivi;
import fi.vm.sade.valinta.kooste.excel.arvo.Arvo;
import fi.vm.sade.valinta.kooste.excel.arvo.BooleanArvo;
import fi.vm.sade.valinta.kooste.excel.arvo.MonivalintaArvo;
import fi.vm.sade.valinta.kooste.excel.arvo.TekstiArvo;
import fi.vm.sade.valinta.kooste.external.resource.koodisto.KoodistoCachedAsyncResource;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ErillishakuExcel {
  private static final Logger LOG = LoggerFactory.getLogger(ErillishakuExcel.class);
  public static final String HEADER_HYVAKSYMISKIRJE_LAHETETTY = "Hyväksymiskirje lähetetty";
  private final Excel excel;

  public ErillishakuExcel(
      Hakutyyppi tyyppi,
      ErillishakuRiviKuuntelija kuuntelija,
      KoodistoCachedAsyncResource koodistoCachedAsyncResource) {
    this(tyyppi, "", "", "", Collections.emptyList(), kuuntelija, koodistoCachedAsyncResource);
  }

  public ErillishakuExcel(
      Hakutyyppi tyyppi,
      String hakuNimi,
      String hakukohdeNimi,
      String tarjoajaNimi,
      List<ErillishakuRivi> erillishakurivit,
      KoodistoCachedAsyncResource koodistoCachedAsyncResource) {
    this(
        tyyppi,
        hakuNimi,
        hakukohdeNimi,
        tarjoajaNimi,
        erillishakurivit,
        rivi -> {},
        koodistoCachedAsyncResource);
  }

  ErillishakuExcel(
      final Hakutyyppi tyyppi,
      String hakuNimi,
      String hakukohdeNimi,
      String tarjoajaNimi,
      List<ErillishakuRivi> erillishakurivit,
      ErillishakuRiviKuuntelija kuuntelija,
      KoodistoCachedAsyncResource koodistoCachedAsyncResource) {
    erillishakurivit = Lists.newArrayList(erillishakurivit);
    List<Rivi> rivit = Lists.newArrayList();
    Collection<Collection<Arvo>> esittelyt = Lists.newArrayList();
    esittelyt.add(Collections.singletonList(new TekstiArvo(hakuNimi, true, false, 4)));
    esittelyt.add(Collections.singletonList(new TekstiArvo(hakukohdeNimi, true, false, 4)));
    esittelyt.add(Collections.singletonList(new TekstiArvo(tarjoajaNimi, true, false, 4)));
    esittelyt.add(Collections.singletonList(new TekstiArvo(StringUtils.EMPTY)));

    ImmutableList.Builder<Arvo> builder = ImmutableList.builder();
    builder.add(new TekstiArvo("Sukunimi"));
    builder.add(new TekstiArvo("Etunimi"));
    builder.add(new TekstiArvo("Henkilötunnus"));
    builder.add(new TekstiArvo("Sähköposti"));
    builder.add(new TekstiArvo("Syntymäaika"));
    builder.add(new TekstiArvo("Sukupuoli"));
    builder.add(new TekstiArvo("Oppijanumero"));
    builder.add(new TekstiArvo("Äidinkieli"));
    builder.add(new TekstiArvo("Hakemuksentila"));
    if (tyyppi == Hakutyyppi.KORKEAKOULU) {
      builder.add(new TekstiArvo("Ehdollinen valinta"));
      builder.add(new TekstiArvo("Hyväksymisen ehto"));
      builder.add(new TekstiArvo("Hyväksymisen ehto FI"));
      builder.add(new TekstiArvo("Hyväksymisen ehto SV"));
      builder.add(new TekstiArvo("Hyväksymisen ehto EN"));
    }
    builder.add(new TekstiArvo(HEADER_HYVAKSYMISKIRJE_LAHETETTY));
    builder.add(new TekstiArvo("Vastaanottotila"));
    builder.add(new TekstiArvo("Ilmoittautumistila"));
    if (tyyppi == Hakutyyppi.KORKEAKOULU) {
      builder.add(new TekstiArvo("Maksuvelvollisuus"));
      builder.add(new TekstiArvo("Maksun tila"));
    }
    builder.add(new TekstiArvo("Julkaistavissa"));
    builder.add(new TekstiArvo("Asiointikieli"));
    builder.add(new TekstiArvo("Puhelinnumero"));
    builder.add(new TekstiArvo("Osoite"));
    builder.add(new TekstiArvo("Postinumero"));
    builder.add(new TekstiArvo("Postitoimipaikka"));
    builder.add(new TekstiArvo("Asuinmaa"));
    builder.add(new TekstiArvo("Kansalaisuus"));
    builder.add(new TekstiArvo("Kotikunta"));
    if (tyyppi == Hakutyyppi.KORKEAKOULU) {
      builder.add(new TekstiArvo("Toisen asteen pohjakoulutus suoritettu"));
      builder.add(new TekstiArvo("Toisen asteen pohjakoulutuksen suoritusmaa"));
    }
    builder.add(new TekstiArvo("Kutsumanimi"));
    builder.add(new TekstiArvo("Syntymäpaikka"));
    builder.add(new TekstiArvo("Passin numero"));
    builder.add(new TekstiArvo("Kansallinen ID-tunnus"));
    builder.add(new TekstiArvo("Kaupunki ja maa"));
    builder.add(new TekstiArvo("Hakemus-oid"));
    esittelyt.add(builder.build());

    Collections.sort(
        erillishakurivit,
        (h1, h2) -> {
          ErillishakuRivi e1 = Optional.ofNullable(h1).orElse(new ErillishakuRiviBuilder().build());
          ErillishakuRivi e2 = Optional.ofNullable(h2).orElse(new ErillishakuRiviBuilder().build());
          String s1 = Optional.ofNullable(e1.getSukunimi()).orElse(StringUtils.EMPTY).toUpperCase();
          String s2 = Optional.ofNullable(e2.getSukunimi()).orElse(StringUtils.EMPTY).toUpperCase();
          int i = s1.compareTo(s2);
          if (i != 0) {
            return i;
          } else {
            String ee1 =
                Optional.ofNullable(e1.getEtunimi()).orElse(StringUtils.EMPTY).toUpperCase();
            String ee2 =
                Optional.ofNullable(e2.getEtunimi()).orElse(StringUtils.EMPTY).toUpperCase();
            return ee1.compareTo(ee2);
          }
        });
    ErillishakuDataRivi dataRivit =
        new ErillishakuDataRivi(
            tyyppi,
            kuuntelija,
            Stream.concat(
                    esittelyt.stream(),
                    arvoRivit(erillishakurivit).map(luoArvot(tyyppi, koodistoCachedAsyncResource)))
                .collect(Collectors.toList()));

    rivit.add(dataRivit);
    this.excel = new Excel("Erillishaku", rivit);
  }

  private Stream<ErillishakuRivi> arvoRivit(List<ErillishakuRivi> erillishakurivit) {
    if (erillishakurivit.isEmpty()) {
      return ImmutableList.of(
          new ErillishakuRiviBuilder()
              .sukunimi("Esimerkki")
              .etunimi("Rivi")
              .henkilotunnus("123456-7890")
              .sahkoposti("esimerkki.rivi@example.com")
              .syntymaAika("01.01.1901")
              .sukupuoli(Sukupuoli.NAINEN)
              .personOid("")
              .aidinkieli("FI")
              .hakemuksenTila(HakemuksenTila.HYVAKSYTTY.toString())
              .ehdollisestiHyvaksyttavissa(false)
              .hyvaksymiskirjeLahetetty(new Date())
              .vastaanottoTila("KESKEN")
              .ilmoittautumisTila("EI_TEHTY")
              .julkaistaankoTiedot(false)
              .poistetaankoRivi(false)
              .asiointikieli("FI")
              .puhelinnumero("040123456789")
              .osoite("Esimerkkitie 2")
              .postinumero("00100")
              .postitoimipaikka("HELSINKI")
              .asuinmaa("FIN")
              .kansalaisuus("FIN")
              .kotikunta("HELSINKI")
              .toisenAsteenSuoritus(true)
              .toisenAsteenSuoritusmaa("FIN")
              .maksuvelvollisuus(Maksuvelvollisuus.NOT_CHECKED)
              .kutsumanimi("Rivi")
              .syntymapaikka("Helsinki, Suomi")
              .passinNumero("4321")
              .idTunnus("1234")
              .kaupunkiJaMaa("Helsinki, Suomi")
              .hakemusOid("")
              .build())
          .stream();
    } else {
      return erillishakurivit.stream();
    }
  }

  private Function<ErillishakuRivi, Collection<Arvo>> luoArvot(
      Hakutyyppi tyyppi, KoodistoCachedAsyncResource koodistoCachedAsyncResource) {
    return rivi -> {
      Collection<Arvo> a = Lists.newArrayList();
      a.add(new TekstiArvo(rivi.getSukunimi(), true, true));
      a.add(new TekstiArvo(rivi.getEtunimi(), true, true));
      a.add(new TekstiArvo(rivi.getHenkilotunnus(), true, true));
      a.add(new TekstiArvo(rivi.getSahkoposti(), true, true));
      a.add(new TekstiArvo(rivi.getSyntymaAika(), true, true));
      a.add(
          new MonivalintaArvo(
              rivi.getSukupuoli().toString(), ErillishakuDataRivi.SUKUPUOLEN_ARVOT));
      a.add(new TekstiArvo(rivi.getPersonOid(), true, true));
      a.add(ErillishakuDataRivi.aidinkieli(rivi.getAidinkieli(), koodistoCachedAsyncResource));
      a.add(ErillishakuDataRivi.hakemuksenTila(tyyppi, rivi.getHakemuksenTila()));
      if (tyyppi == Hakutyyppi.KORKEAKOULU) {
        a.add(
            new BooleanArvo(
                rivi.getEhdollisestiHyvaksyttavissa(),
                ErillishakuDataRivi.TOTUUSARVO,
                ErillishakuDataRivi.TOSI,
                ErillishakuDataRivi.EPATOSI,
                ErillishakuDataRivi.EPATOSI));

        if (rivi.getEhdollisestiHyvaksyttavissa() == true) {
          a.add(
              ErillishakuDataRivi.ehdollisenHyvaksymisenEhtoKoodi(
                  rivi.getEhdollisenHyvaksymisenEhtoKoodi(), koodistoCachedAsyncResource));
          if (StringUtils.equals(rivi.getEhdollisenHyvaksymisenEhtoKoodi(), "muu")) {
            a.add(new TekstiArvo(rivi.getEhdollisenHyvaksymisenEhtoFI(), true, true));
            a.add(new TekstiArvo(rivi.getEhdollisenHyvaksymisenEhtoSV(), true, true));
            a.add(new TekstiArvo(rivi.getEhdollisenHyvaksymisenEhtoEN(), true, true));
          } else {
            a.add(new TekstiArvo("", true, true));
            a.add(new TekstiArvo("", true, true));
            a.add(new TekstiArvo("", true, true));
          }
        } else {
          a.add(
              ErillishakuDataRivi.ehdollisenHyvaksymisenEhtoKoodi("", koodistoCachedAsyncResource));
          a.add(new TekstiArvo("", true, true));
          a.add(new TekstiArvo("", true, true));
          a.add(new TekstiArvo("", true, true));
        }
      }
      a.add(
          new TekstiArvo(
              rivi.getHyvaksymiskirjeLahetetty() == null
                  ? ""
                  : ErillishakuDataRivi.LAHETETTYFORMAT.print(
                      rivi.getHyvaksymiskirjeLahetetty().getTime())));
      a.add(ErillishakuDataRivi.vastaanottoTila(tyyppi, rivi.getVastaanottoTila()));
      a.add(ErillishakuDataRivi.ilmoittautumisTila(rivi.getIlmoittautumisTila()));
      if (tyyppi == Hakutyyppi.KORKEAKOULU) {
        a.add(ErillishakuDataRivi.maksuvelvollisuus(rivi.getMaksuvelvollisuus()));
        a.add(
            ErillishakuDataRivi.maksuntila(
                rivi.getMaksuntila() != null ? rivi.getMaksuntila().toString() : ""));
      }
      a.add(ErillishakuDataRivi.julkaisuLupa(rivi.isJulkaistaankoTiedot()));
      a.add(ErillishakuDataRivi.asiointiKieli(rivi.getAsiointikieli()));
      a.add(new TekstiArvo(rivi.getPuhelinnumero(), true, true));
      a.add(new TekstiArvo(rivi.getOsoite(), true, true));
      a.add(new TekstiArvo(rivi.getPostinumero(), true, true));
      a.add(new TekstiArvo(rivi.getPostitoimipaikka(), true, true));
      a.add(new TekstiArvo(rivi.getAsuinmaa(), true, true));
      a.add(new TekstiArvo(rivi.getKansalaisuus(), true, true));
      a.add(new TekstiArvo(rivi.getKotikunta(), true, true));
      if (tyyppi == Hakutyyppi.KORKEAKOULU) {
        a.add(
            new BooleanArvo(
                rivi.getToisenAsteenSuoritus(),
                ErillishakuDataRivi.TOTUUSARVO,
                ErillishakuDataRivi.TOSI,
                ErillishakuDataRivi.EPATOSI,
                ""));
        a.add(new TekstiArvo(rivi.getToisenAsteenSuoritusmaa(), true, true));
      }
      a.add(new TekstiArvo(rivi.getKutsumanimi(), true, true));
      a.add(new TekstiArvo(rivi.getSyntymapaikka(), true, true));
      a.add(new TekstiArvo(rivi.getPassinNumero(), true, true));
      a.add(new TekstiArvo(rivi.getIdTunnus(), true, true));
      a.add(new TekstiArvo(rivi.getKaupunkiJaMaa(), true, true));
      a.add(new TekstiArvo(rivi.getHakemusOid(), true, true));
      return a;
    };
  }

  public Excel getExcel() {
    return excel;
  }
}
