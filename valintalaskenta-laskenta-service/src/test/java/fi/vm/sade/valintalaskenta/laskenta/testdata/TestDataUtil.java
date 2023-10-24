package fi.vm.sade.valintalaskenta.laskenta.testdata;

import fi.vm.sade.service.valintaperusteet.dto.*;
import fi.vm.sade.service.valintaperusteet.dto.model.Funktionimi;
import fi.vm.sade.service.valintaperusteet.dto.model.Koekutsu;
import fi.vm.sade.service.valintaperusteet.dto.model.ValinnanVaiheTyyppi;
import fi.vm.sade.valintalaskenta.domain.dto.*;
import fi.vm.sade.valintalaskenta.domain.dto.AvainArvoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.Tasasijasaanto;
import fi.vm.sade.valintalaskenta.domain.valinta.*;
import fi.vm.sade.valintalaskenta.domain.valintakoe.*;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.impl.util.HakukohdeValintakoeData;
import org.jetbrains.annotations.NotNull;

import java.math.BigDecimal;
import java.util.*;

import static fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila.HYLATTY;
import static fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA;

/** User: wuoti Date: 6.5.2013 Time: 12.57 */
public abstract class TestDataUtil {
  public static HakemusDTO luoHakemus(String hakuOid, String hakemusOid, String hakijaOid) {
    HakemusDTO hakemus = new HakemusDTO();
    hakemus.setHakemusoid(hakemusOid);
    hakemus.setHakijaOid(hakijaOid);
    hakemus.setHakuoid(hakuOid);
    List<AvainArvoDTO> avaimet = new LinkedList<>();
    AvainArvoDTO avain = new AvainArvoDTO();
    avain.setAvain("hakukohdeKutsunKohde2");
    avain.setArvo("hakukohdeOid2");
    avaimet.add(avain);
    hakemus.setAvaimet(avaimet);
    return hakemus;
  }

  public static HakemusDTO luoHakemus(
      String hakuOid, String hakemusOid, String hakijaOid, String... hakutoiveet) {
    HakemusDTO hakemus = luoHakemus(hakuOid, hakemusOid, hakijaOid);
    int i = 1;
    for (String hakutoive : hakutoiveet) {
      HakukohdeDTO toive = new HakukohdeDTO();
      toive.setOid(hakutoive);
      toive.setPrioriteetti(i);
      hakemus.getHakukohteet().add(toive);
      ++i;
    }
    return hakemus;
  }

  public static ValintaperusteetDTO luoValintaperusteet(String hakuOid, String hakukohdeOid) {
    ValintaperusteetDTO perusteet = new ValintaperusteetDTO();
    perusteet.setHakukohdeOid(hakukohdeOid);
    perusteet.setHakuOid(hakuOid);
    return perusteet;
  }

  public static ValintaperusteetDTO luoValintaperusteetJaValintakoeValinnanvaihe(
      String hakuOid,
      String hakukohdeOid,
      String valinnanVaiheOid,
      int valinnanVaiheJarjestysluku,
      String... valintakoeTunnisteet) {
    ValintaperusteetDTO perusteet = luoValintaperusteet(hakuOid, hakukohdeOid);
    perusteet.setValinnanVaihe(
        luoValintakoeValinnanVaihe(
            valinnanVaiheOid,
            valinnanVaiheJarjestysluku,
            Koekutsu.YLIN_TOIVE,
            "kutsunKohdeAvain",
            valintakoeTunnisteet));
    return perusteet;
  }

  public static ValintaperusteetDTO luoValintaperusteetJaTavallinenValinnanvaihe(
      String hakuOid,
      String hakukohdeOid,
      String valinnanVaiheOid,
      int valinnanVaiheJarjestysluku) {
    ValintaperusteetDTO perusteet = luoValintaperusteet(hakuOid, hakukohdeOid);
    perusteet.setTarjoajaOid("tarjoaja");
    perusteet.setValinnanVaihe(
        luoTavallinenValinnanvaihe(valinnanVaiheOid, valinnanVaiheJarjestysluku));
    return perusteet;
  }

  public static ValintaperusteetValinnanVaiheDTO luoTavallinenValinnanvaihe(
      String valinnanVaiheOid, int valinnanVaiheJarjestysluku) {
    ValintaperusteetValinnanVaiheDTO vaihe = new ValintaperusteetValinnanVaiheDTO();
    vaihe.setValinnanVaiheOid(valinnanVaiheOid);
    vaihe.setValinnanVaiheJarjestysluku(valinnanVaiheJarjestysluku);
    vaihe.setValinnanVaiheTyyppi(ValinnanVaiheTyyppi.TAVALLINEN);
    return vaihe;
  }

  public static ValintatapajonoJarjestyskriteereillaDTO luoValintatapajono(
      String valintatapajonoOid,
      int prioriteetti,
      int aloituspaikat,
      ValintaperusteetJarjestyskriteeriDTO... jarjestyskriteerit) {
    ValintatapajonoJarjestyskriteereillaDTO jono = new ValintatapajonoJarjestyskriteereillaDTO();
    jono.setOid(valintatapajonoOid);
    jono.setAloituspaikat(aloituspaikat);
    jono.setNimi(valintatapajonoOid);
    jono.setPrioriteetti(prioriteetti);
    jono.setSiirretaanSijoitteluun(true);
    jono.setTasasijasaanto(Tasasijasaanto.ARVONTA.name());
    jono.getJarjestyskriteerit().addAll(Arrays.asList(jarjestyskriteerit));
    return jono;
  }

  public static ValintaperusteetJarjestyskriteeriDTO luoJarjestyskriteeri(
      ValintaperusteetFunktiokutsuDTO funktiokutsu, int prioriteetti) {
    ValintaperusteetJarjestyskriteeriDTO jk = new ValintaperusteetJarjestyskriteeriDTO();
    jk.setFunktiokutsu(funktiokutsu);
    jk.setPrioriteetti(prioriteetti);
    return jk;
  }

  public static ValintaperusteetDTO luoValintaperusteetJaValintakoeValinnanVaihe(
      String hakuOid,
      String hakukohdeOid,
      String valinnanVaiheOid,
      int valinnanVaiheJarjestysluku,
      Map<String, FunktiokutsuDTO> valintakokeetJaKaavat,
      Koekutsu kutsunKohde,
      String kutsunKohdeAvain) {
    ValintaperusteetDTO perusteet = luoValintaperusteet(hakuOid, hakukohdeOid);
    perusteet.setValinnanVaihe(
        luoValinnanVaihe(
            valinnanVaiheOid,
            valinnanVaiheJarjestysluku,
            valintakokeetJaKaavat,
            kutsunKohde,
            kutsunKohdeAvain));
    return perusteet;
  }

  public static ValintaperusteetValinnanVaiheDTO luoValintakoeValinnanVaihe(
      String valinnanVaiheOid, int jarjestysluku) {
    ValintaperusteetValinnanVaiheDTO vaihe = new ValintaperusteetValinnanVaiheDTO();
    vaihe.setValinnanVaiheOid(valinnanVaiheOid);
    vaihe.setValinnanVaiheJarjestysluku(jarjestysluku);
    vaihe.setValinnanVaiheTyyppi(ValinnanVaiheTyyppi.VALINTAKOE);
    return vaihe;
  }

  private static ValintaperusteetValinnanVaiheDTO luoValinnanVaihe(
      String valinnanVaiheOid,
      int valinnanVaiheJarjestysluku,
      Map<String, FunktiokutsuDTO> valintakokeetJaKaavat,
      Koekutsu kutsunKohde,
      String kutsunKohdeAvain) {
    ValintaperusteetValinnanVaiheDTO vaihe =
        luoValintakoeValinnanVaihe(valinnanVaiheOid, valinnanVaiheJarjestysluku);
    for (Map.Entry<String, FunktiokutsuDTO> e : valintakokeetJaKaavat.entrySet()) {
      ValintakoeDTO koe = luoValintakoe(e.getKey(), e.getKey(), kutsunKohde, kutsunKohdeAvain);
      koe.setFunktiokutsu(e.getValue());
      vaihe.getValintakoe().add(koe);
    }
    return vaihe;
  }

  public static ValintaperusteetValinnanVaiheDTO luoValintakoeValinnanVaihe(
      String valinnanVaiheOid,
      int jarjestysluku,
      Koekutsu kutsunKohde,
      String kutsunKohdeAvain,
      String... valintakoeTunnisteet) {
    ValintaperusteetValinnanVaiheDTO vaihe =
        luoValintakoeValinnanVaihe(valinnanVaiheOid, jarjestysluku);
    for (String tunniste : valintakoeTunnisteet) {
      vaihe.getValintakoe().add(luoValintakoe(tunniste, tunniste, kutsunKohde, kutsunKohdeAvain));
    }
    return vaihe;
  }

  public static ValintakoeDTO luoValintakoe(
      String valintakoeOid, String tunniste, Koekutsu kutsunKohde, String kutsunKohdeAvain) {
    ValintakoeDTO koe = new ValintakoeDTO();
    koe.setFunktiokutsu(luoFunktioKutsu());
    koe.setTunniste(tunniste);
    koe.setOid(UUID.randomUUID().toString());
    koe.setLahetetaankoKoekutsut(true);
    koe.setAktiivinen(true);
    koe.setKutsutaankoKaikki(false);
    koe.setKutsunKohde(kutsunKohde);
    koe.setKutsunKohdeAvain(kutsunKohdeAvain);
    return koe;
  }

  public static FunktiokutsuDTO luoFunktioKutsu() {
    FunktiokutsuDTO kutsu = new FunktiokutsuDTO();
    kutsu.setFunktionimi(Funktionimi.LUKUARVO);
    SyoteparametriDTO param = new SyoteparametriDTO();
    param.setArvo("5");
    param.setAvain("luku");
    kutsu.setSyoteparametrit(Set.of(param));
    return kutsu;
  }

  public static HakukohdeDTO luoHakukohdeDTO(String hakukohdeOid, int prioriteetti) {
    HakukohdeDTO hakukohde = new HakukohdeDTO();
    hakukohde.setOid(hakukohdeOid);
    hakukohde.setPrioriteetti(prioriteetti);
    return hakukohde;
  }

  public static HakukohdeValintakoeData luoHakukohdeValintakoeData(
      String hakukohdeOid, Osallistuminen osallistuminen, String valintakoeTunniste) {
    HakukohdeValintakoeData koe = new HakukohdeValintakoeData();
    koe.setHakukohdeOid(hakukohdeOid);
    koe.setKutsunKohde(Koekutsu.YLIN_TOIVE);
    OsallistuminenTulos osallistuminenTulos = new OsallistuminenTulos();
    osallistuminenTulos.setOsallistuminen(osallistuminen);
    koe.setOsallistuminenTulos(osallistuminenTulos);
    koe.setValintakoeTunniste(valintakoeTunniste);
    return koe;
  }

  public static Jarjestyskriteeritulos luoJarjestyskriteeritulosEntity(
      double arvo, int prioriteetti, JarjestyskriteerituloksenTila tila) {
    Jarjestyskriteeritulos tulos = new Jarjestyskriteeritulos();
    tulos.setArvo(new BigDecimal(arvo));
    tulos.setPrioriteetti(prioriteetti);
    tulos.setTila(tila);
    tulos.setKuvausFI("");
    tulos.setKuvausEN("");
    tulos.setKuvausSV("");
    return tulos;
  }

  public static SyotettyArvo luoSyotettyArvo(
      String laskennallinenArvo, String arvo, String tunniste, Osallistuminen osallistuminen) {
    SyotettyArvo syotettyArvo = new SyotettyArvo();
    syotettyArvo.setLaskennallinenArvo(laskennallinenArvo);
    syotettyArvo.setTunniste(tunniste);
    syotettyArvo.setOsallistuminen(osallistuminen.name());
    syotettyArvo.setArvo(arvo);
    return syotettyArvo;
  }

  public static Jonosija luoJonosijaEntity(
      String etunimi,
      String sukunimi,
      String hakemusOid,
      int hakutoivePrioriteetti,
      boolean harkinnanvarainen,
      List<Jarjestyskriteeritulos> tulokset) {
    return luoJonosijaEntity(
        etunimi,
        sukunimi,
        hakemusOid,
        hakutoivePrioriteetti,
        harkinnanvarainen,
        tulokset,
        new ArrayList<>());
  }

  public static Jonosija luoJonosijaEntity(
      String etunimi,
      String sukunimi,
      String hakemusOid,
      int hakutoivePrioriteetti,
      boolean harkinnanvarainen,
      List<Jarjestyskriteeritulos> tulokset,
      List<SyotettyArvo> arvot) {
    Jonosija jonosija = new Jonosija();
    jonosija.setEtunimi(etunimi);
    jonosija.setSukunimi(sukunimi);
    jonosija.setHakemusOid(hakemusOid);
    jonosija.setHakutoiveprioriteetti(hakutoivePrioriteetti);
    jonosija.setHarkinnanvarainen(harkinnanvarainen);
    jonosija.setJarjestyskriteeritulokset(tulokset);
    SyotettyArvoContainer container = new SyotettyArvoContainer();
    container.syotetytArvot.addAll(arvot);
    jonosija.setSyotetytArvot(container);
    return jonosija;
  }

  public static Valintatapajono luoValintatapaJonoEntity(
      int aloituspaikat,
      Set<Jonosija> jonosijat,
      String nimi,
      int prioriteetti,
      Tasasijasaanto saanto,
      String valintatapajonoOid) {
    Valintatapajono jono = new Valintatapajono();
    jono.setAloituspaikat(aloituspaikat);
    jono.setJonosijat(jonosijat);
    jono.setNimi(nimi);
    jono.setPrioriteetti(prioriteetti);
    jono.setTasasijasaanto(saanto);
    jono.setValintatapajonoOid(valintatapajonoOid);
    return jono;
  }

  public static Valinnanvaihe luoValinnanvaiheEntity(
      String hakuOid,
      String hakukohdeOid,
      int jarjestysnro,
      String valinnanvaiheOid,
      List<Valintatapajono> jonot) {
    Valinnanvaihe vaihe = new Valinnanvaihe();
    vaihe.setHakuOid(hakuOid);
    vaihe.setHakukohdeOid(hakukohdeOid);
    vaihe.setJarjestysnumero(jarjestysnro);
    vaihe.setValinnanVaiheOid(valinnanvaiheOid);
    vaihe.setValintatapajono(jonot);
    vaihe.setTarjoajaOid("tarjoaja");
    return vaihe;
  }

  public static Hakutoive luoHakutoiveEntity(
      String hakukohdeOid, Set<ValintakoeValinnanvaihe> valintakoeValinnanvaiheet) {
    Hakutoive toive = new Hakutoive();
    toive.setHakukohdeOid(hakukohdeOid);
    toive.setValintakoeValinnanvaiheet(valintakoeValinnanvaiheet);
    return toive;
  }
  ;

  public static ValintakoeValinnanvaihe luoValintakoeValinnanvaiheEntity(
      int jarjestysluku, String valinnanvaiheOid, List<Valintakoe> kokeet) {
    ValintakoeValinnanvaihe vkv = new ValintakoeValinnanvaihe();
    vkv.setValinnanVaiheJarjestysluku(jarjestysluku);
    vkv.setValinnanvaiheOid(valinnanvaiheOid);
    vkv.setValintakokeet(kokeet);
    return vkv;
  }

  public static Valintakoe luoValintakoeEntity(
      String valintakoeOid,
      String tunniste,
      Osallistuminen osallistuminen,
      Boolean laskentaTulos,
      String laskentatila) {
    Valintakoe valintakoe = new Valintakoe();
    valintakoe.setValintakoeOid(valintakoeOid);
    valintakoe.setValintakoeTunniste(tunniste);
    valintakoe.setOsallistuminen(osallistuminen);
    valintakoe.setLaskentaTulos(laskentaTulos);
    valintakoe.setLaskentaTila(laskentatila);
    valintakoe.setAktiivinen(true);
    valintakoe.setNimi(tunniste);
    valintakoe.setLahetetaankoKoekutsut(false);
    return valintakoe;
  }

  public static ValintakoeOsallistuminen luoValintakoeOsallistuminen(
      String hakuOid, String hakijaOid, String hakemusOid, Set<Hakutoive> toiveet) {
    ValintakoeOsallistuminen osallistuminen = new ValintakoeOsallistuminen();
    osallistuminen.setHakuOid(hakuOid);
    osallistuminen.setHakijaOid(hakijaOid);
    osallistuminen.setHakemusOid(hakemusOid);
    osallistuminen.setHakutoiveet(toiveet);
    return osallistuminen;
  }

  public static ValintakoeOsallistuminen luoValintakoeOsallistuminen(
      String hakuOid, String hakijaOid, String hakemusOid) {
    return luoValintakoeOsallistuminen(hakuOid, hakijaOid, hakemusOid, new HashSet<>());
  }

  @NotNull
  public static Jonosija luoHylattyJonosijaValisijoittelussa(
    String hakemusOid, String etunimi, String sukunimi, String hakijaOid) {
    Jonosija sija =
      TestDataUtil.luoJonosijaEntity(
        etunimi,
        sukunimi,
        hakemusOid,
        2,
        false,
        Arrays.asList(
          TestDataUtil.luoJarjestyskriteeritulosEntity(12.0, 0, HYLATTY),
          TestDataUtil.luoJarjestyskriteeritulosEntity(2.0, 1, HYVAKSYTTAVISSA),
          TestDataUtil.luoJarjestyskriteeritulosEntity(6.82, 2, HYVAKSYTTAVISSA)));

    sija.setHylattyValisijoittelussa(true);
    sija.setHakijaOid(hakijaOid);
    return sija;
  }

  @NotNull
  public static Jonosija luoHylattyJonosija(
    String hakemusOid, String etunimi, String sukunimi, String hakijaOid) {
    Jonosija sija =
      TestDataUtil.luoJonosijaEntity(
        etunimi,
        sukunimi,
        hakemusOid,
        1,
        false,
        List.of(TestDataUtil.luoJarjestyskriteeritulosEntity(0, 0, HYLATTY)));
    sija.getJarjestyskriteeritulokset()
      .get(0)
      .setKuvausFI("Hakemus hyväksyttiin korkeammalle hakutoiveelle");
    sija.setHakijaOid(hakijaOid);
    return sija;
  }

  public static ValintakoeOsallistuminen luoValintakoeOsallistuminen(
    String hakemusOid,
    String hakukohdeOid,
    String hakuOid,
    String hakijaOid,
    boolean osallistumisetHylatty) {
    return luoValintakoeOsallistuminen(
        hakuOid,
        hakijaOid,
        hakemusOid,
        Set.of(
          TestDataUtil.luoHakutoiveEntity(
            "1.2.246.562.20.75182408387",
            Set.of(
              TestDataUtil.luoValintakoeValinnanvaiheEntity(
                2,
                "vv2" + hakijaOid,
                Arrays.asList(
                  TestDataUtil.luoValintakoeEntity(
                    "1413283036386-2122258152875806456",
                    "SOTE1_kaikkiosiot",
                    Osallistuminen.EI_OSALLISTU,
                    null,
                    "HYLATTY"),
                  TestDataUtil.luoValintakoeEntity(
                    "14132830363881382138007672413981",
                    "SOTEKOE_VK_RYHMA1",
                    Osallistuminen.EI_OSALLISTU,
                    null,
                    "HYLATTY"),
                  TestDataUtil.luoValintakoeEntity(
                    "1413373250365-957582059449313229",
                    "kielikoe_amk_fi",
                    Osallistuminen.EI_OSALLISTU,
                    null,
                    "HYLATTY"))))),
          TestDataUtil.luoHakutoiveEntity(
            "1.2.246.562.20.68508673735",
            Set.of(
              TestDataUtil.luoValintakoeValinnanvaiheEntity(
                2,
                "vv3" + hakijaOid,
                Arrays.asList(
                  TestDataUtil.luoValintakoeEntity(
                    "1413283096804-4591090746315355326",
                    "SOTE1_kaikkiosiot",
                    Osallistuminen.EI_OSALLISTU,
                    null,
                    "HYLATTY"),
                  TestDataUtil.luoValintakoeEntity(
                    "1413283096806-1884654158026567207",
                    "SOTEKOE_VK_RYHMA1",
                    Osallistuminen.EI_OSALLISTU,
                    null,
                    "HYLATTY"),
                  TestDataUtil.luoValintakoeEntity(
                    "1413373326095-2455808773394676892",
                    "kielikoe_amk_fi",
                    Osallistuminen.EI_OSALLISTU,
                    null,
                    "HYLATTY"))))),
          TestDataUtil.luoHakutoiveEntity(
            hakukohdeOid,
            Set.of(
              TestDataUtil.luoValintakoeValinnanvaiheEntity(
                2,
                "vv5" + hakijaOid,
                Arrays.asList(
                  TestDataUtil.luoValintakoeEntity(
                    "14132865785323928261359415431364",
                    "SOTE1_kaikkiosiot",
                    Osallistuminen.EI_OSALLISTU,
                    null,
                    "HYLATTY"),
                  TestDataUtil.luoValintakoeEntity(
                    "1413286578535-5213085081652469216",
                    "SOTEKOE_VK_RYHMA1",
                    Osallistuminen.EI_OSALLISTU,
                    null,
                    "HYLATTY"),
                  TestDataUtil.luoValintakoeEntity(
                    "1413286578535-5213085081652461564",
                    "KOHTEEN_SPESIFI_KOE_BUG-1564",
                    Osallistuminen.OSALLISTUU,
                    null,
                    osallistumisetHylatty ? "HYLATTY" : "HYVAKSYTTAVISSA"))))),
          TestDataUtil.luoHakutoiveEntity(
            "1.2.246.562.20.93258129167",
            Set.of(
              TestDataUtil.luoValintakoeValinnanvaiheEntity(
                1,
                "vv6" + hakijaOid,
                Arrays.asList(
                  TestDataUtil.luoValintakoeEntity(
                    "1413286979019-1352529564096963601",
                    "SOTE1_kaikkiosiot",
                    Osallistuminen.EI_OSALLISTU,
                    null,
                    "HYLATTY"),
                  TestDataUtil.luoValintakoeEntity(
                    "1413286979022-1524554277697202959",
                    "SOTEKOE_VK_RYHMA1",
                    Osallistuminen.EI_OSALLISTU,
                    null,
                    "HYLATTY"))))),
          TestDataUtil.luoHakutoiveEntity(
            "1.2.246.562.20.64586301414",
            Set.of(
              TestDataUtil.luoValintakoeValinnanvaiheEntity(
                2,
                "vv7" + hakijaOid,
                Arrays.asList(
                  TestDataUtil.luoValintakoeEntity(
                    "1413284065430-8581021134442572546",
                    "SOTE1_kaikkiosiot",
                    Osallistuminen.EI_OSALLISTU,
                    null,
                    "HYLATTY"),
                  TestDataUtil.luoValintakoeEntity(
                    "14132840654327607869284614200980",
                    "SOTEKOE_VK_RYHMA1",
                    Osallistuminen.EI_OSALLISTU,
                    null,
                    "HYLATTY"))))),
          TestDataUtil.luoHakutoiveEntity(
            "1.2.246.562.20.70883151881",
            Set.of(
              TestDataUtil.luoValintakoeValinnanvaiheEntity(
                2,
                "vv8" + hakijaOid,
                Arrays.asList(
                  TestDataUtil.luoValintakoeEntity(
                    "1413284033343-6883461063685526158",
                    "SOTE1_kaikkiosiot",
                    Osallistuminen.OSALLISTUU,
                    true,
                    osallistumisetHylatty ? "HYLATTY" : "HYVAKSYTTAVISSA"),
                  TestDataUtil.luoValintakoeEntity(
                    "1413284033345-7502478179236072968",
                    "SOTEKOE_VK_RYHMA1",
                    Osallistuminen.OSALLISTUU,
                    true,
                    osallistumisetHylatty ? "HYLATTY" : "HYVAKSYTTAVISSA")))))));
  }
}
