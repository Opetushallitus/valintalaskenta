package fi.vm.sade.valintalaskenta.domain.testdata;

import static fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila.HYLATTY;
import static fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA;

import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.Tasasijasaanto;
import fi.vm.sade.valintalaskenta.domain.valinta.*;
import fi.vm.sade.valintalaskenta.domain.valintakoe.*;
import java.math.BigDecimal;
import java.util.*;
import jakarta.validation.constraints.NotNull;

public class TestEntityDataUtil {

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

  public static Jarjestyskriteeritulos luoJarjestyskriteeritulosEntityKuvauksella(
      double arvo, int prioriteetti, JarjestyskriteerituloksenTila tila, String kuvaus) {
    Jarjestyskriteeritulos tulos = new Jarjestyskriteeritulos();
    tulos.setArvo(new BigDecimal(arvo));
    tulos.setPrioriteetti(prioriteetti);
    tulos.setTila(tila);
    tulos.setKuvausFI(kuvaus);
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
      String hakemusOid,
      int hakutoivePrioriteetti,
      boolean harkinnanvarainen,
      List<Jarjestyskriteeritulos> tulokset) {
    return luoJonosijaEntity(
        hakemusOid, hakutoivePrioriteetti, harkinnanvarainen, tulokset, new ArrayList<>());
  }

  public static Jonosija luoJonosijaEntity(
      String hakemusOid,
      int hakutoivePrioriteetti,
      boolean harkinnanvarainen,
      List<Jarjestyskriteeritulos> tulokset,
      List<SyotettyArvo> arvot) {
    Jonosija jonosija = new Jonosija();
    jonosija.setHakemusOid(hakemusOid);
    jonosija.setHakutoiveprioriteetti(hakutoivePrioriteetti);
    jonosija.setHarkinnanvarainen(harkinnanvarainen);
    jonosija.setJarjestyskriteeritulokset(new JarjestyskriteeritulosContainer(tulokset));
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
    vaihe.setValintatapajonot(jonot);
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
  public static Jonosija luoHylattyJonosijaValisijoittelussa(String hakemusOid, String hakijaOid) {
    Jonosija sija =
        luoJonosijaEntity(
            hakemusOid,
            2,
            false,
            Arrays.asList(
                luoJarjestyskriteeritulosEntity(12.0, 0, HYLATTY),
                luoJarjestyskriteeritulosEntity(2.0, 1, HYVAKSYTTAVISSA),
                luoJarjestyskriteeritulosEntity(6.82, 2, HYVAKSYTTAVISSA)));

    sija.setHylattyValisijoittelussa(true);
    sija.setHakijaOid(hakijaOid);
    return sija;
  }

  @NotNull
  public static Jonosija luoHylattyJonosija(String hakemusOid, String hakijaOid) {
    Jonosija sija =
        luoJonosijaEntity(
            hakemusOid, 1, false, List.of(luoJarjestyskriteeritulosEntity(0, 0, HYLATTY)));
    sija.getJarjestyskriteeritulokset()
        .jarjestyskriteeritulokset
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
            luoHakutoiveEntity(
                "1.2.246.562.20.75182408387",
                Set.of(
                    luoValintakoeValinnanvaiheEntity(
                        2,
                        "vv2" + hakijaOid,
                        Arrays.asList(
                            luoValintakoeEntity(
                                "1413283036386-2122258152875806456",
                                "SOTE1_kaikkiosiot",
                                Osallistuminen.EI_OSALLISTU,
                                null,
                                "HYLATTY"),
                            luoValintakoeEntity(
                                "14132830363881382138007672413981",
                                "SOTEKOE_VK_RYHMA1",
                                Osallistuminen.EI_OSALLISTU,
                                null,
                                "HYLATTY"),
                            luoValintakoeEntity(
                                "1413373250365-957582059449313229",
                                "kielikoe_amk_fi",
                                Osallistuminen.EI_OSALLISTU,
                                null,
                                "HYLATTY"))))),
            luoHakutoiveEntity(
                "1.2.246.562.20.68508673735",
                Set.of(
                    luoValintakoeValinnanvaiheEntity(
                        2,
                        "vv3" + hakijaOid,
                        Arrays.asList(
                            luoValintakoeEntity(
                                "1413283096804-4591090746315355326",
                                "SOTE1_kaikkiosiot",
                                Osallistuminen.EI_OSALLISTU,
                                null,
                                "HYLATTY"),
                            luoValintakoeEntity(
                                "1413283096806-1884654158026567207",
                                "SOTEKOE_VK_RYHMA1",
                                Osallistuminen.EI_OSALLISTU,
                                null,
                                "HYLATTY"),
                            luoValintakoeEntity(
                                "1413373326095-2455808773394676892",
                                "kielikoe_amk_fi",
                                Osallistuminen.EI_OSALLISTU,
                                null,
                                "HYLATTY"))))),
            luoHakutoiveEntity(
                hakukohdeOid,
                Set.of(
                    luoValintakoeValinnanvaiheEntity(
                        2,
                        "vv5" + hakijaOid,
                        Arrays.asList(
                            luoValintakoeEntity(
                                "14132865785323928261359415431364",
                                "SOTE1_kaikkiosiot",
                                Osallistuminen.EI_OSALLISTU,
                                null,
                                "HYLATTY"),
                            luoValintakoeEntity(
                                "1413286578535-5213085081652469216",
                                "SOTEKOE_VK_RYHMA1",
                                Osallistuminen.EI_OSALLISTU,
                                null,
                                "HYLATTY"),
                            luoValintakoeEntity(
                                "1413286578535-5213085081652461564",
                                "KOHTEEN_SPESIFI_KOE_BUG-1564",
                                Osallistuminen.OSALLISTUU,
                                null,
                                osallistumisetHylatty ? "HYLATTY" : "HYVAKSYTTAVISSA"))))),
            luoHakutoiveEntity(
                "1.2.246.562.20.93258129167",
                Set.of(
                    luoValintakoeValinnanvaiheEntity(
                        1,
                        "vv6" + hakijaOid,
                        Arrays.asList(
                            luoValintakoeEntity(
                                "1413286979019-1352529564096963601",
                                "SOTE1_kaikkiosiot",
                                Osallistuminen.EI_OSALLISTU,
                                null,
                                "HYLATTY"),
                            luoValintakoeEntity(
                                "1413286979022-1524554277697202959",
                                "SOTEKOE_VK_RYHMA1",
                                Osallistuminen.EI_OSALLISTU,
                                null,
                                "HYLATTY"))))),
            luoHakutoiveEntity(
                "1.2.246.562.20.64586301414",
                Set.of(
                    luoValintakoeValinnanvaiheEntity(
                        2,
                        "vv7" + hakijaOid,
                        Arrays.asList(
                            luoValintakoeEntity(
                                "1413284065430-8581021134442572546",
                                "SOTE1_kaikkiosiot",
                                Osallistuminen.EI_OSALLISTU,
                                null,
                                "HYLATTY"),
                            luoValintakoeEntity(
                                "14132840654327607869284614200980",
                                "SOTEKOE_VK_RYHMA1",
                                Osallistuminen.EI_OSALLISTU,
                                null,
                                "HYLATTY"))))),
            luoHakutoiveEntity(
                "1.2.246.562.20.70883151881",
                Set.of(
                    luoValintakoeValinnanvaiheEntity(
                        2,
                        "vv8" + hakijaOid,
                        Arrays.asList(
                            luoValintakoeEntity(
                                "1413284033343-6883461063685526158",
                                "SOTE1_kaikkiosiot",
                                Osallistuminen.OSALLISTUU,
                                true,
                                osallistumisetHylatty ? "HYLATTY" : "HYVAKSYTTAVISSA"),
                            luoValintakoeEntity(
                                "1413284033345-7502478179236072968",
                                "SOTEKOE_VK_RYHMA1",
                                Osallistuminen.OSALLISTUU,
                                true,
                                osallistumisetHylatty ? "HYLATTY" : "HYVAKSYTTAVISSA")))))));
  }
}
