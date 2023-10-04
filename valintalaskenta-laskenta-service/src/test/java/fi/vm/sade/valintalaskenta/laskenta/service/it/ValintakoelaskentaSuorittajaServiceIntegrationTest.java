package fi.vm.sade.valintalaskenta.laskenta.service.it;

import static fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA;
import static fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen.EI_OSALLISTU;
import static fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen.OSALLISTUU;
import static fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.impl.ValintakoelaskentaSuorittajaServiceImpl.VALINNANVAIHE_HAKIJAN_VALINTA;
import static fi.vm.sade.valintalaskenta.laskenta.testdata.TestDataUtil.*;
import static java.util.stream.Collectors.groupingBy;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.not;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertFalse;

import co.unruly.matchers.StreamMatchers;
import com.google.common.collect.Sets;
import com.google.common.reflect.TypeToken;
import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;
import fi.vm.sade.service.valintaperusteet.dto.FunktiokutsuDTO;
import fi.vm.sade.service.valintaperusteet.dto.SyoteparametriDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.model.Funktionimi;
import fi.vm.sade.service.valintaperusteet.dto.model.Koekutsu;
import fi.vm.sade.valintalaskenta.domain.dto.AvainArvoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.domain.dto.LaskeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.Tasasijasaanto;
import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Hakutoive;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.OsallistuminenTulos;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Valintakoe;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeValinnanvaihe;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValintakoeOsallistuminenDAO;
import fi.vm.sade.valintalaskenta.laskenta.resource.ValintakoelaskennanKumulatiivisetTulokset;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.EdellinenValinnanvaiheKasittelija;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.ValintakoelaskentaSuorittajaService;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import fi.vm.sade.valintalaskenta.laskenta.testing.AbstractIntegrationTest;
import org.apache.commons.io.IOUtils;
import org.junit.Ignore;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.core.io.ClassPathResource;

public class ValintakoelaskentaSuorittajaServiceIntegrationTest extends AbstractIntegrationTest {
  private final String uuid = null;

  @Autowired private ApplicationContext applicationContext;

  @Autowired private ValintakoelaskentaSuorittajaService valintakoelaskentaSuorittajaService;

  @Autowired private ValintakoeOsallistuminenDAO valintakoeOsallistuminenDAO;

  @Autowired private EdellinenValinnanvaiheKasittelija edellinenValinnanvaiheKasittelija;

  private static final FunktiokutsuDTO totuusarvoTrue;
  private static final FunktiokutsuDTO totuusarvoFalse;

  private boolean korkeakouluhaku = false;

  static {
    totuusarvoTrue = new FunktiokutsuDTO();
    totuusarvoTrue.setFunktionimi(Funktionimi.TOTUUSARVO);

    {
      SyoteparametriDTO param = new SyoteparametriDTO();
      param.setAvain("totuusarvo");
      param.setArvo(Boolean.TRUE.toString());
      totuusarvoTrue.getSyoteparametrit().add(param);
    }

    totuusarvoFalse = new FunktiokutsuDTO();
    totuusarvoFalse.setFunktionimi(Funktionimi.TOTUUSARVO);
    {
      SyoteparametriDTO param = new SyoteparametriDTO();
      param.setAvain("totuusarvo");
      param.setArvo(Boolean.FALSE.toString());
      totuusarvoFalse.getSyoteparametrit().add(param);
    }
  }

  private ValintakoelaskennanKumulatiivisetTulokset kumulatiivisetTulokset =
      new ValintakoelaskennanKumulatiivisetTulokset();

  @Test
  public void test() {
    final String hakukohdeOid1 = "hakukohdeOid1";
    final String hakukohdeOid2 = "hakukohdeOid2";
    final String hakemusOid = "hakemusOid";
    final String hakuOid = "hakuOid";

    final HakemusDTO hakemus =
        luoHakemus(hakuOid, hakemusOid, "hakijaOid", hakukohdeOid1, hakukohdeOid2);

    final String valintakoetunniste = "valintakoetunniste";

    final String valinnanVaiheOid1 = "valinnanVaiheOid1";
    final int valinnanVaiheJarjestysluku1 = 0;
    Map<String, FunktiokutsuDTO> kokeet1 = createValintakokeet(valintakoetunniste);
    kokeet1.put("valintakoetunniste2", totuusarvoTrue);

    ValintaperusteetDTO valintaperusteet1 =
        luoValintaperusteetJaValintakoeValinnanVaihe(
            hakuOid,
            hakukohdeOid1,
            valinnanVaiheOid1,
            valinnanVaiheJarjestysluku1,
            kokeet1,
            Koekutsu.YLIN_TOIVE,
            "kutsunKohdeAvain");

    final String valinnanVaiheOid2 = "valinnanVaiheOid2";
    final int valinnanVaiheJarjestysluku2 = 1;

    Map<String, FunktiokutsuDTO> kokeet2 = new HashMap<>();
    kokeet2.put(valintakoetunniste, totuusarvoFalse);

    ValintaperusteetDTO valintaperusteet2 =
        luoValintaperusteetJaValintakoeValinnanVaihe(
            hakuOid,
            hakukohdeOid2,
            valinnanVaiheOid2,
            valinnanVaiheJarjestysluku2,
            kokeet2,
            Koekutsu.YLIN_TOIVE,
            "kutsunKohdeAvain");

    List<ValintaperusteetDTO> valintaperusteet = new ArrayList<>();
    valintaperusteet.add(valintaperusteet1);
    valintaperusteet.add(valintaperusteet2);

    assertNull(valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid));
    valintakoelaskentaSuorittajaService.laske(
        hakemus, valintaperusteet, new HashMap<>(), uuid, kumulatiivisetTulokset, korkeakouluhaku);
    ValintakoeOsallistuminen osallistuminen =
        valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid);
    assertNotNull(osallistuminen);
    assertEquals(
        osallistuminen
            .getHakutoiveetAsList()
            .get(0)
            .getValintakoeValinnanvaiheetAsList()
            .get(0)
            .getValintakokeetAsList()
            .get(0)
            .getOsallistuminenTulos()
            .getOsallistuminen(),
        OSALLISTUU);
    assertEquals(
        osallistuminen
            .getHakutoiveetAsList()
            .get(0)
            .getValintakoeValinnanvaiheetAsList()
            .get(0)
            .getValintakokeetAsList()
            .get(1)
            .getOsallistuminenTulos()
            .getOsallistuminen(),
        OSALLISTUU);
  }

  @Test
  public void testKoekutsuHakijanValinta() {
    final String hakukohdeOid1 = "hakukohdeOid1";
    final String hakukohdeOid2 = "hakukohdeOid2";
    final String hakemusOid = "hakemusOid";
    final String hakuOid = "hakuOid";

    final HakemusDTO hakemus =
        luoHakemus(hakuOid, hakemusOid, "hakijaOid", hakukohdeOid1, hakukohdeOid2);

    final String valintakoetunniste = "valintakoetunniste";

    final String valinnanVaiheOid1 = "valinnanVaiheOid1";
    final int valinnanVaiheJarjestysluku1 = 0;
    Map<String, FunktiokutsuDTO> kokeet1 = createValintakokeet(valintakoetunniste);

    ValintaperusteetDTO valintaperusteet1 =
        luoValintaperusteetJaValintakoeValinnanVaihe(
            hakuOid,
            hakukohdeOid1,
            valinnanVaiheOid1,
            valinnanVaiheJarjestysluku1,
            kokeet1,
            Koekutsu.HAKIJAN_VALINTA,
            "hakukohdeKutsunKohde2");

    List<ValintaperusteetDTO> valintaperusteet = new ArrayList<>();
    valintaperusteet.add(valintaperusteet1);

    valintakoelaskentaSuorittajaService.laske(
        hakemus, valintaperusteet, new HashMap<>(), uuid, kumulatiivisetTulokset, korkeakouluhaku);

    ValintakoeOsallistuminen osallistuminen =
        valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid);
    assertNotNull(osallistuminen);
    assertEquals(
        OSALLISTUU,
        osallistuminen
            .getHakutoiveetAsList()
            .get(0)
            .getValintakoeValinnanvaiheetAsList()
            .get(0)
            .getValintakokeetAsList()
            .get(0)
            .getOsallistuminenTulos()
            .getOsallistuminen());
    assertEquals(
        VALINNANVAIHE_HAKIJAN_VALINTA,
        osallistuminen.getHakutoiveetAsList().get(0).getValintakoeValinnanvaiheetAsList().get(0).getValinnanvaiheOid());
    assertEquals(
        Integer.valueOf(100),
        osallistuminen
            .getHakutoiveetAsList()
            .get(0)
            .getValintakoeValinnanvaiheetAsList()
            .get(0)
            .getValinnanVaiheJarjestysluku());
    assertEquals(1, osallistuminen.getHakutoiveet().size());
  }

  @Test
  public void test1() {
    final String hakukohdeOid1 = "hakukohdeOid1";
    final String hakukohdeOid2 = "hakukohdeOid2";
    final String hakukohdeOid3 = "hakukohdeOid3";

    final String hakuOid = "hakuOid";
    final String valintakoetunniste1 = "valintakoetunniste1";
    final String valintakoetunniste2 = "valintakoetunniste2";
    final String valintakoetunniste3 = "valintakoetunniste3";

    final String valinnanVaiheOid1 = "valinnanVaiheOid1";
    final int valinnanVaiheJarjestysluku1 = 0;
    Map<String, FunktiokutsuDTO> kokeet1 = createValintakokeet(valintakoetunniste1);

    ValintaperusteetDTO valintaperusteet1 =
        luoValintaperusteetJaValintakoeValinnanVaihe(
            hakuOid,
            hakukohdeOid1,
            valinnanVaiheOid1,
            valinnanVaiheJarjestysluku1,
            kokeet1,
            Koekutsu.HAKIJAN_VALINTA,
            "hakukohdeKutsunKohde2");

    final String valinnanVaiheOid2 = "valinnanVaiheOid2";
    final int valinnanVaiheJarjestysluku2 = 0;

    Map<String, FunktiokutsuDTO> kokeet2 = new TreeMap<>();
    kokeet2.put(valintakoetunniste1, totuusarvoTrue);
    kokeet2.put(valintakoetunniste2, totuusarvoTrue);

    ValintaperusteetDTO valintaperusteet2 =
        luoValintaperusteetJaValintakoeValinnanVaihe(
            hakuOid,
            hakukohdeOid2,
            valinnanVaiheOid2,
            valinnanVaiheJarjestysluku2,
            kokeet2,
            Koekutsu.HAKIJAN_VALINTA,
            "hakukohdeKutsunKohde2");
    valintaperusteet2.getValinnanVaihe().getValintakoe().get(1).setKutsunKohde(Koekutsu.YLIN_TOIVE);

    final String valinnanVaiheOid3 = "valinnanVaiheOid3";
    final int valinnanVaiheJarjestysluku3 = 0;

    Map<String, FunktiokutsuDTO> kokeet3 = new TreeMap<>();
    kokeet3.put(valintakoetunniste2, totuusarvoTrue);
    kokeet3.put(valintakoetunniste3, totuusarvoTrue);

    ValintaperusteetDTO valintaperusteet3 =
        luoValintaperusteetJaValintakoeValinnanVaihe(
            hakuOid,
            hakukohdeOid3,
            valinnanVaiheOid3,
            valinnanVaiheJarjestysluku3,
            kokeet3,
            Koekutsu.YLIN_TOIVE,
            "hakukohdeKutsunKohde2");

    testWithOrder(
        "hakemusOid1",
        hakukohdeOid1,
        hakukohdeOid2,
        hakukohdeOid3,
        hakuOid,
        valintaperusteet1,
        valintaperusteet2,
        valintaperusteet3);
    testWithOrder(
        "hakemusOid2",
        hakukohdeOid1,
        hakukohdeOid2,
        hakukohdeOid3,
        hakuOid,
        valintaperusteet1,
        valintaperusteet3,
        valintaperusteet2);
    testWithOrder(
        "hakemusOid3",
        hakukohdeOid1,
        hakukohdeOid2,
        hakukohdeOid3,
        hakuOid,
        valintaperusteet2,
        valintaperusteet1,
        valintaperusteet3);
    testWithOrder(
        "hakemusOid4",
        hakukohdeOid1,
        hakukohdeOid2,
        hakukohdeOid3,
        hakuOid,
        valintaperusteet2,
        valintaperusteet3,
        valintaperusteet1);
    testWithOrder(
        "hakemusOid5",
        hakukohdeOid1,
        hakukohdeOid2,
        hakukohdeOid3,
        hakuOid,
        valintaperusteet3,
        valintaperusteet1,
        valintaperusteet2);
    testWithOrder(
        "hakemusOid6",
        hakukohdeOid1,
        hakukohdeOid2,
        hakukohdeOid3,
        hakuOid,
        valintaperusteet3,
        valintaperusteet2,
        valintaperusteet1);
  }

  private void testWithOrder(
      String hakemusOid,
      String hakukohdeOid1,
      String hakukohdeOid2,
      String hakukohdeOid3,
      String hakuOid,
      ValintaperusteetDTO vp1,
      ValintaperusteetDTO vp2,
      ValintaperusteetDTO vp3) {
    final HakemusDTO hakemus =
        luoHakemus("hakuOid", hakemusOid, "hakijaOid", hakukohdeOid1, hakukohdeOid2, hakukohdeOid3);
    assertNull(valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid));
    valintakoelaskentaSuorittajaService.laske(
        hakemus, Collections.singletonList(vp1), new HashMap<>(), uuid, kumulatiivisetTulokset, korkeakouluhaku);
    valintakoelaskentaSuorittajaService.laske(
        hakemus, Collections.singletonList(vp2), new HashMap<>(), uuid, kumulatiivisetTulokset, korkeakouluhaku);
    valintakoelaskentaSuorittajaService.laske(
        hakemus, Collections.singletonList(vp3), new HashMap<>(), uuid, kumulatiivisetTulokset, korkeakouluhaku);
    assertTest1Results(valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid));
  }

  private void assertTest1Results(ValintakoeOsallistuminen osallistuminen) {
    assertNotNull(osallistuminen);

    assertEquals(2, osallistuminen.getHakutoiveet().size());

    Optional<Hakutoive> hakutoive1 =
        osallistuminen.getHakutoiveet().stream()
            .filter(h -> h.getHakukohdeOid().equals("hakukohdeOid2"))
            .findFirst();
    assertTrue(hakutoive1.isPresent());

    assertEquals(2, hakutoive1.get().getValintakoeValinnanvaiheet().size());
    assertEquals(1, hakutoive1.get().getValintakoeValinnanvaiheetAsList().get(0).getValintakokeet().size());
    assertEquals(1, hakutoive1.get().getValintakoeValinnanvaiheetAsList().get(1).getValintakokeet().size());

    final Optional<Valintakoe> valintakoetunniste1 =
        hakutoive1.get().getValintakoeValinnanvaiheet().stream()
            .flatMap(v -> v.getValintakokeet().stream())
            .filter(koeWithTunniste("valintakoetunniste1"))
            .findFirst();
    assertTrue(valintakoetunniste1.isPresent());
    assertEquals(
        OSALLISTUU, valintakoetunniste1.get().getOsallistuminenTulos().getOsallistuminen());

    final Optional<Valintakoe> valintakoetunniste2 =
        hakutoive1.get().getValintakoeValinnanvaiheet().stream()
            .flatMap(v -> v.getValintakokeet().stream())
            .filter(koeWithTunniste("valintakoetunniste2"))
            .findFirst();
    assertTrue(valintakoetunniste2.isPresent());
    assertEquals(
        OSALLISTUU, valintakoetunniste2.get().getOsallistuminenTulos().getOsallistuminen());

    Optional<Hakutoive> hakutoive2 =
        osallistuminen.getHakutoiveet().stream()
            .filter(h -> h.getHakukohdeOid().equals("hakukohdeOid3"))
            .findFirst();
    assertTrue(hakutoive2.isPresent());

    assertEquals(1, hakutoive2.get().getValintakoeValinnanvaiheet().size());
    assertEquals(2, hakutoive2.get().getValintakoeValinnanvaiheetAsList().get(0).getValintakokeet().size());

    final Optional<Valintakoe> valintakoetunniste3 =
        hakutoive2.get().getValintakoeValinnanvaiheet().stream()
            .flatMap(v -> v.getValintakokeet().stream())
            .filter(koeWithTunniste("valintakoetunniste3"))
            .findFirst();
    assertTrue(valintakoetunniste3.isPresent());
    assertEquals(
        OSALLISTUU, valintakoetunniste3.get().getOsallistuminenTulos().getOsallistuminen());

    final Optional<Valintakoe> valintakoetunniste2b =
        hakutoive2.get().getValintakoeValinnanvaiheet().stream()
            .flatMap(v -> v.getValintakokeet().stream())
            .filter(koeWithTunniste("valintakoetunniste2"))
            .findFirst();
    assertTrue(valintakoetunniste2b.isPresent());
    assertEquals(
        EI_OSALLISTU, valintakoetunniste2b.get().getOsallistuminenTulos().getOsallistuminen());
  }

  @Test
  public void test2() {
    final String hakukohdeOid1 = "hakukohdeOid1";
    final String hakukohdeOid2 = "hakukohdeOid2";
    final String hakukohdeOid3 = "hakukohdeOid3";

    final String hakuOid = "hakuOid";
    final String valintakoetunniste1 = "valintakoetunniste1";
    final String valintakoetunniste2 = "valintakoetunniste2";
    final String valintakoetunniste3 = "valintakoetunniste3";

    final String valinnanVaiheOid1 = "valinnanVaiheOid1";
    final int valinnanVaiheJarjestysluku1 = 0;
    Map<String, FunktiokutsuDTO> kokeet1 = createValintakokeet(valintakoetunniste1);
    kokeet1.put(valintakoetunniste2, totuusarvoTrue);

    ValintaperusteetDTO valintaperusteet1 =
        luoValintaperusteetJaValintakoeValinnanVaihe(
            hakuOid,
            hakukohdeOid1,
            valinnanVaiheOid1,
            valinnanVaiheJarjestysluku1,
            kokeet1,
            Koekutsu.HAKIJAN_VALINTA,
            "hakukohdeKutsunKohde1");
    valintaperusteet1.getValinnanVaihe().getValintakoe().stream()
        .filter(koe -> koe.getTunniste().equals(valintakoetunniste2))
        .findFirst()
        .get()
        .setKutsunKohdeAvain("hakukohdeKutsunKohde2");

    final String valinnanVaiheOid2 = "valinnanVaiheOid2";
    final int valinnanVaiheJarjestysluku2 = 0;

    Map<String, FunktiokutsuDTO> kokeet2 = new TreeMap<>();
    kokeet2.put(valintakoetunniste2, totuusarvoTrue);
    kokeet2.put(valintakoetunniste3, totuusarvoTrue);

    ValintaperusteetDTO valintaperusteet2 =
        luoValintaperusteetJaValintakoeValinnanVaihe(
            hakuOid,
            hakukohdeOid2,
            valinnanVaiheOid2,
            valinnanVaiheJarjestysluku2,
            kokeet2,
            Koekutsu.HAKIJAN_VALINTA,
            "hakukohdeKutsunKohde2");
    valintaperusteet2.getValinnanVaihe().getValintakoe().stream()
        .filter(koe -> koe.getTunniste().equals(valintakoetunniste3))
        .findFirst()
        .get()
        .setKutsunKohde(Koekutsu.YLIN_TOIVE);

    final String valinnanVaiheOid3 = "valinnanVaiheOid3";
    final int valinnanVaiheJarjestysluku3 = 0;

    Map<String, FunktiokutsuDTO> kokeet3 = new TreeMap<>();
    kokeet3.put(valintakoetunniste1, totuusarvoTrue);
    kokeet3.put(valintakoetunniste2, totuusarvoTrue);

    ValintaperusteetDTO valintaperusteet3 =
        luoValintaperusteetJaValintakoeValinnanVaihe(
            hakuOid,
            hakukohdeOid3,
            valinnanVaiheOid3,
            valinnanVaiheJarjestysluku3,
            kokeet3,
            Koekutsu.HAKIJAN_VALINTA,
            "hakukohdeKutsunKohde1");
    valintaperusteet3.getValinnanVaihe().getValintakoe().stream()
        .filter(koe -> koe.getTunniste().equals(valintakoetunniste2))
        .findFirst()
        .get()
        .setKutsunKohdeAvain("hakukohdeKutsunKohde2");

    testWithOrder2(
        "hakemusOid1",
        hakukohdeOid1,
        hakukohdeOid2,
        hakukohdeOid3,
        hakuOid,
        valintaperusteet1,
        valintaperusteet2,
        valintaperusteet3);
    testWithOrder2(
        "hakemusOid2",
        hakukohdeOid1,
        hakukohdeOid2,
        hakukohdeOid3,
        hakuOid,
        valintaperusteet1,
        valintaperusteet3,
        valintaperusteet2);
    testWithOrder2(
        "hakemusOid3",
        hakukohdeOid1,
        hakukohdeOid2,
        hakukohdeOid3,
        hakuOid,
        valintaperusteet2,
        valintaperusteet1,
        valintaperusteet3);
    testWithOrder2(
        "hakemusOid4",
        hakukohdeOid1,
        hakukohdeOid2,
        hakukohdeOid3,
        hakuOid,
        valintaperusteet2,
        valintaperusteet3,
        valintaperusteet1);
    testWithOrder2(
        "hakemusOid5",
        hakukohdeOid1,
        hakukohdeOid2,
        hakukohdeOid3,
        hakuOid,
        valintaperusteet3,
        valintaperusteet1,
        valintaperusteet2);
    testWithOrder2(
        "hakemusOid6",
        hakukohdeOid1,
        hakukohdeOid2,
        hakukohdeOid3,
        hakuOid,
        valintaperusteet3,
        valintaperusteet2,
        valintaperusteet1);
  }

  private void testWithOrder2(
      String hakemusOid,
      String hakukohdeOid1,
      String hakukohdeOid2,
      String hakukohdeOid3,
      String hakuOid,
      ValintaperusteetDTO vp1,
      ValintaperusteetDTO vp2,
      ValintaperusteetDTO vp3) {
    final HakemusDTO hakemus =
        luoHakemus("hakuOid", hakemusOid, "hakijaOid", hakukohdeOid1, hakukohdeOid2, hakukohdeOid3);
    AvainArvoDTO avain = new AvainArvoDTO();
    avain.setAvain("hakukohdeKutsunKohde1");
    avain.setArvo("hakukohdeOid2");
    AvainArvoDTO avain2 = new AvainArvoDTO();
    avain2.setAvain("hakukohdeKutsunKohde2");
    avain2.setArvo("hakukohdeOid3");
    hakemus.setAvaimet(Arrays.asList(avain, avain2));

    assertNull(valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid));
    valintakoelaskentaSuorittajaService.laske(
        hakemus, Collections.singletonList(vp1), new HashMap<>(), uuid, kumulatiivisetTulokset, korkeakouluhaku);
    valintakoelaskentaSuorittajaService.laske(
        hakemus, Collections.singletonList(vp2), new HashMap<>(), uuid, kumulatiivisetTulokset, korkeakouluhaku);
    valintakoelaskentaSuorittajaService.laske(
        hakemus, Collections.singletonList(vp3), new HashMap<>(), uuid, kumulatiivisetTulokset, korkeakouluhaku);
    assertTest2Results(valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid));
  }

  private void assertTest2Results(ValintakoeOsallistuminen osallistuminen) {
    assertNotNull(osallistuminen);

    assertEquals(2, osallistuminen.getHakutoiveet().size());

    Optional<Hakutoive> hakutoive1 =
        osallistuminen.getHakutoiveet().stream()
            .filter(h -> h.getHakukohdeOid().equals("hakukohdeOid2"))
            .findFirst();
    assertTrue(hakutoive1.isPresent());

    assertEquals(2, hakutoive1.get().getValintakoeValinnanvaiheet().size());
    assertEquals(1, hakutoive1.get().getValintakoeValinnanvaiheetAsList().get(0).getValintakokeet().size());
    assertEquals(1, hakutoive1.get().getValintakoeValinnanvaiheetAsList().get(1).getValintakokeet().size());

    final Optional<Valintakoe> valintakoetunniste1 =
        hakutoive1.get().getValintakoeValinnanvaiheet().stream()
            .flatMap(v -> v.getValintakokeet().stream())
            .filter(koeWithTunniste("valintakoetunniste1"))
            .findFirst();
    assertTrue(valintakoetunniste1.isPresent());
    assertEquals(
        OSALLISTUU, valintakoetunniste1.get().getOsallistuminenTulos().getOsallistuminen());

    final Optional<Valintakoe> valintakoetunniste3 =
        hakutoive1.get().getValintakoeValinnanvaiheet().stream()
            .flatMap(v -> v.getValintakokeet().stream())
            .filter(koeWithTunniste("valintakoetunniste3"))
            .findFirst();
    assertTrue(valintakoetunniste3.isPresent());
    assertEquals(
        OSALLISTUU, valintakoetunniste3.get().getOsallistuminenTulos().getOsallistuminen());

    Optional<Hakutoive> hakutoive2 =
        osallistuminen.getHakutoiveet().stream()
            .filter(h -> h.getHakukohdeOid().equals("hakukohdeOid3"))
            .findFirst();
    assertTrue(hakutoive2.isPresent());

    assertEquals(1, hakutoive2.get().getValintakoeValinnanvaiheet().size());
    assertEquals(1, hakutoive2.get().getValintakoeValinnanvaiheetAsList().get(0).getValintakokeet().size());

    final Optional<Valintakoe> valintakoetunniste2 =
        hakutoive2.get().getValintakoeValinnanvaiheet().stream()
            .flatMap(v -> v.getValintakokeet().stream())
            .filter(koeWithTunniste("valintakoetunniste2"))
            .findFirst();
    assertTrue(valintakoetunniste2.isPresent());
    assertEquals(
        OSALLISTUU, valintakoetunniste2.get().getOsallistuminenTulos().getOsallistuminen());
  }

  @Test
  public void test3() {
    final String hakukohdeOid1 = "hakukohdeOid1";
    final String hakukohdeOid2 = "hakukohdeOid2";
    final String hakukohdeOid3 = "hakukohdeOid3";

    final String hakuOid = "hakuOid";
    final String valintakoetunniste1 = "valintakoetunniste1";
    final String valintakoetunniste2 = "valintakoetunniste2";

    final String valinnanVaiheOid1 = "valinnanVaiheOid1";
    final int valinnanVaiheJarjestysluku1 = 0;
    Map<String, FunktiokutsuDTO> kokeet1 = createValintakokeet(valintakoetunniste1);
    kokeet1.put(valintakoetunniste2, totuusarvoTrue);

    ValintaperusteetDTO valintaperusteet1 =
        luoValintaperusteetJaValintakoeValinnanVaihe(
            hakuOid,
            hakukohdeOid1,
            valinnanVaiheOid1,
            valinnanVaiheJarjestysluku1,
            kokeet1,
            Koekutsu.HAKIJAN_VALINTA,
            "hakukohdeKutsunKohde1");
    valintaperusteet1.getValinnanVaihe().getValintakoe().stream()
        .filter(koe -> koe.getTunniste().equals(valintakoetunniste2))
        .findFirst()
        .get()
        .setKutsunKohdeAvain("hakukohdeKutsunKohde2");

    final String valinnanVaiheOid2 = "valinnanVaiheOid2";
    final int valinnanVaiheJarjestysluku2 = 0;

    Map<String, FunktiokutsuDTO> kokeet2 = new TreeMap<>();
    kokeet2.put(valintakoetunniste2, totuusarvoTrue);

    ValintaperusteetDTO valintaperusteet2 =
        luoValintaperusteetJaValintakoeValinnanVaihe(
            hakuOid,
            hakukohdeOid2,
            valinnanVaiheOid2,
            valinnanVaiheJarjestysluku2,
            kokeet2,
            Koekutsu.HAKIJAN_VALINTA,
            "hakukohdeKutsunKohde2");

    final String valinnanVaiheOid3 = "valinnanVaiheOid3";
    final int valinnanVaiheJarjestysluku3 = 0;

    Map<String, FunktiokutsuDTO> kokeet3 = new TreeMap<>();
    kokeet3.put(valintakoetunniste1, totuusarvoTrue);

    ValintaperusteetDTO valintaperusteet3 =
        luoValintaperusteetJaValintakoeValinnanVaihe(
            hakuOid,
            hakukohdeOid3,
            valinnanVaiheOid3,
            valinnanVaiheJarjestysluku3,
            kokeet3,
            Koekutsu.HAKIJAN_VALINTA,
            "hakukohdeKutsunKohde1");

    testWithOrder3(
        "hakemusOid1",
        hakukohdeOid2,
        hakukohdeOid3,
        hakuOid,
        valintaperusteet1,
        valintaperusteet2,
        valintaperusteet3);
    testWithOrder3(
        "hakemusOid2",
        hakukohdeOid2,
        hakukohdeOid3,
        hakuOid,
        valintaperusteet1,
        valintaperusteet3,
        valintaperusteet2);
    testWithOrder3(
        "hakemusOid3",
        hakukohdeOid2,
        hakukohdeOid3,
        hakuOid,
        valintaperusteet2,
        valintaperusteet1,
        valintaperusteet3);
    testWithOrder3(
        "hakemusOid4",
        hakukohdeOid2,
        hakukohdeOid3,
        hakuOid,
        valintaperusteet2,
        valintaperusteet3,
        valintaperusteet1);
    testWithOrder3(
        "hakemusOid5",
        hakukohdeOid2,
        hakukohdeOid3,
        hakuOid,
        valintaperusteet3,
        valintaperusteet1,
        valintaperusteet2);
    testWithOrder3(
        "hakemusOid6",
        hakukohdeOid2,
        hakukohdeOid3,
        hakuOid,
        valintaperusteet3,
        valintaperusteet2,
        valintaperusteet1);
  }

  private void testWithOrder3(
      String hakemusOid,
      String hakukohdeOid2,
      String hakukohdeOid3,
      String hakuOid,
      ValintaperusteetDTO vp1,
      ValintaperusteetDTO vp2,
      ValintaperusteetDTO vp3) {
    final HakemusDTO hakemus =
        luoHakemus("hakuOid", hakemusOid, "hakijaOid", hakukohdeOid2, hakukohdeOid3);
    AvainArvoDTO avain = new AvainArvoDTO();
    avain.setAvain("hakukohdeKutsunKohde1");
    avain.setArvo("hakukohdeOid1");
    AvainArvoDTO avain2 = new AvainArvoDTO();
    avain2.setAvain("hakukohdeKutsunKohde2");
    avain2.setArvo("hakukohdeOid1");
    hakemus.setAvaimet(Arrays.asList(avain, avain2));

    assertNull(valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid));
    valintakoelaskentaSuorittajaService.laske(
        hakemus, Collections.singletonList(vp1), new HashMap<>(), uuid, kumulatiivisetTulokset, korkeakouluhaku);
    valintakoelaskentaSuorittajaService.laske(
        hakemus, Collections.singletonList(vp2), new HashMap<>(), uuid, kumulatiivisetTulokset, korkeakouluhaku);
    valintakoelaskentaSuorittajaService.laske(
        hakemus, Collections.singletonList(vp3), new HashMap<>(), uuid, kumulatiivisetTulokset, korkeakouluhaku);
    assertTest3Results(valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid));
  }

  private void assertTest3Results(ValintakoeOsallistuminen osallistuminen) {
    assertNotNull(osallistuminen);

    assertEquals(1, osallistuminen.getHakutoiveet().size());

    Hakutoive hakutoive1 = osallistuminen.getHakutoiveetAsList().get(0);
    assertEquals(1, hakutoive1.getValintakoeValinnanvaiheet().size());

    assertEquals(2, hakutoive1.getValintakoeValinnanvaiheetAsList().get(0).getValintakokeet().size());

    final Optional<Valintakoe> valintakoetunniste2 =
        hakutoive1.getValintakoeValinnanvaiheet().stream()
            .flatMap(v -> v.getValintakokeet().stream())
            .filter(koeWithTunniste("valintakoetunniste2"))
            .findFirst();
    assertTrue(valintakoetunniste2.isPresent());
    assertEquals(
        OSALLISTUU, valintakoetunniste2.get().getOsallistuminenTulos().getOsallistuminen());

    final Optional<Valintakoe> valintakoetunniste3 =
        hakutoive1.getValintakoeValinnanvaiheet().stream()
            .flatMap(v -> v.getValintakokeet().stream())
            .filter(koeWithTunniste("valintakoetunniste1"))
            .findFirst();
    assertTrue(valintakoetunniste3.isPresent());
    assertEquals(
        OSALLISTUU, valintakoetunniste3.get().getOsallistuminenTulos().getOsallistuminen());
  }

  @Test
  public void test4() {
    final String hakukohdeOid1 = "hakukohdeOid1";
    final String hakukohdeOid2 = "hakukohdeOid2";
    final String hakukohdeOid3 = "hakukohdeOid3";

    final String hakuOid = "hakuOid";
    final String valintakoetunniste1 = "valintakoetunniste1";
    final String valintakoetunniste2 = "valintakoetunniste2";
    final String valintakoetunniste3 = "valintakoetunniste3";

    final String valinnanVaiheOid1 = "valinnanVaiheOid1";
    final int valinnanVaiheJarjestysluku1 = 0;
    Map<String, FunktiokutsuDTO> kokeet1 = createValintakokeet(valintakoetunniste1);
    kokeet1.put(valintakoetunniste2, totuusarvoTrue);

    ValintaperusteetDTO valintaperusteet1 =
        luoValintaperusteetJaValintakoeValinnanVaihe(
            hakuOid,
            hakukohdeOid1,
            valinnanVaiheOid1,
            valinnanVaiheJarjestysluku1,
            kokeet1,
            Koekutsu.HAKIJAN_VALINTA,
            "hakukohdeKutsunKohde1");
    valintaperusteet1.getValinnanVaihe().getValintakoe().stream()
        .filter(koe -> koe.getTunniste().equals(valintakoetunniste2))
        .findFirst()
        .get()
        .setKutsunKohdeAvain("hakukohdeKutsunKohde2");

    final String valinnanVaiheOid2 = "valinnanVaiheOid2";
    final int valinnanVaiheJarjestysluku2 = 0;

    Map<String, FunktiokutsuDTO> kokeet2 = new TreeMap<>();
    kokeet2.put(valintakoetunniste2, totuusarvoTrue);
    kokeet2.put(valintakoetunniste3, totuusarvoTrue);

    ValintaperusteetDTO valintaperusteet2 =
        luoValintaperusteetJaValintakoeValinnanVaihe(
            hakuOid,
            hakukohdeOid2,
            valinnanVaiheOid2,
            valinnanVaiheJarjestysluku2,
            kokeet2,
            Koekutsu.HAKIJAN_VALINTA,
            "hakukohdeKutsunKohde2");
    valintaperusteet2.getValinnanVaihe().getValintakoe().stream()
        .filter(koe -> koe.getTunniste().equals(valintakoetunniste3))
        .findFirst()
        .get()
        .setKutsunKohde(Koekutsu.YLIN_TOIVE);

    final String valinnanVaiheOid3 = "valinnanVaiheOid3";
    final int valinnanVaiheJarjestysluku3 = 0;

    Map<String, FunktiokutsuDTO> kokeet3 = new TreeMap<>();
    kokeet3.put(valintakoetunniste1, totuusarvoTrue);
    kokeet3.put(valintakoetunniste2, totuusarvoTrue);

    ValintaperusteetDTO valintaperusteet3 =
        luoValintaperusteetJaValintakoeValinnanVaihe(
            hakuOid,
            hakukohdeOid3,
            valinnanVaiheOid3,
            valinnanVaiheJarjestysluku3,
            kokeet3,
            Koekutsu.HAKIJAN_VALINTA,
            "hakukohdeKutsunKohde1");
    valintaperusteet3.getValinnanVaihe().getValintakoe().stream()
        .filter(koe -> koe.getTunniste().equals(valintakoetunniste2))
        .findFirst()
        .get()
        .setKutsunKohdeAvain("hakukohdeKutsunKohde2");

    AvainArvoDTO avain = new AvainArvoDTO();
    avain.setAvain("hakukohdeKutsunKohde1");
    avain.setArvo("hakukohdeOid2");
    AvainArvoDTO avain2 = new AvainArvoDTO();
    avain2.setAvain("hakukohdeKutsunKohde2");
    avain2.setArvo("hakukohdeOid3");

    assertNull(valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, "hakemusOid1"));
    final HakemusDTO hakemus =
        luoHakemus(
            "hakuOid", "hakemusOid1", "hakijaOid", hakukohdeOid1, hakukohdeOid2, hakukohdeOid3);
    hakemus.setAvaimet(Arrays.asList(avain, avain2));

    valintakoelaskentaSuorittajaService.laske(
        hakemus,
        Arrays.asList(valintaperusteet1, valintaperusteet2, valintaperusteet3),
        new HashMap<>(),
        uuid,
        kumulatiivisetTulokset,
        korkeakouluhaku);
    final HakemusDTO hakemus2 =
        luoHakemus("hakuOid", "hakemusOid2", "hakijaOid", hakukohdeOid1, hakukohdeOid3);
    hakemus2.setAvaimet(Arrays.asList(avain, avain2));

    valintakoelaskentaSuorittajaService.laske(
        hakemus2,
        Arrays.asList(valintaperusteet1, valintaperusteet2, valintaperusteet3),
        new HashMap<>(),
        uuid,
        kumulatiivisetTulokset,
        korkeakouluhaku);
    assertTest4Results(
        valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, "hakemusOid2"));
  }

  private void assertTest4Results(ValintakoeOsallistuminen osallistuminen) {
    assertNotNull(osallistuminen);

    assertEquals(2, osallistuminen.getHakutoiveet().size());

    Hakutoive hakutoive1 = osallistuminen.getHakutoiveetAsList().get(0);
    assertEquals(1, hakutoive1.getValintakoeValinnanvaiheet().size());
    assertEquals(1, hakutoive1.getValintakoeValinnanvaiheetAsList().get(0).getValintakokeet().size());

    final Optional<Valintakoe> valintakoetunniste2 =
        hakutoive1.getValintakoeValinnanvaiheet().stream()
            .flatMap(v -> v.getValintakokeet().stream())
            .filter(koeWithTunniste("valintakoetunniste2"))
            .findFirst();
    assertTrue(valintakoetunniste2.isPresent());
    assertEquals(
        OSALLISTUU, valintakoetunniste2.get().getOsallistuminenTulos().getOsallistuminen());

    Hakutoive hakutoive2 = osallistuminen.getHakutoiveetAsList().get(1);
    assertEquals(1, hakutoive2.getValintakoeValinnanvaiheet().size());
    assertEquals(1, hakutoive2.getValintakoeValinnanvaiheetAsList().get(0).getValintakokeet().size());

    final Optional<Valintakoe> valintakoetunniste1 =
        hakutoive2.getValintakoeValinnanvaiheet().stream()
            .flatMap(v -> v.getValintakokeet().stream())
            .filter(koeWithTunniste("valintakoetunniste1"))
            .findFirst();
    assertTrue(valintakoetunniste1.isPresent());
    assertEquals(
        OSALLISTUU, valintakoetunniste1.get().getOsallistuminenTulos().getOsallistuminen());
  }

  @Ignore
  @Test
  public void testEiKoekutsujaAikaisemminHylatyilleHakijanValinnoille() {
    final String hakemusOid = "1.2.246.562.11.00000072753";
    final String hakukohdeOid = "1.2.246.562.5.91937845484";
    final String hakuOid = "1.2.246.562.5.2013080813081926341927";
    final String valinnanVaiheOid = "valinnanVaiheHakijanValinta";
    final String valintakoetunniste = "kielikoe_tunniste";

    ValintaperusteetDTO vv2 =
        luoValintaperusteetJaValintakoeValinnanvaihe(
            hakuOid, hakukohdeOid, valinnanVaiheOid, 1, valintakoetunniste);
    vv2.getValinnanVaihe().getValintakoe().stream()
        .filter(koe -> koe.getTunniste().equals(valintakoetunniste))
        .findFirst()
        .get()
        .setKutsunKohde(Koekutsu.HAKIJAN_VALINTA);
    vv2.getValinnanVaihe().getValintakoe().stream()
        .filter(koe -> koe.getTunniste().equals(valintakoetunniste))
        .findFirst()
        .get()
        .setKutsunKohdeAvain("hakukohdeKutsunKohde");

    AvainArvoDTO avain2 = new AvainArvoDTO();
    avain2.setAvain("hakukohdeKutsunKohde");
    avain2.setArvo(hakukohdeOid);
    final HakemusDTO hakemus = luoHakemus(hakuOid, hakemusOid, "hakijaOid", hakukohdeOid);
    hakemus.setAvaimet(Collections.singletonList(avain2));

    valintakoelaskentaSuorittajaService.laske(
        hakemus, Collections.singletonList(vv2), new HashMap<>(), uuid, kumulatiivisetTulokset, korkeakouluhaku);

    ValintakoeOsallistuminen osallistuminen =
        valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid);
    assertNotNull(osallistuminen);

    // todo: add asserts (work in progress)
  }

  @Test
  public void testViimeisinValinnanVaihe() {
    final String hakemusOid = "1.2.246.562.11.00000072753";
    final String hakukohdeOid = "1.2.246.562.5.91937845484";
    final String hakuOid = "1.2.246.562.5.2013080813081926341927";
    final String valinnanVaiheOid = "vv2";
    final String valintakoetunniste = "koe1";

    Valintatapajono jono = luoValintatapaJonoEntity(10,
      Sets.newHashSet(
        luoJonosijaEntity("Valtteri", "Villi", "1.2.246.562.11.00000072672", 5, false, List.of(luoJarjestyskriteeritulosEntity(0.0, 0, HYVAKSYTTAVISSA))),
        luoJonosijaEntity("Keijo", "Keskeyttänyt", hakemusOid, 1, false, List.of(luoJarjestyskriteeritulosEntity(0.0, 0, JarjestyskriteerituloksenTila.HYLATTY)))),
      "Harkinnanvaraisten käsittelyvaiheen valintatapajono", 0, Tasasijasaanto.ARVONTA, "1388739480159-1173947553521563587");

    Valinnanvaihe vaihe = luoValinnanvaiheEntity(hakuOid, hakukohdeOid, 0, "vv2", List.of(jono));
    valinnanvaiheRepository.save(vaihe);

    Hakutoive toive = luoHakutoiveEntity(hakukohdeOid,
      Sets.newHashSet(
        luoValintakoeValinnanvaiheEntity(1,
          "13887394798212581302211576347831",
          List.of(luoValintakoeEntity("13887394815186315041955335611484", "kielikoe_tunniste", EI_OSALLISTU, true, "HYVAKSYTTAVISSA")))));

    ValintakoeOsallistuminen osa = new ValintakoeOsallistuminen();
    osa.setEtunimi("Keijo");
    osa.setSukunimi("Keskeyttänyt");
    osa.setHakemusOid(hakemusOid);
    osa.setHakuOid(hakuOid);
    osa.setHakijaOid("akija");
    osa.setHakutoiveet(Sets.newHashSet(toive));
    valintakoeOsallistuminenRepository.save(osa);

    ValintaperusteetDTO vv2 =
        luoValintaperusteetJaValintakoeValinnanvaihe(
            hakuOid, hakukohdeOid, valinnanVaiheOid, 1, valintakoetunniste);
    valintakoelaskentaSuorittajaService.laske(
        luoHakemus(hakuOid, hakemusOid, hakemusOid, hakukohdeOid),
        Collections.singletonList(vv2),
        new HashMap<>(),
        uuid,
        kumulatiivisetTulokset,
        korkeakouluhaku);

    ValintakoeOsallistuminen osallistuminen =
        valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid);
    assertNotNull(osallistuminen);
    assertEquals(hakemusOid, osallistuminen.getHakemusOid());
    assertEquals(1, osallistuminen.getHakutoiveet().size());

    Hakutoive hakutoive = osallistuminen.getHakutoiveetAsList().get(0);
    assertEquals(hakukohdeOid, hakutoive.getHakukohdeOid());

    assertEquals(1, hakutoive.getValintakoeValinnanvaiheet().size());
    ValintakoeValinnanvaihe vv = hakutoive.getValintakoeValinnanvaiheetAsList().get(0);
    assertEquals(valinnanVaiheOid, vv.getValinnanvaiheOid());
    assertEquals(1, vv.getValintakokeet().size());

    Valintakoe koe = vv.getValintakokeetAsList().get(0);
    assertEquals(valintakoetunniste, koe.getValintakoeTunniste());
    assertEquals(EI_OSALLISTU, koe.getOsallistuminenTulos().getOsallistuminen());
  }

  @Test
  public void testViimeisinValinnanVaiheEnsimmainenHakutoiveHylatty() {
    // Testa
    final String hakemusOid = "1.2.246.562.11.00000072753";
    final String hakukohdeOid1 = "1.2.246.562.5.91937845484";
    final String hakukohdeOid2 = "1.2.246.562.5.91937845485";
    final String hakuOid = "1.2.246.562.5.2013080813081926341927";
    final String koekutsuvaiheOid_kohde1 = "vv2_kohde1";
    final String koekutsuvaiheOid_kohde2 = "vv2_kohde2";
    final String valintakoetunniste = "koetunniste";

    ValintaperusteetDTO vv2_kohde1 =
        luoValintaperusteetJaValintakoeValinnanVaihe(
            hakuOid,
            hakukohdeOid1,
            koekutsuvaiheOid_kohde1,
            1,
            createValintakokeet(valintakoetunniste),
            Koekutsu.YLIN_TOIVE,
            "hakukohdeKutsunKohde2");
    ValintaperusteetDTO vv2_kohde2 =
        luoValintaperusteetJaValintakoeValinnanVaihe(
            hakuOid,
            hakukohdeOid2,
            koekutsuvaiheOid_kohde2,
            1,
            createValintakokeet(valintakoetunniste),
            Koekutsu.YLIN_TOIVE,
            "hakukohdeKutsunKohde2");

    Valintatapajono jono1 = luoValintatapaJonoEntity(10,
      Sets.newHashSet(
        luoJonosijaEntity("Valtteri", "Villi", hakemusOid, 1, false, List.of(luoJarjestyskriteeritulosEntity(0.0, 0, JarjestyskriteerituloksenTila.HYLATTY)))),
      "Harkinnanvaraisten käsittelyvaiheen valintatapajono", 0, Tasasijasaanto.ARVONTA, "1388739480159-1173947553521563587");

    Valinnanvaihe vaihe1 = luoValinnanvaiheEntity(hakuOid, hakukohdeOid1, 0, "vv2_kohde1", List.of(jono1));
    valinnanvaiheRepository.save(vaihe1);

    Valintatapajono jono2 = luoValintatapaJonoEntity(10,
      Sets.newHashSet(
        luoJonosijaEntity("Valtteri", "Villi", hakemusOid, 2, false, List.of(luoJarjestyskriteeritulosEntity(0.0, 0, JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA)))),
      "Harkinnanvaraisten käsittelyvaiheen valintatapajono", 0, Tasasijasaanto.ARVONTA, "1388739480159-117394755352156358");

    Valinnanvaihe vaihe2 = luoValinnanvaiheEntity(hakuOid, hakukohdeOid2, 0, "vv2_kohde2", List.of(jono2));
    valinnanvaiheRepository.save(vaihe2);

    valintakoelaskentaSuorittajaService.laske(
        luoHakemus(hakuOid, hakemusOid, "hakija_oid", hakukohdeOid1, hakukohdeOid2),
        Collections.singletonList(vv2_kohde1),
        new HashMap<>(),
        uuid,
        kumulatiivisetTulokset,
        korkeakouluhaku);
    valintakoelaskentaSuorittajaService.laske(
        luoHakemus(hakuOid, hakemusOid, "hakija_oid", hakukohdeOid1, hakukohdeOid2),
        Collections.singletonList(vv2_kohde2),
        new HashMap<>(),
        uuid,
        kumulatiivisetTulokset,
        korkeakouluhaku);

    ValintakoeOsallistuminen osallistuminen =
        valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid);

    assertNotNull(osallistuminen);
    assertEquals(2, osallistuminen.getHakutoiveetAsList().size());
    Hakutoive toive1 = osallistuminen.getHakutoiveetAsList().stream().filter(t -> t.getHakukohdeOid().equals(hakukohdeOid1)).findFirst().orElseThrow();
    assertEquals(
        EI_OSALLISTU,
        toive1
            .getValintakoeValinnanvaiheetAsList()
            .get(0)
            .getValintakokeetAsList()
            .get(0)
            .getOsallistuminenTulos()
            .getOsallistuminen());

    Hakutoive toive2 = osallistuminen.getHakutoiveetAsList().stream().filter(t -> t.getHakukohdeOid().equals(hakukohdeOid2)).findFirst().orElseThrow();
    assertEquals(
        OSALLISTUU,
        toive2
            .getValintakoeValinnanvaiheetAsList()
            .get(0)
            .getValintakokeetAsList()
            .get(0)
            .getOsallistuminenTulos()
            .getOsallistuminen());
  }

  private Map<String, FunktiokutsuDTO> createValintakokeet(final String valintakoetunniste) {
    Map<String, FunktiokutsuDTO> kokeet1 = new HashMap<>();
    kokeet1.put(valintakoetunniste, totuusarvoTrue);
    return kokeet1;
  }

  @Test
  public void testOlemassaolevatKokoeet() throws JsonSyntaxException, IOException {

    Hakutoive toive = luoHakutoiveEntity("1.2.246.562.5.85532589612",
      Sets.newHashSet(
        luoValintakoeValinnanvaiheEntity(1,
          "1395127862819-2325484147265742138",
          List.of(
            luoValintakoeEntity("1395127863035-4341500380094392330", "kielikoe_fi", EI_OSALLISTU, false, "HYVAKSYTTAVISSA"),
            luoValintakoeEntity("1395127863037-3426800498277516554", "1_2_246_562_5_85532589612_urheilija_lisapiste", EI_OSALLISTU, false, "HYVAKSYTTAVISSA"),
            luoValintakoeEntity("1395127863046-7261761979692934156", "Eläintenhoidon koulutusohjelma, pk (Maatalousalan perustutkinto), pääsy- ja soveltuvuuskoe", OSALLISTUU, true, "HYVAKSYTTAVISSA")))));
    Hakutoive toive2 = luoHakutoiveEntity("1.2.246.562.5.37009438716",
      Sets.newHashSet(
        luoValintakoeValinnanvaiheEntity(1,
          "1395127824348-1049605647060825146",
          List.of(
            luoValintakoeEntity("1401778607536-1848915787886635271", "Eläintenhoidon koulutusohjelma, pk (Maatalousalan perustutkinto), pääsy- ja soveltuvuuskoe", OSALLISTUU, true, "HYVAKSYTTAVISSA")))));

    ValintakoeOsallistuminen osa = new ValintakoeOsallistuminen();
    osa.setEtunimi("Jenna X");
    osa.setSukunimi("Alavirta");
    osa.setHakemusOid("1.2.246.562.11.00000304421");
    osa.setHakuOid("1.2.246.562.5.2013080813081926341927");
    osa.setHakijaOid("1.2.246.562.24.30568204729");
    osa.setHakutoiveet(Sets.newHashSet(toive, toive2));
    valintakoeOsallistuminenRepository.save(osa);

    LaskeDTO dto = readJson("laskeDTO.json", new TypeToken<LaskeDTO>() {});

    valintakoelaskentaSuorittajaService.laske(
        dto.getHakemus().get(0),
        dto.getValintaperuste(),
        new HashMap<>(),
        uuid,
        kumulatiivisetTulokset,
        korkeakouluhaku);

    ValintakoeOsallistuminen osallistuminen =
        valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(
            "1.2.246.562.5.2013080813081926341927", "1.2.246.562.11.00000304421");

    assertTrue(osallistuminen.getHakutoiveet().size() == 2);

    osallistuminen.getHakutoiveetAsList().sort(Comparator.comparing(Hakutoive::getHakukohdeOid));

    assertTrue(
        osallistuminen.getHakutoiveetAsList().get(0).getValintakoeValinnanvaiheetAsList().get(0).getValintakokeet().size()
            == 3);
    assertTrue(
        osallistuminen.getHakutoiveetAsList().get(1).getValintakoeValinnanvaiheetAsList().get(0).getValintakokeet().size()
            == 3);

    osallistuminen
        .getHakutoiveetAsList()
        .get(0)
        .getValintakoeValinnanvaiheetAsList()
        .get(0)
        .getValintakokeetAsList()
        .sort(Comparator.comparing(Valintakoe::getValintakoeTunniste));
    osallistuminen
        .getHakutoiveetAsList()
        .get(1)
        .getValintakoeValinnanvaiheetAsList()
        .get(0)
        .getValintakokeetAsList()
        .sort(Comparator.comparing(Valintakoe::getValintakoeTunniste));

    assertEquals(
        OSALLISTUU,
        osallistuminen
            .getHakutoiveetAsList()
            .get(1)
            .getValintakoeValinnanvaiheetAsList()
            .get(0)
            .getValintakokeetAsList()
            .get(1)
            .getOsallistuminenTulos()
            .getOsallistuminen());
    assertEquals(
        EI_OSALLISTU,
        osallistuminen
            .getHakutoiveetAsList()
            .get(0)
            .getValintakoeValinnanvaiheetAsList()
            .get(0)
            .getValintakokeetAsList()
            .get(1)
            .getOsallistuminenTulos()
            .getOsallistuminen());
  }

  @Test
  public void kielikokeeseenKutsutaanJosSuoritustaTaiTodennettuaKielitaitoaEiLoydy()
      throws JsonSyntaxException, IOException {
    LaskeDTO laskeDTOIlmanKielikoetulosta =
        readJson("laskeDTOIlmanKielikoetulosta.json", new TypeToken<LaskeDTO>() {});

    valintakoelaskentaSuorittajaService.laske(
        laskeDTOIlmanKielikoetulosta.getHakemus().get(0),
        laskeDTOIlmanKielikoetulosta.getValintaperuste(),
        new HashMap<>(),
        uuid,
        kumulatiivisetTulokset,
        korkeakouluhaku);

    ValintakoeOsallistuminen osallistuminen =
        valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(
            "1.2.246.562.5.2013080813081926341927", "1.2.246.562.11.00000304421");

    assertEquals(1, osallistuminen.getHakutoiveet().size());

    Hakutoive osallistumisenHakutoiveJohonOnKielikoe = osallistuminen.getHakutoiveetAsList().get(0);
    ValintakoeValinnanvaihe kielikokeenPakollisuusVaihe =
        osallistumisenHakutoiveJohonOnKielikoe.getValintakoeValinnanvaiheetAsList().get(0);
    assertEquals(1, kielikokeenPakollisuusVaihe.getValintakokeet().size());

    Valintakoe kielikoetulos = kielikokeenPakollisuusVaihe.getValintakokeetAsList().get(0);
    OsallistuminenTulos kielikoetulosOsallistuminenTulos = kielikoetulos.getOsallistuminenTulos();

    assertEquals(OSALLISTUU, kielikoetulosOsallistuminenTulos.getOsallistuminen());
    assertTrue(kielikoetulosOsallistuminenTulos.getLaskentaTulos());
    assertEquals(HYVAKSYTTAVISSA.name(), kielikoetulosOsallistuminenTulos.getLaskentaTila());
  }

  @Test
  public void kielikokeeseenKutsutaanJosOsallistuminenLoytyyKyseiseltaHakemukselta()
      throws JsonSyntaxException, IOException {
    LaskeDTO laskeDTOIlmanKielikoetulosta =
        readJson("laskeDTOIlmanKielikoetulosta.json", new TypeToken<LaskeDTO>() {});
    HakemusDTO hakemus = laskeDTOIlmanKielikoetulosta.getHakemus().get(0);
    setValueOnCombinedHakemusData(hakemus, "kielikoe_fi", "true");
    setValueOnCombinedHakemusData(hakemus, "kielikoe_fi-OSALLISTUMINEN", "OSALLISTUI");

    valintakoelaskentaSuorittajaService.laske(
        hakemus,
        laskeDTOIlmanKielikoetulosta.getValintaperuste(),
        new HashMap<>(),
        uuid,
        kumulatiivisetTulokset,
        korkeakouluhaku);

    ValintakoeOsallistuminen osallistuminen =
        valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(
            "1.2.246.562.5.2013080813081926341927", "1.2.246.562.11.00000304421");

    assertEquals(1, osallistuminen.getHakutoiveet().size());

    Hakutoive osallistumisenHakutoiveJohonOnKielikoe = osallistuminen.getHakutoiveetAsList().get(0);
    ValintakoeValinnanvaihe kielikokeenPakollisuusVaihe =
        osallistumisenHakutoiveJohonOnKielikoe.getValintakoeValinnanvaiheetAsList().get(0);
    assertEquals(1, kielikokeenPakollisuusVaihe.getValintakokeet().size());

    Valintakoe kielikoetulos = kielikokeenPakollisuusVaihe.getValintakokeetAsList().get(0);
    OsallistuminenTulos kielikoetulosOsallistuminenTulos = kielikoetulos.getOsallistuminenTulos();

    assertEquals(OSALLISTUU, kielikoetulosOsallistuminenTulos.getOsallistuminen());
    assertTrue(kielikoetulosOsallistuminenTulos.getLaskentaTulos());
    assertEquals(HYVAKSYTTAVISSA.name(), kielikoetulosOsallistuminenTulos.getLaskentaTila());
  }

  @Test
  public void kielikokeeseenEiKutsutaJosSuoritusLoytyyEriHakemukselta()
      throws JsonSyntaxException, IOException {
    LaskeDTO laskeDTOIlmanKielikoetulosta =
        readJson("laskeDTOIlmanKielikoetulosta.json", new TypeToken<LaskeDTO>() {});
    HakemusDTO hakemus = laskeDTOIlmanKielikoetulosta.getHakemus().get(0);
    setValueOnCombinedHakemusData(hakemus, "kielikoe_fi", "true");
    setValueOnCombinedHakemusData(hakemus, "kielikoe_fi-OSALLISTUMINEN", "MERKITSEMATTA");

    valintakoelaskentaSuorittajaService.laske(
        hakemus,
        laskeDTOIlmanKielikoetulosta.getValintaperuste(),
        new HashMap<>(),
        uuid,
        kumulatiivisetTulokset,
        korkeakouluhaku);

    ValintakoeOsallistuminen osallistuminen =
        valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(
            "1.2.246.562.5.2013080813081926341927", "1.2.246.562.11.00000304421");

    assertEquals(1, osallistuminen.getHakutoiveet().size());

    Hakutoive osallistumisenHakutoiveJohonOnKielikoe = osallistuminen.getHakutoiveetAsList().get(0);
    ValintakoeValinnanvaihe kielikokeenPakollisuusVaihe =
        osallistumisenHakutoiveJohonOnKielikoe.getValintakoeValinnanvaiheetAsList().get(0);
    assertEquals(1, kielikokeenPakollisuusVaihe.getValintakokeet().size());

    Valintakoe kielikoetulos = kielikokeenPakollisuusVaihe.getValintakokeetAsList().get(0);
    OsallistuminenTulos kielikoetulosOsallistuminenTulos = kielikoetulos.getOsallistuminenTulos();

    assertEquals(EI_OSALLISTU, kielikoetulosOsallistuminenTulos.getOsallistuminen());
    assertFalse(kielikoetulosOsallistuminenTulos.getLaskentaTulos());
    assertEquals(HYVAKSYTTAVISSA.name(), kielikoetulosOsallistuminenTulos.getLaskentaTila());
  }

  private <T> T readJson(String pathInClasspath, TypeToken<T> typeToken) throws IOException {
    return new Gson()
        .fromJson(
            IOUtils.toString(new ClassPathResource(pathInClasspath).getInputStream()),
            typeToken.getType());
  }

  private <T> T readJsonFromSamePackage(
      Class<?> clazz, String nameInSamePackage, TypeToken<T> typeToken) throws IOException {
    return new Gson()
        .fromJson(
            IOUtils.toString(
                new ClassPathResource(nameInSamePackage, clazz).getInputStream(), "UTF-8"),
            typeToken.getType());
  }

  @Test
  public void testMukanaKokeessaToisessaKohteessa() {
    final String hakemusOid = "1.2.246.562.11.00001212279";
    final String hakukohdeOid = "1.2.246.562.20.66128426039";
    final String hakuOid = "1.2.246.562.29.173465377510";
    final String hakemusOid2 = "1.2.246.562.11.00001223556";

    Hakutoive toive = luoHakutoiveEntity(hakukohdeOid,
      Sets.newHashSet(
        luoValintakoeValinnanvaiheEntity(2,
          "1395127824348-1049605647060825146",
          List.of(
            luoValintakoeEntity("1395127863037-3426800498277516554", "SOTE1_kaikkiosiot", EI_OSALLISTU, null, "HYLATTY"),
            luoValintakoeEntity("1401778607536-1848915787886635271", "SOTEKOE_VK_RYHMA1", EI_OSALLISTU, null, "HYLATTY")))));

    Hakutoive toive2 = luoHakutoiveEntity("1.2.246.562.5.37009438716",
      Sets.newHashSet(
        luoValintakoeValinnanvaiheEntity(2,
          "1395127862819-2325484147265742138",
          List.of(
            luoValintakoeEntity("1395127863046-7261761979692934156", "SOTE1_kaikkiosiot", OSALLISTUU, true, "HYVAKSYTTAVISSA")))));


    ValintakoeOsallistuminen osa = new ValintakoeOsallistuminen();
    osa.setEtunimi("Ruhtinas");
    osa.setSukunimi("Nukettaja");
    osa.setHakemusOid(hakemusOid);
    osa.setHakuOid(hakuOid);
    osa.setHakijaOid("1.2.246.562.24.30568204729");
    osa.setHakutoiveet(Sets.newHashSet(toive, toive2));
    valintakoeOsallistuminenRepository.save(osa);

    Hakutoive toive3 = luoHakutoiveEntity(hakukohdeOid,
      Sets.newHashSet(
        luoValintakoeValinnanvaiheEntity(2,
          "1395127824348-1049605647060825149",
          List.of(
            luoValintakoeEntity("1395127863037-3426800498277516554", "SOTE1_kaikkiosiot", EI_OSALLISTU, null, "HYLATTY"),
            luoValintakoeEntity("1401778607536-1848915787886635271", "SOTEKOE_VK_RYHMA1", EI_OSALLISTU, null, "HYLATTY")))));

    ValintakoeOsallistuminen osa2 = new ValintakoeOsallistuminen();
    osa2.setEtunimi("Jenna X");
    osa2.setSukunimi("Alavirta");
    osa2.setHakemusOid(hakemusOid2);
    osa2.setHakuOid(hakuOid);
    osa2.setHakijaOid("1.2.246.562.24.30568204729");
    osa2.setHakutoiveet(Sets.newHashSet(toive3));
    valintakoeOsallistuminenRepository.save(osa2);

    ValintakoeOsallistuminen osallistuminen =
        valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid);
    assertNotNull(osallistuminen);
    boolean voidaanHyvaksya =
        edellinenValinnanvaiheKasittelija.koeOsallistuminenToisessaKohteessa(
            hakukohdeOid, osallistuminen);
    assertTrue(voidaanHyvaksya);

    osallistuminen = valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid2);
    assertNotNull(osallistuminen);
    voidaanHyvaksya =
        edellinenValinnanvaiheKasittelija.koeOsallistuminenToisessaKohteessa(
            hakukohdeOid, osallistuminen);
    assertFalse(voidaanHyvaksya);
  }

  @Test
  public void testMukanaYhdessaMutteiKaikissaKokeissaToisessaKohteessa() {
    final String hakemusOid = "1.2.246.562.11.00001212279";
    final String hakukohdeOid = "1.2.246.562.20.66128426039";
    final String hakuOid = "1.2.246.562.29.173465377510";

    ValintakoeOsallistuminen osallistuminen =
        valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid);
    assertNotNull(osallistuminen);

    List<Valintakoe> kohteenValintakokeet =
        osallistuminen.getHakutoiveet().stream()
            .filter(h -> h.getHakukohdeOid().equals(hakukohdeOid))
            .flatMap(h -> h.getValintakoeValinnanvaiheet().stream())
            .flatMap(v -> v.getValintakokeet().stream())
            .sorted(Comparator.comparing(Valintakoe::getValintakoeTunniste))
            .collect(Collectors.toList());

    assertThat(kohteenValintakokeet, hasSize(3));
    assertThat(
        kohteenValintakokeet.stream().map(Valintakoe::getValintakoeTunniste),
        StreamMatchers.contains(
            "KOHTEEN_SPESIFI_KOE_BUG-1564", "SOTE1_kaikkiosiot", "SOTEKOE_VK_RYHMA1"));

    Map<Osallistuminen, List<Valintakoe>> kohteenKokeetOsallistumisenMukaan =
        kohteenValintakokeet.stream()
            .collect(groupingBy(k -> k.getOsallistuminenTulos().getOsallistuminen()));
    assertThat(kohteenKokeetOsallistumisenMukaan.get(OSALLISTUU), hasSize(1));
    assertThat(
        kohteenKokeetOsallistumisenMukaan.get(OSALLISTUU).stream()
            .map(Valintakoe::getValintakoeTunniste),
        StreamMatchers.contains("KOHTEEN_SPESIFI_KOE_BUG-1564"));
    assertThat(kohteenKokeetOsallistumisenMukaan.get(EI_OSALLISTU), hasSize(2));
    assertThat(
        kohteenKokeetOsallistumisenMukaan.get(EI_OSALLISTU).stream()
            .map(Valintakoe::getValintakoeTunniste),
        StreamMatchers.contains("SOTE1_kaikkiosiot", "SOTEKOE_VK_RYHMA1"));

    List<Valintakoe> muidenkohteidenKokeetJoihinOsallistutaan =
        osallistuminen.getHakutoiveet().stream()
            .filter(h -> !h.getHakukohdeOid().equals(hakukohdeOid))
            .flatMap(h -> h.getValintakoeValinnanvaiheet().stream())
            .flatMap(v -> v.getValintakokeet().stream())
            .filter(k -> OSALLISTUU.equals(k.getOsallistuminenTulos().getOsallistuminen()))
            .sorted(Comparator.comparing(Valintakoe::getValintakoeTunniste))
            .collect(Collectors.toList());

    assertThat(muidenkohteidenKokeetJoihinOsallistutaan, hasSize(2));
    assertThat(
        muidenkohteidenKokeetJoihinOsallistutaan.stream().map(Valintakoe::getValintakoeTunniste),
        StreamMatchers.contains("SOTE1_kaikkiosiot", "SOTEKOE_VK_RYHMA1"));
    assertThat(
        muidenkohteidenKokeetJoihinOsallistutaan.stream().map(Valintakoe::getValintakoeTunniste),
        not(StreamMatchers.contains("KOHTEEN_SPESIFI_KOE_BUG-1564")));

    assertTrue(
        edellinenValinnanvaiheKasittelija.koeOsallistuminenToisessaKohteessa(
            hakukohdeOid, osallistuminen));
  }

  @Test
  public void bug1564KutsuttavaKohdekohtaiseenKokeeseen() throws IOException {
    final String hakemusOid = "1.2.246.562.11.00009176948";
    final String hakukohdeOidJossaOmaKoe = "1.2.246.562.20.80972757381";
    final String ylempiHakukohdeOidJossaYhteinenKoe = "1.2.246.562.20.68517235666";
    final String hakuOid = "1.2.246.562.29.59856749474";

    ArrayList<ValintaperusteetDTO> perusteetKohde1 =
        readJsonFromSamePackage(
            getClass(),
            "bug1564-valintaperusteet-ylempi.json",
            new TypeToken<ArrayList<ValintaperusteetDTO>>() {});
    ArrayList<ValintaperusteetDTO> perusteetKohde2 =
        readJsonFromSamePackage(
            getClass(),
            "bug1564-valintaperusteet.json",
            new TypeToken<ArrayList<ValintaperusteetDTO>>() {});

    HakemusDTO hakemus =
        luoHakemus(
            hakuOid,
            hakemusOid,
            hakemusOid,
            ylempiHakukohdeOidJossaYhteinenKoe,
            hakukohdeOidJossaOmaKoe);
    valintakoelaskentaSuorittajaService.laske(
        hakemus,
        perusteetKohde1,
        new HashMap<>(),
        uuid,
        new ValintakoelaskennanKumulatiivisetTulokset(),
        korkeakouluhaku);
    valintakoelaskentaSuorittajaService.laske(
        hakemus,
        perusteetKohde2,
        new HashMap<>(),
        uuid,
        new ValintakoelaskennanKumulatiivisetTulokset(),
        korkeakouluhaku);
    valintakoelaskentaSuorittajaService.laske(
        hakemus,
        perusteetKohde2,
        new HashMap<>(),
        uuid,
        new ValintakoelaskennanKumulatiivisetTulokset(),
        korkeakouluhaku); // again, to get previous results in place...

    ValintakoeOsallistuminen osallistuminen =
        valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid);

    assertNotNull(osallistuminen);

    List<Valintakoe> ylemmankohteenValintakokeet =
        osallistuminen.getHakutoiveet().stream()
            .filter(h -> h.getHakukohdeOid().equals(ylempiHakukohdeOidJossaYhteinenKoe))
            .flatMap(h -> h.getValintakoeValinnanvaiheet().stream())
            .flatMap(v -> v.getValintakokeet().stream())
            .sorted(Comparator.comparing(Valintakoe::getValintakoeTunniste))
            .collect(Collectors.toList());

    assertThat(ylemmankohteenValintakokeet, hasSize(2));
    assertThat(
        ylemmankohteenValintakokeet.stream().map(Valintakoe::getValintakoeTunniste),
        StreamMatchers.contains("Sote3_pakollinen_osio", "Sote3_valintakoe"));

    Map<Osallistuminen, List<Valintakoe>> ylemmanKohteenKokeetOsallistumisenMukaan =
        ylemmankohteenValintakokeet.stream()
            .collect(groupingBy(k -> k.getOsallistuminenTulos().getOsallistuminen()));
    assertThat(ylemmanKohteenKokeetOsallistumisenMukaan.get(OSALLISTUU), hasSize(2));
    assertThat(ylemmanKohteenKokeetOsallistumisenMukaan.keySet(), hasSize(1));
    assertThat(ylemmanKohteenKokeetOsallistumisenMukaan.keySet(), hasItem(OSALLISTUU));
    assertThat(
        ylemmanKohteenKokeetOsallistumisenMukaan.get(OSALLISTUU).stream()
            .map(Valintakoe::getValintakoeTunniste),
        StreamMatchers.contains("Sote3_pakollinen_osio", "Sote3_valintakoe"));

    List<Valintakoe> kohteenJossaOmaKoeValintakokeet =
        osallistuminen.getHakutoiveet().stream()
            .filter(h -> h.getHakukohdeOid().equals(hakukohdeOidJossaOmaKoe))
            .flatMap(h -> h.getValintakoeValinnanvaiheet().stream())
            .flatMap(v -> v.getValintakokeet().stream())
            .sorted(Comparator.comparing(Valintakoe::getValintakoeTunniste))
            .collect(Collectors.toList());

    assertThat(kohteenJossaOmaKoeValintakokeet, hasSize(4));
    assertThat(
        kohteenJossaOmaKoeValintakokeet.stream().map(Valintakoe::getValintakoeTunniste),
        StreamMatchers.contains(
            "Sote3_pakollinen_osio",
            "Sote3_valintakoe",
            "amk_kielikoe_2017_suomi",
            "mikon-testikoe-BUG-1564"));

    Map<Osallistuminen, List<Valintakoe>> kohteenJossaOmaKoeKokeetOsallistumisenMukaan =
        kohteenJossaOmaKoeValintakokeet.stream()
            .collect(groupingBy(k -> k.getOsallistuminenTulos().getOsallistuminen()));
    assertThat(kohteenJossaOmaKoeKokeetOsallistumisenMukaan.get(OSALLISTUU), hasSize(2));
    assertThat(
        kohteenJossaOmaKoeKokeetOsallistumisenMukaan.get(OSALLISTUU).stream()
            .map(Valintakoe::getValintakoeTunniste),
        StreamMatchers.contains("amk_kielikoe_2017_suomi", "mikon-testikoe-BUG-1564"));
    assertThat(kohteenJossaOmaKoeKokeetOsallistumisenMukaan.get(EI_OSALLISTU), hasSize(2));
    assertThat(
        kohteenJossaOmaKoeKokeetOsallistumisenMukaan.get(EI_OSALLISTU).stream()
            .map(Valintakoe::getValintakoeTunniste),
        StreamMatchers.contains("Sote3_pakollinen_osio", "Sote3_valintakoe"));
  }

  private Predicate<Valintakoe> koeWithTunniste(String tunniste) {
    return k -> k.getValintakoeTunniste().equals(tunniste);
  }

  private void setValueOnCombinedHakemusData(HakemusDTO hakemus, String avain, String arvo) {
    hakemus.getAvaimet().stream()
        .filter(a -> a.getAvain().equals(avain))
        .forEach(a -> a.setArvo(arvo));
  }
}
