package fi.vm.sade.valintalaskenta.tulos.service;

import static org.junit.jupiter.api.Assertions.*;

import fi.vm.sade.valintalaskenta.domain.dto.*;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.*;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.ValintatietoValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.testdata.TestEntityDataUtil;
import fi.vm.sade.valintalaskenta.domain.valinta.*;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.laskenta.testdata.TestDataUtil;
import fi.vm.sade.valintalaskenta.testing.AbstractMocklessIntegrationTest;
import java.math.BigDecimal;
import java.util.*;
import java.util.function.Function;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;

public class ValintalaskentaTulosServiceTest extends AbstractMocklessIntegrationTest {

  public static final String VALINTATAPAJONO_OID = "jono1";
  public static final String HAKEMUS_OID = "1.2.246.562.11.00001128774";
  @Autowired private ValintalaskentaTulosService valintalaskentaTulosService;

  @Autowired private ApplicationContext applicationContext;

  @Test
  public void haeValintakoeOsallistumisetByOidTest() {
    valintakoeOsallistuminenRepository.save(
        TestEntityDataUtil.luoValintakoeOsallistuminen("hakuOid1", "hakijaoid1", "oid1"));
    ValintakoeOsallistuminen kaikki =
        valintalaskentaTulosService.haeValintakoeOsallistumiset("oid1");

    assertEquals("hakijaoid1", kaikki.getHakijaOid());
  }

  @Test
  public void haeValintakoeOsallistumisetByHakutoiveTest() {
    valintakoeOsallistuminenRepository.save(
        TestEntityDataUtil.luoValintakoeOsallistuminen(
            "hakuoid",
            "hakijaoid1",
            "hakemusoid1",
            Set.of(TestEntityDataUtil.luoHakutoiveEntity("oid1", new HashSet<>()))));
    valintakoeOsallistuminenRepository.save(
        TestEntityDataUtil.luoValintakoeOsallistuminen(
            "hakuoid",
            "hakijaoid2",
            "hakemusoid2",
            Set.of(
                TestEntityDataUtil.luoHakutoiveEntity("oid1", new HashSet<>()),
                TestEntityDataUtil.luoHakutoiveEntity("oid2", new HashSet<>()))));
    List<ValintakoeOsallistuminen> kaikki =
        valintalaskentaTulosService.haeValintakoeOsallistumisetByHakutoive("oid1");
    assertEquals(2, kaikki.size());
    kaikki = valintalaskentaTulosService.haeValintakoeOsallistumisetByHakutoive("oid2");
    assertEquals(1, kaikki.size());
  }

  @Test
  public void haeLasketutuValintavaiheetHaulleAndConvertTest() {
    luoValintatapajonojaTasasijoilla();

    final String hakuOid = "haku1";

    List<HakukohdeDTO> hakukohdeDTOs =
        valintalaskentaTulosService.haeLasketutValinnanvaiheetHaulle(hakuOid);

    assertEquals(1, hakukohdeDTOs.size());
    HakukohdeDTO hakukohdeDTO = hakukohdeDTOs.get(0);
    assertEquals(0, hakukohdeDTO.getPrioriteetti());

    final int convertedPrioriteetti = 15;
    Function<HakukohdeDTO, HakukohdeDTO> testConverter =
        (HakukohdeDTO source) -> {
          HakukohdeDTO result = new HakukohdeDTO();
          result.setPrioriteetti(convertedPrioriteetti);
          return result;
        };

    List<HakukohdeDTO> convertedHakukohdeDTOs =
        valintalaskentaTulosService
            .haeLasketutValinnanvaiheetHaulle(hakuOid, testConverter)
            .toList();

    assertEquals(1, convertedHakukohdeDTOs.size());
    HakukohdeDTO convertedHakukohdeDTO = convertedHakukohdeDTOs.get(0);
    assertEquals(convertedPrioriteetti, convertedHakukohdeDTO.getPrioriteetti());
  }

  @Test
  public void testHaeValintakoevirheetHaulle() {
    final String hakuOid = "hakuOid1";
    valintakoeOsallistuminenRepository.save(
        TestEntityDataUtil.luoValintakoeOsallistuminen(
            hakuOid,
            "hakijaoid1",
            "hakemusOid1",
            Set.of(
                TestEntityDataUtil.luoHakutoiveEntity(
                    "hakukohdeOid1",
                    Set.of(
                        TestEntityDataUtil.luoValintakoeValinnanvaiheEntity(
                            0,
                            "valinnanvaheOid1",
                            List.of(
                                TestEntityDataUtil.luoValintakoeEntity(
                                    "valintakoeOid1",
                                    "valintakoeTunniste1",
                                    Osallistuminen.VIRHE,
                                    false,
                                    null))),
                        TestEntityDataUtil.luoValintakoeValinnanvaiheEntity(
                            0,
                            "valinnanvaiheOid2",
                            List.of(
                                (TestEntityDataUtil.luoValintakoeEntity(
                                    "valintakoeOid2",
                                    "valintakoeTunniste2",
                                    Osallistuminen.OSALLISTUU,
                                    false,
                                    null)))))))));

    valintakoeOsallistuminenRepository.save(
        TestEntityDataUtil.luoValintakoeOsallistuminen(
            hakuOid,
            "hakijaoid2",
            "hakemusOid2",
            Set.of(
                TestEntityDataUtil.luoHakutoiveEntity(
                    "hakukohdeOid1",
                    Set.of(
                        TestEntityDataUtil.luoValintakoeValinnanvaiheEntity(
                            0,
                            "valinnanvaheOid1",
                            List.of(
                                TestEntityDataUtil.luoValintakoeEntity(
                                    "valintakoeOid1",
                                    "valintakoeTunniste1",
                                    Osallistuminen.OSALLISTUU,
                                    false,
                                    null))))))));

    valintakoeOsallistuminenRepository.save(
        TestEntityDataUtil.luoValintakoeOsallistuminen(
            "hakuOid2",
            "hakijaoid1",
            "hakemusOid3",
            Set.of(
                TestEntityDataUtil.luoHakutoiveEntity(
                    "hakukohdeOid1",
                    Set.of(
                        TestEntityDataUtil.luoValintakoeValinnanvaiheEntity(
                            0,
                            "valinnanvaheOid1",
                            List.of(
                                TestEntityDataUtil.luoValintakoeEntity(
                                    "valintakoeOid1",
                                    "valintakoeTunniste1",
                                    Osallistuminen.VIRHE,
                                    false,
                                    null))))))));

    List<ValintakoeOsallistuminenDTO> osallistumiset =
        valintalaskentaTulosService.haeValintakoevirheetHaulle(hakuOid);
    assertEquals(1, osallistumiset.size());
    ValintakoeOsallistuminenDTO vko = osallistumiset.get(0);
    assertEquals(hakuOid, vko.getHakuOid());
    assertEquals(1, vko.getHakutoiveet().size());

    HakutoiveDTO ht = vko.getHakutoiveet().get(0);
    assertEquals(1, ht.getValinnanVaiheet().size());

    ValintakoeValinnanvaiheDTO vv = ht.getValinnanVaiheet().get(0);
    assertEquals(1, vv.getValintakokeet().size());

    ValintakoeDTO vk = vv.getValintakokeet().get(0);
    assertEquals(Osallistuminen.VIRHE, vk.getOsallistuminenTulos().getOsallistuminen());
  }

  @Test
  public void testHaeTuloksetHakemukselle() {
    valinnanvaiheRepository.save(
        TestEntityDataUtil.luoValinnanvaiheEntity(
            "hakuOid1",
            "hakukohdeOid1",
            0,
            "valinnanvaiheOid1",
            Arrays.asList(
                TestEntityDataUtil.luoValintatapaJonoEntity(
                    10,
                    Set.of(
                        TestEntityDataUtil.luoJonosijaEntity(
                            "hakemusOid1",
                            0,
                            false,
                            Arrays.asList(
                                TestEntityDataUtil.luoJarjestyskriteeritulosEntity(
                                    13.0, 0, JarjestyskriteerituloksenTila.HYLATTY),
                                TestEntityDataUtil.luoJarjestyskriteeritulosEntity(
                                    5.0, 1, JarjestyskriteerituloksenTila.HYLATTY))),
                        TestEntityDataUtil.luoJonosijaEntity(
                            "hakemusOid2",
                            0,
                            false,
                            Arrays.asList(
                                TestEntityDataUtil.luoJarjestyskriteeritulosEntity(
                                    20.0, 0, JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA),
                                TestEntityDataUtil.luoJarjestyskriteeritulosEntity(
                                    10.0, 1, JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA)))),
                    "valintatapajonoOid1",
                    0,
                    null,
                    "valintatapajonoOid1"))));

    MuokattuJonosija mjs = new MuokattuJonosija();
    mjs.setHakuOid("hakuOid1");
    mjs.setHakukohdeOid("hakukohdeOid1");
    mjs.setValintatapajonoOid("valintatapajonoOid1");
    mjs.setHakemusOid("hakemusOid1");
    mjs.setPrioriteetti(0);
    mjs.setHarkinnanvarainen(true);
    mjs.setMuokkaaja("muokkaajaOid");
    mjs.setJarjestyskriteerit(
        Set.of(
            TestEntityDataUtil.luoJarjestyskriteeritulosEntity(
                100.0, 0, JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA),
            TestEntityDataUtil.luoJarjestyskriteeritulosEntity(
                5.0, 1, JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA)));
    tulosMuokattuJonosijaRepository.save(mjs);

    HarkinnanvarainenHyvaksyminen hyvaksyminen = new HarkinnanvarainenHyvaksyminen();
    hyvaksyminen.setHarkinnanvaraisuusTila(HarkinnanvaraisuusTila.HYVAKSYTTY);
    hyvaksyminen.setHakukohdeOid("hakukohdeOid1");
    hyvaksyminen.setHakuOid("hakuOid1");
    hyvaksyminen.setHakemusOid("hakemusOid2");
    tulosHarkinnanvarainenHyvaksyminenRepository.save(hyvaksyminen);

    {
      HakemusDTO hakemus =
          valintalaskentaTulosService.haeTuloksetHakemukselle("hakuOid1", "hakemusOid1");
      assertEquals(1, hakemus.getHakukohteet().size());
      HakukohdeDTO hakukohde = hakemus.getHakukohteet().get(0);
      assertEquals(1, hakukohde.getValinnanvaihe().size());
      ValinnanvaiheDTO vv = hakukohde.getValinnanvaihe().get(0);
      assertEquals(1, vv.getValintatapajonot().size());
      ValintatapajonoDTO valintatapajono = vv.getValintatapajonot().get(0);
      assertEquals(1, valintatapajono.getJonosijat().size());
      JonosijaDTO jonosija = valintatapajono.getJonosijat().get(0);
      assertEquals(2, jonosija.getJarjestyskriteerit().size());
      assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jonosija.getTuloksenTila());

      Iterator<JarjestyskriteeritulosDTO> i = jonosija.getJarjestyskriteerit().iterator();
      JarjestyskriteeritulosDTO kriteeri1 = i.next();
      assertEquals(0, kriteeri1.getPrioriteetti());
      assertEquals(new BigDecimal("100"), kriteeri1.getArvo());
      assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, kriteeri1.getTila());

      JarjestyskriteeritulosDTO kriteeri2 = i.next();
      assertEquals(1, kriteeri2.getPrioriteetti());
      assertEquals(new BigDecimal("5"), kriteeri2.getArvo());
      assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, kriteeri2.getTila());
    }

    {
      HakemusDTO hakemus =
          valintalaskentaTulosService.haeTuloksetHakemukselle("hakuOid1", "hakemusOid2");
      assertEquals(1, hakemus.getHakukohteet().size());
      HakukohdeDTO hakukohde = hakemus.getHakukohteet().get(0);
      assertEquals(1, hakukohde.getValinnanvaihe().size());
      ValinnanvaiheDTO vv = hakukohde.getValinnanvaihe().get(0);
      assertEquals(1, vv.getValintatapajonot().size());
      ValintatapajonoDTO valintatapajono = vv.getValintatapajonot().get(0);
      assertEquals(1, valintatapajono.getJonosijat().size());
      JonosijaDTO jonosija = valintatapajono.getJonosijat().get(0);
      assertEquals(2, jonosija.getJarjestyskriteerit().size());
      assertEquals(
          JarjestyskriteerituloksenTila.HYVAKSYTTY_HARKINNANVARAISESTI, jonosija.getTuloksenTila());

      Iterator<JarjestyskriteeritulosDTO> i = jonosija.getJarjestyskriteerit().iterator();
      JarjestyskriteeritulosDTO kriteeri1 = i.next();
      assertEquals(0, kriteeri1.getPrioriteetti());
      assertEquals(new BigDecimal("20"), kriteeri1.getArvo());
      assertEquals(
          JarjestyskriteerituloksenTila.HYVAKSYTTY_HARKINNANVARAISESTI, kriteeri1.getTila());

      JarjestyskriteeritulosDTO kriteeri2 = i.next();
      assertEquals(1, kriteeri2.getPrioriteetti());
      assertEquals(new BigDecimal("10"), kriteeri2.getArvo());
      assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, kriteeri2.getTila());
    }
  }

  @Test
  public void testHaeTuloksetHakukohteelle() {
    luoValintatapajonojaTasasijoilla();

    List<ValintatietoValinnanvaiheDTO> hakukohde =
        valintalaskentaTulosService.haeValinnanvaiheetHakukohteelle("hakukohde1");
    assertEquals(1, hakukohde.size());
    assertEquals(1, hakukohde.get(0).getValintatapajonot().size());
    List<JonosijaDTO> jonosijat = hakukohde.get(0).getValintatapajonot().get(0).getJonosijat();
    jonosijat.stream()
        .filter(h -> h.getHakemusOid().contains("Pitäis olla tasasijalla"))
        .forEach(
            h -> {
              assertEquals(1, h.getJonosija());
            });
    jonosijat.stream()
        .filter(h -> !h.getHakemusOid().contains("Pitäis olla tasasijalla"))
        .forEach(
            h -> {
              assertNotEquals(1, h.getJonosija());
            });
  }

  @Test
  public void testMuokkaaJonosijanJarjestyskriteeria() {
    try (MockedStatic<AuthorizationUtil> util = Mockito.mockStatic(AuthorizationUtil.class)) {
      util.when(AuthorizationUtil::getCurrentUser).thenReturn("Ruhtinas Nukettaja");
      luoValintatapajonojaTasasijoilla();
      MuokattuJonosijaArvoDTO arvo = new MuokattuJonosijaArvoDTO();
      arvo.setTila(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA);
      arvo.setArvo(BigDecimal.valueOf(78.0));
      arvo.setSelite("Tälle vähän lisää pisteitä");
      MuokattuJonosija jonosija =
          valintalaskentaTulosService.muutaJarjestyskriteeri(
              VALINTATAPAJONO_OID, HAKEMUS_OID, 0, arvo, TestDataUtil.TEST_AUDIT_USER);
      Jarjestyskriteeritulos tulos =
          jonosija.getJarjestyskriteerit().stream()
              .filter(k -> k.getPrioriteetti() == 0)
              .findFirst()
              .orElseThrow();
      assertJarjestyskriteeriTulos(
          tulos,
          JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA,
          BigDecimal.valueOf(78.0),
          "Tälle vähän lisää pisteitä");
    }
    ;
  }

  @Test
  public void testMuokkaaJonosijanJarjestyskriteereja() {
    try (MockedStatic<AuthorizationUtil> util = Mockito.mockStatic(AuthorizationUtil.class)) {
      util.when(AuthorizationUtil::getCurrentUser).thenReturn("Ruhtinas Nukettaja");
      luoValintatapajonojaTasasijoilla();
      MuokattuJonosijaArvoPrioriteettiDTO arvo1 =
          luoMuokattuJonosijaArvoPrioriteettiDTO(
              JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, 76, "Tälle vähän lisää pisteitä", 0);
      MuokattuJonosijaArvoPrioriteettiDTO arvo2 =
          luoMuokattuJonosijaArvoPrioriteettiDTO(
              JarjestyskriteerituloksenTila.HYLATTY, 0, "Tälle 0 pistettä!", 1);
      MuokattuJonosija jonosija =
          valintalaskentaTulosService.muutaJarjestyskriteerit(
              VALINTATAPAJONO_OID,
              HAKEMUS_OID,
              List.of(arvo1, arvo2),
              TestDataUtil.TEST_AUDIT_USER);
      Jarjestyskriteeritulos tulos =
          jonosija.getJarjestyskriteerit().stream()
              .filter(k -> k.getPrioriteetti() == 0)
              .findFirst()
              .orElseThrow();
      assertJarjestyskriteeriTulos(
          tulos,
          JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA,
          BigDecimal.valueOf(76),
          "Tälle vähän lisää pisteitä");
      Jarjestyskriteeritulos tulos2 =
          jonosija.getJarjestyskriteerit().stream()
              .filter(k -> k.getPrioriteetti() == 1)
              .findFirst()
              .orElseThrow();
      assertJarjestyskriteeriTulos(
          tulos2,
          JarjestyskriteerituloksenTila.HYLATTY,
          BigDecimal.valueOf(0),
          "Tälle 0 pistettä!");
    }
  }

  @Test
  public void testMuokkaaOlemassaOlevanMuokatunJonosijanJarjestyskriteereja() {
    try (MockedStatic<AuthorizationUtil> util = Mockito.mockStatic(AuthorizationUtil.class)) {
      util.when(AuthorizationUtil::getCurrentUser).thenReturn("Ruhtinas Nukettaja");
      luoValintatapajonojaTasasijoilla();
      MuokattuJonosijaArvoPrioriteettiDTO arvo1 =
          luoMuokattuJonosijaArvoPrioriteettiDTO(
              JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, 56, "Tää on riittävä", 0);
      MuokattuJonosijaArvoPrioriteettiDTO arvo2 =
          luoMuokattuJonosijaArvoPrioriteettiDTO(
              JarjestyskriteerituloksenTila.MAARITTELEMATON, -10, "Mitäs tälle piti antaa?", 1);
      MuokattuJonosija jonosija =
          valintalaskentaTulosService.muutaJarjestyskriteerit(
              VALINTATAPAJONO_OID,
              HAKEMUS_OID,
              List.of(arvo1, arvo2),
              TestDataUtil.TEST_AUDIT_USER);
      assertEquals(2, jonosija.getJarjestyskriteerit().size());
      Jarjestyskriteeritulos tulos =
          jonosija.getJarjestyskriteerit().stream()
              .filter(k -> k.getPrioriteetti() == 0)
              .findFirst()
              .orElseThrow();
      assertJarjestyskriteeriTulos(
          tulos,
          JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA,
          BigDecimal.valueOf(56),
          "Tää on riittävä");
      Jarjestyskriteeritulos tulos2 =
          jonosija.getJarjestyskriteerit().stream()
              .filter(k -> k.getPrioriteetti() == 1)
              .findFirst()
              .orElseThrow();
      assertJarjestyskriteeriTulos(
          tulos2,
          JarjestyskriteerituloksenTila.MAARITTELEMATON,
          BigDecimal.valueOf(-10),
          "Mitäs tälle piti antaa?");
      MuokattuJonosijaArvoPrioriteettiDTO arvo3 =
          luoMuokattuJonosijaArvoPrioriteettiDTO(
              JarjestyskriteerituloksenTila.HYVAKSYTTY_HARKINNANVARAISESTI,
              10,
              "Tätä vois harkita",
              1);
      MuokattuJonosija jonosijaMuokattu =
          valintalaskentaTulosService.muutaJarjestyskriteerit(
              VALINTATAPAJONO_OID, HAKEMUS_OID, List.of(arvo3), TestDataUtil.TEST_AUDIT_USER);
      assertEquals(2, jonosijaMuokattu.getJarjestyskriteerit().size());
      Jarjestyskriteeritulos uusiTulos =
          jonosijaMuokattu.getJarjestyskriteerit().stream()
              .filter(k -> k.getPrioriteetti() == 0)
              .findFirst()
              .orElseThrow();
      assertJarjestyskriteeriTulos(
          uusiTulos,
          JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA,
          BigDecimal.valueOf(56),
          "Tää on riittävä");
      Jarjestyskriteeritulos uusiTulos2 =
          jonosijaMuokattu.getJarjestyskriteerit().stream()
              .filter(k -> k.getPrioriteetti() == 1)
              .findFirst()
              .orElseThrow();
      assertJarjestyskriteeriTulos(
          uusiTulos2,
          JarjestyskriteerituloksenTila.HYVAKSYTTY_HARKINNANVARAISESTI,
          BigDecimal.valueOf(10),
          "Tätä vois harkita");
    }
  }

  private MuokattuJonosijaArvoPrioriteettiDTO luoMuokattuJonosijaArvoPrioriteettiDTO(
      JarjestyskriteerituloksenTila tila, Integer arvo, String selite, Integer prioriteetti) {
    MuokattuJonosijaArvoPrioriteettiDTO dto = new MuokattuJonosijaArvoPrioriteettiDTO();
    dto.setTila(tila);
    dto.setArvo(BigDecimal.valueOf(arvo));
    dto.setSelite(selite);
    dto.setJarjestyskriteeriPrioriteetti(prioriteetti);
    return dto;
  }

  private static void assertJarjestyskriteeriTulos(
      Jarjestyskriteeritulos tulos,
      JarjestyskriteerituloksenTila expectedTila,
      BigDecimal expectedArvo,
      String expectedKuvaus) {
    assertEquals(expectedTila, tulos.getTila());
    assertEquals(expectedArvo, tulos.getArvo());
    assertEquals(expectedKuvaus, tulos.getKuvausFI());
  }

  private void luoValintatapajonojaTasasijoilla() {
    Valintatapajono vtpj =
        TestEntityDataUtil.luoValintatapaJonoEntity(
            144,
            Set.of(
                TestEntityDataUtil.luoJonosijaEntity(
                    HAKEMUS_OID,
                    6,
                    false,
                    Arrays.asList(
                        TestEntityDataUtil.luoJarjestyskriteeritulosEntity(
                            70.0, 0, JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA),
                        TestEntityDataUtil.luoJarjestyskriteeritulosEntity(
                            1, 1, JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA),
                        TestEntityDataUtil.luoJarjestyskriteeritulosEntity(
                            49.0, 2, JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA),
                        TestEntityDataUtil.luoJarjestyskriteeritulosEntity(
                            7.975, 3, JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA),
                        TestEntityDataUtil.luoJarjestyskriteeritulosEntity(
                            21.0, 4, JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA))),
                TestEntityDataUtil.luoJonosijaEntity(
                    "1.2.246.562.11.00001146141",
                    1,
                    false,
                    Arrays.asList(
                        TestEntityDataUtil.luoJarjestyskriteeritulosEntity(
                            70.0, 0, JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA),
                        TestEntityDataUtil.luoJarjestyskriteeritulosEntity(
                            6, 1, JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA),
                        TestEntityDataUtil.luoJarjestyskriteeritulosEntity(
                            48.0, 2, JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA),
                        TestEntityDataUtil.luoJarjestyskriteeritulosEntity(
                            8.0, 3, JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA),
                        TestEntityDataUtil.luoJarjestyskriteeritulosEntity(
                            22, 4, JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA))),
                TestEntityDataUtil.luoJonosijaEntity(
                    "1.2.246.562.11.00001046636",
                    2,
                    false,
                    Arrays.asList(
                        TestEntityDataUtil.luoJarjestyskriteeritulosEntity(
                            70.0, 0, JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA),
                        TestEntityDataUtil.luoJarjestyskriteeritulosEntity(
                            5, 1, JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA),
                        TestEntityDataUtil.luoJarjestyskriteeritulosEntity(
                            45.0, 2, JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA),
                        TestEntityDataUtil.luoJarjestyskriteeritulosEntity(
                            8.62, 3, JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA),
                        TestEntityDataUtil.luoJarjestyskriteeritulosEntity(
                            25, 4, JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA))),
                TestEntityDataUtil.luoJonosijaEntity(
                    "Pitäis olla tasasijalla 1",
                    1,
                    false,
                    Arrays.asList(
                        TestEntityDataUtil.luoJarjestyskriteeritulosEntity(
                            70.0, 0, JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA),
                        TestEntityDataUtil.luoJarjestyskriteeritulosEntity(
                            6, 1, JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA),
                        TestEntityDataUtil.luoJarjestyskriteeritulosEntity(
                            53.0, 2, JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA),
                        TestEntityDataUtil.luoJarjestyskriteeritulosEntity(
                            7.375, 3, JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA),
                        TestEntityDataUtil.luoJarjestyskriteeritulosEntity(
                            17, 4, JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA))),
                TestEntityDataUtil.luoJonosijaEntity(
                    "Pitäis olla tasasijalla 2",
                    1,
                    false,
                    Arrays.asList(
                        TestEntityDataUtil.luoJarjestyskriteeritulosEntity(
                            70.0, 0, JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA),
                        TestEntityDataUtil.luoJarjestyskriteeritulosEntity(
                            6, 1, JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA),
                        TestEntityDataUtil.luoJarjestyskriteeritulosEntity(
                            53.0, 2, JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA),
                        TestEntityDataUtil.luoJarjestyskriteeritulosEntity(
                            7.375, 3, JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA),
                        TestEntityDataUtil.luoJarjestyskriteeritulosEntity(
                            17, 4, JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA)))),
            "Varsinaisen valinnanvaiheen valintatapajono",
            0,
            Tasasijasaanto.YLITAYTTO,
            VALINTATAPAJONO_OID);
    vtpj.setSiirretaanSijoitteluun(true);
    vtpj.setPoissaOlevaTaytto(true);
    Valinnanvaihe vv =
        TestEntityDataUtil.luoValinnanvaiheEntity(
            "haku1", "hakukohde1", 0, "vaihe1", List.of(vtpj));
    valinnanvaiheRepository.save(vv);
  }
}
