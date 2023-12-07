package fi.vm.sade.valintalaskenta.tulos.service;

import static org.junit.jupiter.api.Assertions.*;

import fi.vm.sade.valintalaskenta.domain.dto.*;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.*;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.ValintatietoValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.testdata.TestEntityDataUtil;
import fi.vm.sade.valintalaskenta.domain.valinta.*;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.testing.AbstractMocklessIntegrationTest;
import java.math.BigDecimal;
import java.util.*;
import java.util.function.Function;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;

public class ValintalaskentaTulosServiceTest extends AbstractMocklessIntegrationTest {

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
    mjs.setJarjestyskriteerit(
        Set.of(
            TestEntityDataUtil.luoJarjestyskriteeritulosEntity(
                100.0, 0, JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA),
            TestEntityDataUtil.luoJarjestyskriteeritulosEntity(
                5.0, 1, JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA)));
    muokattuJonosijaRepository.save(mjs);

    HarkinnanvarainenHyvaksyminen hyvaksyminen = new HarkinnanvarainenHyvaksyminen();
    hyvaksyminen.setHarkinnanvaraisuusTila(HarkinnanvaraisuusTila.HYVAKSYTTY);
    hyvaksyminen.setHakukohdeOid("hakukohdeOid1");
    hyvaksyminen.setHakuOid("hakuOid1");
    hyvaksyminen.setHakemusOid("hakemusOid2");
    harkinnanvarainenHyvaksyminenRepository.save(hyvaksyminen);

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

  private void luoValintatapajonojaTasasijoilla() {
    Valintatapajono vtpj =
        TestEntityDataUtil.luoValintatapaJonoEntity(
            144,
            Set.of(
                TestEntityDataUtil.luoJonosijaEntity(
                    "1.2.246.562.11.00001128774",
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
            "jono1");
    vtpj.setSiirretaanSijoitteluun(true);
    vtpj.setPoissaOlevaTaytto(true);
    Valinnanvaihe vv =
        TestEntityDataUtil.luoValinnanvaiheEntity(
            "haku1", "hakukohde1", 0, "vaihe1", List.of(vtpj));
    valinnanvaiheRepository.save(vv);
  }
}
