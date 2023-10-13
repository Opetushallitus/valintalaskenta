package fi.vm.sade.valintalaskenta.tulos.service;

import fi.vm.sade.valintalaskenta.domain.dto.*;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.*;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.ValintatietoValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.valinta.*;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import java.math.BigDecimal;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import fi.vm.sade.valintalaskenta.laskenta.testdata.TestDataUtil;
import fi.vm.sade.valintalaskenta.testing.AbstractMocklessIntegrationTest;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;

import static org.junit.Assert.*;

public class ValintalaskentaTulosServiceTest extends AbstractMocklessIntegrationTest {

  @Autowired private ValintalaskentaTulosService valintalaskentaTulosService;

  @Autowired private ApplicationContext applicationContext;

  @Test
  public void haeValintakoeOsallistumisetByOidTest() {
    valintakoeOsallistuminenRepository.save(TestDataUtil.luoValintakoeOsallistuminen("hakuOid1", "hakijaoid1", "oid1"));
    ValintakoeOsallistuminen kaikki =
        valintalaskentaTulosService.haeValintakoeOsallistumiset("oid1");

    assertEquals("hakijaoid1", kaikki.getHakijaOid());
  }

  @Test
  public void haeValintakoeOsallistumisetByHakutoiveTest() {
    valintakoeOsallistuminenRepository.save(TestDataUtil.luoValintakoeOsallistuminen("hakuoid", "hakijaoid1", "hakemusoid1",
      Set.of(TestDataUtil.luoHakutoiveEntity("oid1", new HashSet<>()))));
    valintakoeOsallistuminenRepository.save(TestDataUtil.luoValintakoeOsallistuminen("hakuoid", "hakijaoid2", "hakemusoid2",
      Set.of(TestDataUtil.luoHakutoiveEntity("oid1", new HashSet<>()),
        TestDataUtil.luoHakutoiveEntity("oid2", new HashSet<>()))));
    List<ValintakoeOsallistuminen> kaikki =
        valintalaskentaTulosService.haeValintakoeOsallistumisetByHakutoive("oid1");
    assertEquals(2, kaikki.size());
    kaikki = valintalaskentaTulosService.haeValintakoeOsallistumisetByHakutoive("oid2");
    assertEquals(1, kaikki.size());
  }

  @Test
  public void haeLasketutuValintavaiheetHaulleAndConvertTest() {
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
            .collect(Collectors.toList());

    assertEquals(1, convertedHakukohdeDTOs.size());
    HakukohdeDTO convertedHakukohdeDTO = convertedHakukohdeDTOs.get(0);
    assertEquals(convertedPrioriteetti, convertedHakukohdeDTO.getPrioriteetti());
  }

  @Test
  public void testHaeValintakoevirheetHaulle() {
    final String hakuOid = "hakuOid1";
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
      assertEquals(new BigDecimal("100.0"), kriteeri1.getArvo());
      assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, kriteeri1.getTila());

      JarjestyskriteeritulosDTO kriteeri2 = i.next();
      assertEquals(1, kriteeri2.getPrioriteetti());
      assertEquals(new BigDecimal("5.0"), kriteeri2.getArvo());
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
      assertEquals(new BigDecimal("20.0"), kriteeri1.getArvo());
      assertEquals(
          JarjestyskriteerituloksenTila.HYVAKSYTTY_HARKINNANVARAISESTI, kriteeri1.getTila());

      JarjestyskriteeritulosDTO kriteeri2 = i.next();
      assertEquals(1, kriteeri2.getPrioriteetti());
      assertEquals(new BigDecimal("10.0"), kriteeri2.getArvo());
      assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, kriteeri2.getTila());
    }
  }

  @Test
  public void testHaeTuloksetHakukohteelle() {
    {
      List<ValintatietoValinnanvaiheDTO> hakukohde =
          valintalaskentaTulosService.haeValinnanvaiheetHakukohteelle("hakukohde1");
      hakukohde.get(0).getValintatapajonot().get(0).getJonosijat().stream()
          .filter(h -> h.getSukunimi().equals("Lahtinen"))
          .forEach(
              h -> {
                assertEquals(1, h.getJonosija());
              });
      hakukohde.get(0).getValintatapajonot().get(0).getJonosijat().stream()
          .filter(h -> !h.getSukunimi().equals("Lahtinen"))
          .forEach(
              h -> {
                assertFalse(h.getJonosija() == 1);
              });
    }
  }

}
