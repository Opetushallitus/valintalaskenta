package fi.vm.sade.valintalaskenta.tulos.dao;

import static org.junit.Assert.assertEquals;
import static org.junit.jupiter.api.Assertions.*;

import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeOsallistuminenDTO;
import fi.vm.sade.valintalaskenta.domain.testdata.TestEntityDataUtil;
import fi.vm.sade.valintalaskenta.domain.valintakoe.*;
import fi.vm.sade.valintalaskenta.testing.AbstractIntegrationTest;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import java.util.*;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

public class TulosValintakoeOsallistuminenDAOTest extends AbstractIntegrationTest {

  @Autowired private TulosValintakoeOsallistuminenDAO tulosValintakoeOsallistuminenDAO;

  @Autowired private ValintalaskentaModelMapper modelMapper;

  @Test
  public void testFindByHakuAndOsallistuminen() {
    final String hakuOid = "hakuOid1";
    final Osallistuminen osallistuminen = Osallistuminen.VIRHE;

    final String hakemusOid = "hakemusOid1";
    valintakoeOsallistuminenRepository.save(
        TestEntityDataUtil.luoValintakoeOsallistuminen(
            hakuOid,
            "hakija1",
            hakemusOid,
            Set.of(
                TestEntityDataUtil.luoHakutoiveEntity(
                    "hakukohde1",
                    Set.of(
                        TestEntityDataUtil.luoValintakoeValinnanvaiheEntity(
                            0,
                            "vaihe1",
                            List.of(
                                TestEntityDataUtil.luoValintakoeEntity(
                                    "vk1", "vk1", osallistuminen, false, null))),
                        TestEntityDataUtil.luoValintakoeValinnanvaiheEntity(
                            0,
                            "vaihe2",
                            List.of(
                                TestEntityDataUtil.luoValintakoeEntity(
                                    "vk2", "vk2", osallistuminen, false, null))))))));

    List<ValintakoeOsallistuminen> osallistumiset =
        tulosValintakoeOsallistuminenDAO.findByHakuAndOsallistuminen(hakuOid, osallistuminen);
    assertEquals(1, osallistumiset.size());
    ValintakoeOsallistuminen vko = osallistumiset.get(0);

    assertNotNull(modelMapper.map(vko, ValintakoeOsallistuminenDTO.class));

    assertEquals(vko.getHakemusOid(), hakemusOid);
    assertEquals(1, vko.getHakutoiveet().size());
    Hakutoive hakutoive = vko.getHakutoiveetAsList().get(0);
    assertEquals(2, hakutoive.getValintakoeValinnanvaiheet().size());
    ValintakoeValinnanvaihe vaihe = hakutoive.getValintakoeValinnanvaiheetAsList().get(0);
    assertEquals(1, vaihe.getValintakokeet().size());
    Valintakoe koe = vaihe.getValintakokeetAsList().get(0);
    assertEquals(koe.getOsallistuminen(), osallistuminen);
  }

  @Test
  public void findByHakutoiveetBatched_emptyInput_returnsEmpty() {
    assertTrue(tulosValintakoeOsallistuminenDAO.findByHakutoiveetBatched(List.of()).isEmpty());
    assertTrue(tulosValintakoeOsallistuminenDAO.findByHakutoiveetBatched(null).isEmpty());
  }

  @Test
  public void findByHakutoiveetBatched_noMatchingHakukohde_returnsEmpty() {
    valintakoeOsallistuminenRepository.save(
        TestEntityDataUtil.luoValintakoeOsallistuminen(
            "haku1",
            "hakija1",
            "hakemus1",
            Set.of(TestEntityDataUtil.luoHakutoiveEntity("hakukohde-A", Set.of()))));

    List<ValintakoeOsallistuminen> result =
        tulosValintakoeOsallistuminenDAO.findByHakutoiveetBatched(List.of("hakukohde-X"));

    assertTrue(result.isEmpty());
  }

  @Test
  public void findByHakutoiveetBatched_singleMatch_loadsFullTree() {
    valintakoeOsallistuminenRepository.save(
        TestEntityDataUtil.luoValintakoeOsallistuminen(
            "haku1",
            "hakija1",
            "hakemus1",
            Set.of(
                TestEntityDataUtil.luoHakutoiveEntity(
                    "hakukohde-A",
                    Set.of(
                        TestEntityDataUtil.luoValintakoeValinnanvaiheEntity(
                            0,
                            "vaihe1",
                            List.of(
                                TestEntityDataUtil.luoValintakoeEntity(
                                    "vk1", "vk1", Osallistuminen.OSALLISTUU, true, null),
                                TestEntityDataUtil.luoValintakoeEntity(
                                    "vk2", "vk2", Osallistuminen.EI_OSALLISTU, false, null))))))));

    List<ValintakoeOsallistuminen> result =
        tulosValintakoeOsallistuminenDAO.findByHakutoiveetBatched(List.of("hakukohde-A"));

    assertEquals(1, result.size());
    ValintakoeOsallistuminen vko = result.get(0);
    assertEquals("hakemus1", vko.getHakemusOid());
    assertEquals(1, vko.getHakutoiveet().size());

    Hakutoive hakutoive = vko.getHakutoiveetAsList().get(0);
    assertEquals("hakukohde-A", hakutoive.getHakukohdeOid());
    assertEquals(1, hakutoive.getValintakoeValinnanvaiheet().size());

    ValintakoeValinnanvaihe vaihe = hakutoive.getValintakoeValinnanvaiheetAsList().get(0);
    assertEquals(2, vaihe.getValintakokeet().size());
    assertTrue(
        vaihe.getValintakokeetAsList().stream()
            .anyMatch(vk -> Osallistuminen.OSALLISTUU.equals(vk.getOsallistuminen())));
  }

  @Test
  public void findByHakutoiveetBatched_multipleVos_returnsAll() {
    valintakoeOsallistuminenRepository.save(
        TestEntityDataUtil.luoValintakoeOsallistuminen(
            "haku1",
            "hakija1",
            "hakemus1",
            Set.of(TestEntityDataUtil.luoHakutoiveEntity("hakukohde-A", Set.of()))));
    valintakoeOsallistuminenRepository.save(
        TestEntityDataUtil.luoValintakoeOsallistuminen(
            "haku1",
            "hakija2",
            "hakemus2",
            Set.of(TestEntityDataUtil.luoHakutoiveEntity("hakukohde-B", Set.of()))));

    List<ValintakoeOsallistuminen> result =
        tulosValintakoeOsallistuminenDAO.findByHakutoiveetBatched(
            List.of("hakukohde-A", "hakukohde-B"));

    assertEquals(2, result.size());
    assertTrue(result.stream().anyMatch(v -> "hakemus1".equals(v.getHakemusOid())));
    assertTrue(result.stream().anyMatch(v -> "hakemus2".equals(v.getHakemusOid())));
  }

  @Test
  public void findByHakutoiveetBatched_voMatchedByMultipleOids_returnedOnce() {
    // VO has two hakutoiveet; both hakukohde OIDs are in the query input
    valintakoeOsallistuminenRepository.save(
        TestEntityDataUtil.luoValintakoeOsallistuminen(
            "haku1",
            "hakija1",
            "hakemus1",
            Set.of(
                TestEntityDataUtil.luoHakutoiveEntity("hakukohde-A", Set.of()),
                TestEntityDataUtil.luoHakutoiveEntity("hakukohde-B", Set.of()))));

    List<ValintakoeOsallistuminen> result =
        tulosValintakoeOsallistuminenDAO.findByHakutoiveetBatched(
            List.of("hakukohde-A", "hakukohde-B"));

    assertEquals(1, result.size());
    assertEquals("hakemus1", result.get(0).getHakemusOid());
    assertEquals(2, result.get(0).getHakutoiveet().size());
  }

  @Test
  public void findByHakutoiveetBatched_onlyReturnsVosMatchingInput() {
    // Two VOs saved; only one has a hakukohde OID that is in the input
    valintakoeOsallistuminenRepository.save(
        TestEntityDataUtil.luoValintakoeOsallistuminen(
            "haku1",
            "hakija1",
            "hakemus1",
            Set.of(TestEntityDataUtil.luoHakutoiveEntity("hakukohde-A", Set.of()))));
    valintakoeOsallistuminenRepository.save(
        TestEntityDataUtil.luoValintakoeOsallistuminen(
            "haku1",
            "hakija2",
            "hakemus2",
            Set.of(TestEntityDataUtil.luoHakutoiveEntity("hakukohde-B", Set.of()))));

    List<ValintakoeOsallistuminen> result =
        tulosValintakoeOsallistuminenDAO.findByHakutoiveetBatched(List.of("hakukohde-A"));

    assertEquals(1, result.size());
    assertEquals("hakemus1", result.get(0).getHakemusOid());
  }
}
