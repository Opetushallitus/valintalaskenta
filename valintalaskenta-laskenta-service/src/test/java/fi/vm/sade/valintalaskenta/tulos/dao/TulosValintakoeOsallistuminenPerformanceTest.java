package fi.vm.sade.valintalaskenta.tulos.dao;

import static org.junit.jupiter.api.Assertions.*;

import fi.vm.sade.valintalaskenta.domain.testdata.TestEntityDataUtil;
import fi.vm.sade.valintalaskenta.domain.valintakoe.*;
import fi.vm.sade.valintalaskenta.testing.AbstractIntegrationTest;
import java.util.*;
import java.util.stream.Collectors;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;

/**
 * Compares findByHakutoiveet (N+1 Spring Data JDBC aggregate loading) against
 * findByHakutoiveetBatched (4 batched IN queries) on a dataset large enough to make the difference
 * visible.
 *
 * <p>Dataset: 200 hakukohde OIDs × 2 VOs each = 400 VOs, each with 3 hakutoiveet × 2
 * valinnanvaiheet × 3 valintakokeet. Old method issues ~4001 queries; new method issues 4.
 *
 * <p>Uses TRUNCATE instead of deleteAll() to avoid the history trigger's clock-skew issue.
 */
public class TulosValintakoeOsallistuminenPerformanceTest extends AbstractIntegrationTest {

  @Autowired private TulosValintakoeOsallistuminenDAO dao;
  @Autowired private JdbcTemplate jdbcTemplate;

  // Override the parent's deleteAll()-based setUp to use TRUNCATE, which bypasses the
  // row-level history trigger and is also faster for large datasets.
  @Override
  @BeforeEach
  public void setUp() {
    jdbcTemplate.execute("TRUNCATE valintakoe_osallistuminen CASCADE");
  }

  @Test
  void findByHakutoiveetBatched_isFasterThanOldMethod_onLargeDataset() {
    // 200 hakukohde OIDs × 2 VOs × 3 hakutoiveet × 2 valinnanvaiheet × 3 valintakokeet
    // → old method: 1 + 400 + 1200 + 2400 = 4001 queries
    // → new method: 4 queries
    List<String> hakukohdeOids = insertLargeDataset(200, 2, 3, 2, 3);

    // Warm up both methods to avoid first-query overhead skewing results
    dao.findByHakutoiveet(List.of(hakukohdeOids.get(0)));
    dao.findByHakutoiveetBatched(List.of(hakukohdeOids.get(0)));

    long oldStart = System.currentTimeMillis();
    List<ValintakoeOsallistuminen> oldResult = dao.findByHakutoiveet(hakukohdeOids);
    long oldMs = System.currentTimeMillis() - oldStart;

    long newStart = System.currentTimeMillis();
    List<ValintakoeOsallistuminen> newResult = dao.findByHakutoiveetBatched(hakukohdeOids);
    long newMs = System.currentTimeMillis() - newStart;

    System.out.printf(
        "[Performance] old (N+1): %d ms  |  new (batched): %d ms  |  speedup: %.1fx%n",
        oldMs, newMs, (double) oldMs / Math.max(newMs, 1));

    assertEquals(oldResult.size(), newResult.size(), "Both methods must return the same VO count");
    // Require ≥5× speedup AND a meaningful old baseline so the comparison isn't dominated by
    // single-digit-ms timer noise on fast CI hardware.
    assertTrue(
        oldMs > 100 && newMs * 5 < oldMs,
        "Batched method (%d ms) should be ≥5× faster than N+1 method (%d ms)"
            .formatted(newMs, oldMs));
  }

  @Test
  void findByHakutoiveetBatched_returnsEquivalentDataToOldMethod() {
    // Moderate dataset — enough to cover multiple VOs, hakutoiveet, and valintakokeet
    List<String> hakukohdeOids = insertLargeDataset(10, 2, 3, 2, 3);

    List<ValintakoeOsallistuminen> oldResult = dao.findByHakutoiveet(hakukohdeOids);
    List<ValintakoeOsallistuminen> newResult = dao.findByHakutoiveetBatched(hakukohdeOids);

    assertEquals(oldResult.size(), newResult.size());

    Map<String, ValintakoeOsallistuminen> oldByHakemus = indexByHakemusOid(oldResult);
    Map<String, ValintakoeOsallistuminen> newByHakemus = indexByHakemusOid(newResult);
    assertEquals(oldByHakemus.keySet(), newByHakemus.keySet(), "same hakemusOids");

    for (String hakemusOid : oldByHakemus.keySet()) {
      ValintakoeOsallistuminen old = oldByHakemus.get(hakemusOid);
      ValintakoeOsallistuminen nw = newByHakemus.get(hakemusOid);

      assertEquals(old.getHakuOid(), nw.getHakuOid(), "hakuOid for " + hakemusOid);
      assertEquals(old.getHakijaOid(), nw.getHakijaOid(), "hakijaOid for " + hakemusOid);

      Set<String> oldHakukohdeOids = hakukohdeOidsOf(old);
      Set<String> newHakukohdeOids = hakukohdeOidsOf(nw);
      assertEquals(oldHakukohdeOids, newHakukohdeOids, "hakutoiveet for " + hakemusOid);

      for (Hakutoive oldToive : old.getHakutoiveet()) {
        Hakutoive newToive =
            nw.getHakutoiveet().stream()
                .filter(h -> h.getHakukohdeOid().equals(oldToive.getHakukohdeOid()))
                .findFirst()
                .orElseThrow();

        Set<String> oldVvvOids = valinnanvaiheOidsOf(oldToive);
        Set<String> newVvvOids = valinnanvaiheOidsOf(newToive);
        assertEquals(oldVvvOids, newVvvOids, "valinnanvaiheet for " + oldToive.getHakukohdeOid());

        for (ValintakoeValinnanvaihe oldVvv : oldToive.getValintakoeValinnanvaiheet()) {
          ValintakoeValinnanvaihe newVvv =
              newToive.getValintakoeValinnanvaiheet().stream()
                  .filter(v -> v.getValinnanvaiheOid().equals(oldVvv.getValinnanvaiheOid()))
                  .findFirst()
                  .orElseThrow();

          Set<String> oldVkOids = valintakoeOidsOf(oldVvv);
          Set<String> newVkOids = valintakoeOidsOf(newVvv);
          assertEquals(oldVkOids, newVkOids, "valintakokeet for " + oldVvv.getValinnanvaiheOid());
        }
      }
    }
  }

  private static Map<String, ValintakoeOsallistuminen> indexByHakemusOid(
      List<ValintakoeOsallistuminen> list) {
    return list.stream().collect(Collectors.toMap(ValintakoeOsallistuminen::getHakemusOid, v -> v));
  }

  private static Set<String> hakukohdeOidsOf(ValintakoeOsallistuminen vo) {
    return vo.getHakutoiveet().stream().map(Hakutoive::getHakukohdeOid).collect(Collectors.toSet());
  }

  private static Set<String> valinnanvaiheOidsOf(Hakutoive h) {
    return h.getValintakoeValinnanvaiheet().stream()
        .map(ValintakoeValinnanvaihe::getValinnanvaiheOid)
        .collect(Collectors.toSet());
  }

  private static Set<String> valintakoeOidsOf(ValintakoeValinnanvaihe vvv) {
    return vvv.getValintakokeet().stream()
        .map(Valintakoe::getValintakoeOid)
        .collect(Collectors.toSet());
  }

  /**
   * Inserts a dataset with the given branching factors. Returns the list of hakukohde OIDs that can
   * be used as query input. Each hakukohde OID is held by {@code vosPerHakukohde} VOs; each VO has
   * {@code hakutoiveetPerVo} hakutoiveet (one using the hakukohde OID, the rest using unrelated
   * OIDs).
   */
  private List<String> insertLargeDataset(
      int hakukohdeCount,
      int vosPerHakukohde,
      int hakutoiveetPerVo,
      int valinnanvaiheetPerHakutoive,
      int valintakokeetPerValinnanvaihe) {

    // Set createdAt to yesterday so the history trigger (tstzrange(created_at, now())) never
    // sees lower > upper when rows are deleted in subsequent tests.
    Date yesterday = new Date(System.currentTimeMillis() - 86_400_000L);

    List<String> hakukohdeOids = new ArrayList<>();
    for (int i = 0; i < hakukohdeCount; i++) {
      hakukohdeOids.add("perf-hakukohde-" + i);
    }

    for (int i = 0; i < hakukohdeCount; i++) {
      String hakukohdeOid = hakukohdeOids.get(i);
      for (int j = 0; j < vosPerHakukohde; j++) {
        Set<Hakutoive> hakutoiveet = new HashSet<>();
        for (int k = 0; k < hakutoiveetPerVo; k++) {
          String toiveOid = k == 0 ? hakukohdeOid : "perf-other-" + i + "-" + k;
          Set<ValintakoeValinnanvaihe> vaiheet = new HashSet<>();
          for (int v = 0; v < valinnanvaiheetPerHakutoive; v++) {
            List<Valintakoe> kokeet = new ArrayList<>();
            for (int kk = 0; kk < valintakokeetPerValinnanvaihe; kk++) {
              kokeet.add(
                  TestEntityDataUtil.luoValintakoeEntity(
                      "vk-%d-%d-%d-%d-%d".formatted(i, j, k, v, kk),
                      "tunniste-" + kk,
                      Osallistuminen.OSALLISTUU,
                      false,
                      null));
            }
            vaiheet.add(
                TestEntityDataUtil.luoValintakoeValinnanvaiheEntity(
                    v, "vaihe-%d-%d-%d-%d".formatted(i, j, k, v), kokeet));
          }
          hakutoiveet.add(TestEntityDataUtil.luoHakutoiveEntity(toiveOid, vaiheet));
        }
        ValintakoeOsallistuminen vo =
            TestEntityDataUtil.luoValintakoeOsallistuminen(
                "perf-haku",
                "hakija-%d-%d".formatted(i, j),
                "hakemus-%d-%d".formatted(i, j),
                hakutoiveet);
        vo.setCreatedAt(yesterday);
        valintakoeOsallistuminenRepository.save(vo);
      }
    }

    return hakukohdeOids;
  }
}
