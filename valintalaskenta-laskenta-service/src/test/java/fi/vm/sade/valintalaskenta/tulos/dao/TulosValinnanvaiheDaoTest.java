package fi.vm.sade.valintalaskenta.tulos.dao;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import fi.vm.sade.valintalaskenta.domain.testdata.TestEntityDataUtil;
import fi.vm.sade.valintalaskenta.domain.valinta.HakukohdeLaskentaTehty;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.testing.AbstractIntegrationTest;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

public class TulosValinnanvaiheDaoTest extends AbstractIntegrationTest {

  @Autowired private TulosValinnanvaiheDAO tulosValinnanvaiheDAO;

  @Test
  public void testHaeTuloksetValinnantapajonolle() {
    valinnanvaiheRepository.save(
        TestEntityDataUtil.luoValinnanvaiheEntity(
            "haku1",
            "hakukohde1",
            1,
            "vaihe1",
            List.of(
                TestEntityDataUtil.luoValintatapaJonoEntity(
                    0, new HashSet<>(), "jono1nimi", 0, null, "jono1"))));

    Valinnanvaihe vaihe = tulosValinnanvaiheDAO.findByValintatapajonoOid("jono1");

    assertNotNull(vaihe);
    assertEquals("vaihe1", vaihe.getValinnanVaiheOid());
  }

  @Test
  public void testHakeeViimeisimmatAnnetunHaunHakukohteetLaskentaTehtyTiedoilla()
      throws InterruptedException {
    Valinnanvaihe vv1 = luoValinnanVaiheAikaLeimalla("hk1", "vaihe1", 1);
    Valinnanvaihe vv2 = luoValinnanVaiheAikaLeimalla("hk1", "vaihe2", 2);
    Valinnanvaihe vv3 = luoValinnanVaiheAikaLeimalla("hk1", "vaihe3", 3);
    valinnanvaiheRepository.saveAll(List.of(vv1, vv2, vv3));
    // varmistetaan ajan kulu jotta testi on deterministinen
    Thread.sleep(1L);
    Date timeNow = new Date();
    valinnanvaiheRepository.save(luoValinnanVaiheAikaLeimalla("hk1", "vaihe4", 4));
    Valinnanvaihe vv1haku2 = luoValinnanVaiheAikaLeimalla("hkhaku2", "vaihe1h2", 1);
    vv1haku2.setHakuOid("MUU_HAKU");
    valinnanvaiheRepository.save(vv1haku2);
    List<HakukohdeLaskentaTehty> tehdytLaskennat =
        tulosValinnanvaiheDAO.haeLasketutHakukohteetHaulle("haku1");
    assertEquals(1, tehdytLaskennat.size());
    assertEquals("hk1", tehdytLaskennat.get(0).hakukohdeOid);
    assertTimeEqualsOrBefore(tehdytLaskennat.get(0), timeNow);
  }

  @Test
  public void testLaskentaTietojaEiLoydyHaulle() {
    assertTrue(tulosValinnanvaiheDAO.haeLasketutHakukohteetHaulle("haku1").isEmpty());
  }

  @Test
  public void testHakeeViimeisimmatHaunHakukohteetLaskentaTehtyTiedoillaUseallaHakukohteella()
      throws InterruptedException {
    Valinnanvaihe vv1 = luoValinnanVaiheAikaLeimalla("hk1", "vaihe1", 1);
    Valinnanvaihe vv2 = luoValinnanVaiheAikaLeimalla("hk1", "vaihe2", 2);
    Valinnanvaihe vv3 = luoValinnanVaiheAikaLeimalla("hk2", "vaihe1hk2", 1);
    valinnanvaiheRepository.saveAll(List.of(vv1, vv2, vv3));
    // varmistetaan ajan kulu jotta testi on deterministinen
    Thread.sleep(1L);
    Date timeNow = new Date();
    valinnanvaiheRepository.save(luoValinnanVaiheAikaLeimalla("hk1", "vaihe3", 3));
    valinnanvaiheRepository.save(luoValinnanVaiheAikaLeimalla("hk2", "vaihe2hk2", 2));
    List<HakukohdeLaskentaTehty> tehdytLaskennat =
        tulosValinnanvaiheDAO.haeLasketutHakukohteetHaulle("haku1");
    assertEquals(2, tehdytLaskennat.size());
    assertTimeEqualsOrBefore(tehdytLaskennat.get(0), timeNow);
    assertTimeEqualsOrBefore(tehdytLaskennat.get(1), timeNow);
    assertTrue(
        tehdytLaskennat.stream()
            .map(l -> l.hakukohdeOid)
            .toList()
            .containsAll(List.of("hk1", "hk2")));
  }

  private void assertTimeEqualsOrBefore(HakukohdeLaskentaTehty ht, Date timeToCompare) {
    Date modified = ht.lastModified;
    assertTrue(timeToCompare.before(modified) || timeToCompare.equals(modified));
  }

  private Valinnanvaihe luoValinnanVaiheAikaLeimalla(
      String hakukohde, String vaiheOid, int jarjestysnro) {
    String jonoOid = "jono" + hakukohde + jarjestysnro;
    return TestEntityDataUtil.luoValinnanvaiheEntity(
        "haku1",
        hakukohde,
        jarjestysnro,
        vaiheOid,
        List.of(
            TestEntityDataUtil.luoValintatapaJonoEntity(
                0, new HashSet<>(), jonoOid, 0, null, jonoOid)));
  }
}
