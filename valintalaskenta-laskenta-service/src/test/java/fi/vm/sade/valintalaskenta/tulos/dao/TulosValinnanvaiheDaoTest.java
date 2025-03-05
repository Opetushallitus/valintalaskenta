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
  public void testHakeeViimeisimmätHaunHakukohteetLaskentaTehtyTiedoilla()
      throws InterruptedException {
    Valinnanvaihe vv1 = luoValinnanVaiheAIkaLeimalla("vaihe1", 1);
    Valinnanvaihe vv2 = luoValinnanVaiheAIkaLeimalla("vaihe2", 2);
    Valinnanvaihe vv3 = luoValinnanVaiheAIkaLeimalla("vaihe3", 3);
    valinnanvaiheRepository.saveAll(List.of(vv1, vv2, vv3));
    // varmistetaan ajan kulu jotta testi on deterministinen
    Thread.sleep(1L);
    Date timeNow = new Date();
    valinnanvaiheRepository.save(luoValinnanVaiheAIkaLeimalla("vaihe4", 4));
    List<HakukohdeLaskentaTehty> tehdytLaskennat =
        tulosValinnanvaiheDAO.haeLasketutHakukohteetHaulle("haku1");
    assertEquals(1, tehdytLaskennat.size());
    Date modified = tehdytLaskennat.get(0).lastModified;
    assertTrue(timeNow.before(modified) || timeNow.equals(modified));
  }

  private Valinnanvaihe luoValinnanVaiheAIkaLeimalla(String vaiheOid, int jarjestysnro) {
    return TestEntityDataUtil.luoValinnanvaiheEntity(
        "haku1",
        "hakukohde1",
        jarjestysnro,
        vaiheOid,
        List.of(
            TestEntityDataUtil.luoValintatapaJonoEntity(
                0, new HashSet<>(), "jono1nimi" + jarjestysnro, 0, null, "jono1" + jarjestysnro)));
  }
}
