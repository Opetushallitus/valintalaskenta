package fi.vm.sade.valintalaskenta.tulos.service.dao;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import fi.vm.sade.valintalaskenta.domain.testdata.TestEntityDataUtil;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.testing.AbstractIntegrationTest;
import fi.vm.sade.valintalaskenta.tulos.dao.TulosValinnanvaiheDAO;
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
}
