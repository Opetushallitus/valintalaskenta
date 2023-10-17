package fi.vm.sade.valintalaskenta.tulos.service.dao;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.laskenta.testdata.TestDataUtil;
import fi.vm.sade.valintalaskenta.testing.AbstractIntegrationTest;
import fi.vm.sade.valintalaskenta.tulos.dao.TulosValinnanvaiheDAO;
import java.util.HashSet;
import java.util.List;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

public class TulosValinnanvaiheDaoTest extends AbstractIntegrationTest {

  @Autowired private TulosValinnanvaiheDAO tulosValinnanvaiheDAO;

  @Test
  public void testHaeTuloksetValinnantapajonolle() {
    valinnanvaiheRepository.save(
        TestDataUtil.luoValinnanvaiheEntity(
            "haku1",
            "hakukohde1",
            1,
            "vaihe1",
            List.of(
                TestDataUtil.luoValintatapaJonoEntity(
                    0, new HashSet<>(), "jono1nimi", 0, null, "jono1"))));

    Valinnanvaihe vaihe = tulosValinnanvaiheDAO.findByValintatapajonoOid("jono1");

    assertNotNull(vaihe);
    assertEquals("vaihe1", vaihe.getValinnanVaiheOid());
  }
}
