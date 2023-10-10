package fi.vm.sade.valintalaskenta.tulos.dao;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/** Created by kjsaila on 11/03/15. */
@ContextConfiguration(locations = "classpath:application-context-test.xml")
@RunWith(SpringJUnit4ClassRunner.class)
public class ValinnanvaiheDaoTest {

  @Autowired
  TulosValinnanvaiheDAO tulosValinnanvaiheDAO;

  @Autowired private ApplicationContext applicationContext;

  @Test
  public void testHaeTuloksetValinnantapajonolle() {
    {
      Valinnanvaihe vaihe = tulosValinnanvaiheDAO.findByValintatapajonoOid("jono1");

      assertNotNull(vaihe);
      assertEquals("vaihe1", vaihe.getValinnanVaiheOid());
    }
  }
}
