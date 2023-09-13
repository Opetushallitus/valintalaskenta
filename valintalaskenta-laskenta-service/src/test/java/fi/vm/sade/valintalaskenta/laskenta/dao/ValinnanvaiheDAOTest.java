package fi.vm.sade.valintalaskenta.laskenta.dao;

import static org.junit.Assert.*;

import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;
import java.util.Arrays;
import org.hamcrest.Matchers;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestExecutionListeners;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.support.DependencyInjectionTestExecutionListener;
import org.springframework.test.context.support.DirtiesContextTestExecutionListener;

/** User: wuoti Date: 4.9.2013 Time: 12.27 */
@ContextConfiguration(locations = "classpath:application-context-test.xml")
@RunWith(SpringJUnit4ClassRunner.class)
@TestExecutionListeners(
    listeners = {
      DependencyInjectionTestExecutionListener.class,
      DirtiesContextTestExecutionListener.class
    })
public class ValinnanvaiheDAOTest {

  @Autowired private ApplicationContext applicationContext;

  @Autowired private ValinnanvaiheDAO valinnanvaiheDAO;

  @Test
  public void testHaeEdellinenValinnanvaihe() throws InterruptedException {
    final String hakuOid = "hakuOid1";
    final String hakukohdeOid = "hakukohdeOid1";
    final int jarjestysnumero = 3;

    final String edellinenValinnanvaiheOid = "valinnanvaiheOid2";
    final int edellinenValinnanvaiheJarjestysnumero = 2;

    Valinnanvaihe valinnanvaihe =
        valinnanvaiheDAO.haeEdeltavaValinnanvaihe(hakuOid, hakukohdeOid, jarjestysnumero);
    assertEquals(hakuOid, valinnanvaihe.getHakuOid());
    assertEquals(hakukohdeOid, valinnanvaihe.getHakukohdeOid());
    assertEquals(edellinenValinnanvaiheOid, valinnanvaihe.getValinnanVaiheOid());
    assertEquals(edellinenValinnanvaiheJarjestysnumero, valinnanvaihe.getJarjestysnumero());

    assertNull(valinnanvaiheDAO.haeEdeltavaValinnanvaihe(hakuOid, hakukohdeOid, 1));
  }

  @Test
  public void testHaeValinnanvaihe() throws InterruptedException {
    final String valinnanvaiheOid = "valinnanvaiheOid2";

    Valinnanvaihe valinnanvaihe = valinnanvaiheDAO.haeValinnanvaihe(valinnanvaiheOid);
    assertEquals(valinnanvaiheOid, valinnanvaihe.getValinnanVaiheOid());
  }

  //@Test
  /*public void testLoadingValintatapajonoWithoutJonosijat() {
    Valinnanvaihe valinnanvaihe = valinnanvaiheDAO.haeValinnanvaihe("tyhjaValinnanvaiheOid");
    valinnanvaihe
        .getValintatapajonot()
        .forEach(
            valintatapajono -> {
              assertThat(valintatapajono.getJonosijat(), Matchers.empty());
              assertThat(valintatapajono.getJonosijaIdt(), Matchers.empty());
            });
  }*/

  @Test
  public void testSavingAndLoadingNewValinnanvaihe() {
    Valinnanvaihe valinnanvaihe = new Valinnanvaihe();
    Valintatapajono valintatapajono = new Valintatapajono();
    valinnanvaihe.setValintatapajonot(Arrays.asList(valintatapajono));
    valintatapajono.setJonosijat(Arrays.asList(new Jonosija(), new Jonosija()));
    valinnanvaihe.setValinnanVaiheOid("uusiValinnanvaiheOid");
    valinnanvaiheDAO.saveOrUpdate(valinnanvaihe);
    Valinnanvaihe savedValinnanvaihe = valinnanvaiheDAO.haeValinnanvaihe("uusiValinnanvaiheOid");
    assertNotNull(savedValinnanvaihe);
    assertThat(savedValinnanvaihe.getValintatapajonot().get(0).getJonosijat(), Matchers.hasSize(2));
    //assertThat(savedValinnanvaihe.getValintatapajonot().get(0).getJonosijaIdt(), Matchers.hasSize(2));
  }
}
