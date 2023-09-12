package fi.vm.sade.valintalaskenta.laskenta.dao;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.valintakoe.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestExecutionListeners;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.support.DependencyInjectionTestExecutionListener;
import org.springframework.test.context.support.DirtiesContextTestExecutionListener;

/** User: wuoti Date: 7.5.2013 Time: 9.19 */
@ContextConfiguration(locations = "classpath:application-context-test.xml")
@RunWith(SpringJUnit4ClassRunner.class)
@TestExecutionListeners(
    listeners = {
      DependencyInjectionTestExecutionListener.class,
      DirtiesContextTestExecutionListener.class
    })
public class ValintakoeOsallistuminenDAOTest {

  @Autowired private ApplicationContext applicationContext;

  @Autowired private ValintakoeOsallistuminenDAO valintakoeOsallistuminenDAO;

  private static Valintakoe luoValintakoe(
      String valintakoeOid, String valintakoetunniste, Osallistuminen osallistuminen) {
    Valintakoe valintakoe = new Valintakoe();
    valintakoe.setValintakoeOid(valintakoeOid);
    valintakoe.setValintakoeTunniste(valintakoetunniste);
    valintakoe.setOsallistuminen(osallistuminen);

    return valintakoe;
  }

  private static ValintakoeValinnanvaihe luoValinnanVaihe(
      String valinnanVaiheOid, int valinnanVaiheJarjestysluku) {
    ValintakoeValinnanvaihe valinnanVaihe = new ValintakoeValinnanvaihe();
    Valinnanvaihe vaihe = new Valinnanvaihe();
    vaihe.setValinnanvaiheOid(valinnanVaiheOid);
    valinnanVaihe.setValinnanVaihe(vaihe);
    valinnanVaihe.setValinnanVaiheJarjestysluku(valinnanVaiheJarjestysluku);
    return valinnanVaihe;
  }

  private static Hakutoive luoHakutoive(String hakukohdeOid) {
    Hakutoive hakutoive = new Hakutoive();
    hakutoive.setHakukohdeOid(hakukohdeOid);
    return hakutoive;
  }

  private static ValintakoeOsallistuminen luoValintakoeOsallistuminen(
      String hakemusOid, String hakijaOid, String hakuOid) {
    ValintakoeOsallistuminen osallistuminen = new ValintakoeOsallistuminen();
    osallistuminen.setHakemusOid(hakemusOid);
    osallistuminen.setHakijaOid(hakijaOid);
    osallistuminen.setHakuOid(hakuOid);

    return osallistuminen;
  }

  private static ValintakoeOsallistuminen luoTestiOsallistuminen(
      String hakemusOid, String hakijaOid, String hakuOid) {
    ValintakoeOsallistuminen osallistuminen =
        luoValintakoeOsallistuminen(hakemusOid, hakijaOid, hakuOid);

    for (int i = 1; i <= 3; ++i) {
      Hakutoive hakutoive = luoHakutoive("hakutoive" + i);
      for (int j = 1; j <= 3; ++j) {
        ValintakoeValinnanvaihe vaihe = luoValinnanVaihe("valinnanVaihe" + j, j);
        for (int k = 1; k <= 3; ++k) {
          Valintakoe koe =
              luoValintakoe(
                  "" + i + j + k,
                  "" + i + j + k,
                  k % 2 == 0 ? Osallistuminen.EI_OSALLISTU : Osallistuminen.OSALLISTUU);
          vaihe.getValintakokeet().add(koe);
        }
        hakutoive.getValintakoeValinnanvaiheet().add(vaihe);
      }
      osallistuminen.getHakutoiveet().add(hakutoive);
    }

    return osallistuminen;
  }

  @Test
  public void testCreateAndReadByHakuOidAndHakemusOid() {
    assertEquals(0, valintakoeOsallistuminenDAO.readAll().size());

    String hakuOid = "hakuOid";

    String hakemusOid1 = "hakemusOid1";
    String hakijaOid1 = "hakijaOid1";

    ValintakoeOsallistuminen osallistuminen1 =
        luoTestiOsallistuminen(hakemusOid1, hakijaOid1, hakuOid);

    String hakemusOid2 = "hakemusOid2";
    String hakijaOid2 = "hakijaOid2";

    ValintakoeOsallistuminen osallistuminen2 =
        luoTestiOsallistuminen(hakemusOid2, hakijaOid2, hakuOid);

    valintakoeOsallistuminenDAO.createOrUpdate(osallistuminen1);
    valintakoeOsallistuminenDAO.createOrUpdate(osallistuminen2);
    assertEquals(2, valintakoeOsallistuminenDAO.readAll().size());

    ValintakoeOsallistuminen haettu1 =
        valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid1);
    assertEquals(osallistuminen1.getHakuOid(), haettu1.getHakuOid());
    assertEquals(osallistuminen1.getHakemusOid(), haettu1.getHakemusOid());

    ValintakoeOsallistuminen haettu2 =
        valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid2);
    assertEquals(osallistuminen2.getHakuOid(), haettu2.getHakuOid());
    assertEquals(osallistuminen2.getHakemusOid(), haettu2.getHakemusOid());
  }

  @Test
  public void testHaeEdeltavaValinnanvaihe() {
    assertNotNull(
        valintakoeOsallistuminenDAO.haeEdeltavaValinnanvaihe("hakuOid1", "hakuKohdeOid1", 1));
    assertNull(
        valintakoeOsallistuminenDAO.haeEdeltavaValinnanvaihe("hakuOid1", "hakuKohdeOid2", 1));
    assertNotNull(
        valintakoeOsallistuminenDAO.haeEdeltavaValinnanvaihe("hakuOid1", "hakuKohdeOid1", 6));
    assertNotNull(
        valintakoeOsallistuminenDAO.haeEdeltavaValinnanvaihe("hakuOid1", "hakuKohdeOid4", 3));
  }
}
