package fi.vm.sade.valintalaskenta.ovara.test;

import fi.vm.sade.valintalaskenta.ovara.ajastus.OvaraApp;
import fi.vm.sade.valintalaskenta.ovara.ajastus.SiirtotiedostoProsessi;
import fi.vm.sade.valintalaskenta.ovara.ajastus.repository.SiirtotiedostoProsessiRepository;
import fi.vm.sade.valintalaskenta.ovara.test.config.OvaraTestConfigurationWithMocks;
import org.junit.Assert;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Import;
import org.springframework.data.jdbc.repository.config.EnableJdbcRepositories;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit4.SpringRunner;

@RunWith(SpringRunner.class)
@SpringBootTest(classes = OvaraApp.class)
@EnableJdbcRepositories(basePackageClasses = {SiirtotiedostoProsessiRepository.class})
@ActiveProfiles({"test", "ovara"})
@Import(OvaraTestConfigurationWithMocks.class)
@Disabled
public class SiirtotiedostoProsessiTest {

  @Autowired private SiirtotiedostoProsessiRepository spr;

  public SiirtotiedostoProsessiTest() {}

  @Test
  public void testSiirtotiedostoProsessi() {
    SiirtotiedostoProsessi edellinenOnnistunut = spr.findLatestSuccessful();
    System.out.println("Edellinen prosessi: " + edellinenOnnistunut);

    SiirtotiedostoProsessi uusiProsessi = edellinenOnnistunut.createNewProcessBasedOnThis();
    System.out.println("Uusi prosessi: " + edellinenOnnistunut);

    Assert.assertEquals(
        "Uuden prosessin ikkuna alkaa siitä mihin edellisen onnistuneen ikkuna loppui",
        uusiProsessi.getWindowStart(),
        edellinenOnnistunut.getWindowEnd());

    SiirtotiedostoProsessi uusiPersistoituna = spr.save(uusiProsessi);
    System.out.println("Uusi prosessi persistoituna: " + uusiPersistoituna);

    Assert.assertEquals(
        "Persistoitu prosessi vastaa persistoimatonta", uusiProsessi, uusiPersistoituna);
  }
}
