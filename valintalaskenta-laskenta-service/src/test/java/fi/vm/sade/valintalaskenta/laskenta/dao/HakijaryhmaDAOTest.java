package fi.vm.sade.valintalaskenta.laskenta.dao;

import static com.lordofthejars.nosqlunit.mongodb.MongoDbRule.MongoDbRuleBuilder.newMongoDbRule;
import static java.util.Arrays.asList;
import static java.util.stream.Collectors.toList;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;

import com.lordofthejars.nosqlunit.annotation.UsingDataSet;
import com.lordofthejars.nosqlunit.core.LoadStrategyEnum;
import com.lordofthejars.nosqlunit.mongodb.MongoDbRule;
import fi.vm.sade.auditlog.User;
import fi.vm.sade.valintalaskenta.domain.valinta.Hakijaryhma;
import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import org.bson.types.ObjectId;
import org.hamcrest.Matchers;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestExecutionListeners;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.support.DependencyInjectionTestExecutionListener;
import org.springframework.test.context.support.DirtiesContextTestExecutionListener;

@ContextConfiguration(locations = "classpath:application-context-test.xml")
@RunWith(SpringJUnit4ClassRunner.class)
@TestExecutionListeners(
    listeners = {
      DependencyInjectionTestExecutionListener.class,
      DirtiesContextTestExecutionListener.class
    })
public class HakijaryhmaDAOTest {

  @Rule public MongoDbRule mongoDbRule = newMongoDbRule().defaultSpringMongoDb("test");

  @Autowired private ApplicationContext applicationContext;

  @Autowired private HakijaryhmaDAO hakijaryhmaDAO;

  private final User auditUser = null;

  @Test
  @UsingDataSet(
      locations = "hakijaryhmaMigrationTestData.json",
      loadStrategy = LoadStrategyEnum.CLEAN_INSERT)
  public void testMigrationOfHakijaryma() {
    Hakijaryhma hakijaryhma = hakijaryhmaDAO.haeHakijaryhma("vanhaHakijaryhmaOid").get();
    List<Jonosija> jonosijat = hakijaryhma.getJonosijat();
    List<ObjectId> jonosijaIdt = hakijaryhma.getJonosijaIdt();
    assertThat(jonosijat, Matchers.hasSize(3));
    assertThat(jonosijaIdt, Matchers.hasSize(3));
    for (int i = 0; i < Math.max(jonosijat.size(), jonosijaIdt.size()); i++) {
      assertEquals(jonosijaIdt.get(i), jonosijat.get(i).getId());
    }
  }

  @Test
  @UsingDataSet(
      locations = "hakijaryhmaMigrationTestData.json",
      loadStrategy = LoadStrategyEnum.CLEAN_INSERT)
  public void testLoadingOfMigratedHakijaryhma() {
    Hakijaryhma hakijaryhma = hakijaryhmaDAO.haeHakijaryhma("migroituHakijaryhmaOid").get();
    List<Jonosija> jonosijat = hakijaryhma.getJonosijat();
    List<ObjectId> jonosijaIdt = hakijaryhma.getJonosijaIdt();
    assertThat(jonosijat, Matchers.hasSize(3));
    assertThat(jonosijaIdt, Matchers.hasSize(3));
    for (int i = 0; i < Math.max(jonosijat.size(), jonosijaIdt.size()); i++) {
      assertEquals(jonosijaIdt.get(i), jonosijat.get(i).getId());
    }
  }

  @Test
  @UsingDataSet(
      locations = "hakijaryhmaMigrationTestData.json",
      loadStrategy = LoadStrategyEnum.CLEAN_INSERT)
  public void testLoadingHakijaryhmaWithoutJonosijas() {
    Hakijaryhma hakijaryhma = hakijaryhmaDAO.haeHakijaryhma("tyhjaHakijaryhmaOid").get();
    assertThat(hakijaryhma.getJonosijaIdt(), Matchers.empty());
    assertThat(hakijaryhma.getJonosijat(), Matchers.empty());
  }

  @Test
  @UsingDataSet(
      locations = "hakijaryhmaMigrationTestData.json",
      loadStrategy = LoadStrategyEnum.CLEAN_INSERT)
  public void testDeletingHakijaryhmaWithoutJonosijas() {
    Hakijaryhma hakijaryhma = hakijaryhmaDAO.haeHakijaryhma("tyhjaHakijaryhmaOid").get();
    hakijaryhmaDAO.poistaHakijaryhma(hakijaryhma);
    assertEquals(Optional.empty(), hakijaryhmaDAO.haeHakijaryhma("tyhjaHakijaryhmaOid"));
  }

  @Test
  public void testSavingAndLoadingNewHakijaryhma() {
    Hakijaryhma hakijaryhma = new Hakijaryhma();
    hakijaryhma.setJonosijat(asList(new Jonosija(), new Jonosija()));
    hakijaryhma.setHakijaryhmaOid("uusiHakijaryhmaOid");
    hakijaryhmaDAO.create(hakijaryhma, auditUser);
    Hakijaryhma savedHakijaryhma = hakijaryhmaDAO.haeHakijaryhma("uusiHakijaryhmaOid").get();
    assertThat(savedHakijaryhma.getJonosijat(), Matchers.hasSize(2));
    assertThat(savedHakijaryhma.getJonosijaIdt(), Matchers.hasSize(2));
  }

  @Test
  @UsingDataSet(
      locations = "multipleHakijaryhmasWithVaryingPriorities.json",
      loadStrategy = LoadStrategyEnum.CLEAN_INSERT)
  public void allCallsSortHakijaryhmaEntitiesInPriorityAscendingOrder() throws Exception {
    List<Hakijaryhma> fetched = hakijaryhmaDAO.haeHakijaryhmat("1.2.246.562.20.18895322503");
    assertEquals(3, fetched.size());

    List<Hakijaryhma> sorted = new ArrayList<>(fetched);
    sorted.sort(Comparator.comparing(Hakijaryhma::getPrioriteetti));

    assertEquals("Hakijaryhma entries are not sorted based on priority!", sorted, fetched);

    assertEquals(
        "lowest numeric priority should come first meaning it is the most important",
        asList("highestPriorityOid", "middlePriorityOid", "lowestPriorityOid"),
        sorted.stream().map(Hakijaryhma::getHakijaryhmaOid).collect(toList()));
  }
}
