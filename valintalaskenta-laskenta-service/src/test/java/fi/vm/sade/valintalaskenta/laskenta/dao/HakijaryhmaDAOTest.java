package fi.vm.sade.valintalaskenta.laskenta.dao;

import static java.util.Arrays.asList;
import static java.util.stream.Collectors.toList;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;

import fi.vm.sade.auditlog.User;
import fi.vm.sade.valintalaskenta.domain.valinta.Hakijaryhma;
import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
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

@ContextConfiguration(locations = "classpath:application-context-test.xml")
@RunWith(SpringJUnit4ClassRunner.class)
@TestExecutionListeners(
    listeners = {
      DependencyInjectionTestExecutionListener.class,
      DirtiesContextTestExecutionListener.class
    })
public class HakijaryhmaDAOTest {

  @Autowired private ApplicationContext applicationContext;

  @Autowired private HakijaryhmaDAO hakijaryhmaDAO;

  private final User auditUser = null;

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
