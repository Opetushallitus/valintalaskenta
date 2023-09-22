package fi.vm.sade.valintalaskenta.laskenta.dao;

import static java.util.Arrays.asList;
import static java.util.stream.Collectors.toList;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;

import fi.vm.sade.auditlog.User;
import fi.vm.sade.valintalaskenta.domain.valinta.HakijaryhmaEntity;
import fi.vm.sade.valintalaskenta.domain.valinta.Hakijaryhma;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import fi.vm.sade.valintalaskenta.laskenta.testing.AbstractIntegrationTest;
import fi.vm.sade.valintalaskenta.laskenta.dao.impl.HakijaryhmaServiceImpl;
import fi.vm.sade.valintalaskenta.laskenta.dao.repository.HakijaryhmaRepository;
import org.junit.Test;
import org.junit.jupiter.api.BeforeEach;
import org.springframework.beans.factory.annotation.Autowired;

public class HakijaryhmaServiceTest extends AbstractIntegrationTest {

  @Autowired
  private HakijaryhmaServiceImpl hakijaryhmaDAO;

  @Autowired
  private HakijaryhmaRepository repo;
  private final User auditUser = null;

  @BeforeEach
  public void clear() {
    repo.deleteAll();
  }

  @Test
  public void testSavingAndLoadingNewHakijaryhma() {
    Hakijaryhma hakijaryhma = new Hakijaryhma();
    //hakijaryhma.setJonosijat(asList(new Jonosija(), new Jonosija()));
    hakijaryhma.hakijaryhmaOid = "uusiHakijaryhmaOid";
    hakijaryhmaDAO.create(hakijaryhma, auditUser);
    List<Hakijaryhma> ryhmat = (List<Hakijaryhma>) repo.findAll();
    Hakijaryhma savedHakijaryhma = hakijaryhmaDAO.haeHakijaryhmaLite("uusiHakijaryhmaOid").get();
    assertEquals(savedHakijaryhma.hakijaryhmaOid, "uusiHakijaryhmaOid");
    //assertThat(savedHakijaryhma.getJonosijat(), Matchers.hasSize(2));
    //assertThat(savedHakijaryhma.getJonosijaIdt(), Matchers.hasSize(2));
  }

  @Test
  public void allCallsSortHakijaryhmaEntitiesInPriorityAscendingOrder() throws Exception {
    List<HakijaryhmaEntity> fetched = hakijaryhmaDAO.haeHakijaryhmat("1.2.246.562.20.18895322503");
    assertEquals(3, fetched.size());

    List<HakijaryhmaEntity> sorted = new ArrayList<>(fetched);
    sorted.sort(Comparator.comparing(HakijaryhmaEntity::getPrioriteetti));

    assertEquals("Hakijaryhma entries are not sorted based on priority!", sorted, fetched);

    assertEquals(
        "lowest numeric priority should come first meaning it is the most important",
        asList("highestPriorityOid", "middlePriorityOid", "lowestPriorityOid"),
        sorted.stream().map(HakijaryhmaEntity::getHakijaryhmaOid).collect(toList()));
  }
}
