package fi.vm.sade.valintalaskenta.laskenta.dao;

import static java.util.Arrays.asList;
import static java.util.stream.Collectors.toList;
import static org.junit.Assert.assertEquals;

import fi.vm.sade.auditlog.User;
import fi.vm.sade.valintalaskenta.domain.valinta.Hakijaryhma;
import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import fi.vm.sade.valintalaskenta.laskenta.dao.impl.HakijaryhmaServiceImpl;
import fi.vm.sade.valintalaskenta.laskenta.dao.repository.HakijaryhmaRepository;
import fi.vm.sade.valintalaskenta.testing.AbstractIntegrationTest;
import java.util.*;
import org.junit.Test;
import org.junit.jupiter.api.BeforeEach;
import org.springframework.beans.factory.annotation.Autowired;

public class HakijaryhmaServiceTest extends AbstractIntegrationTest {

  @Autowired private HakijaryhmaServiceImpl hakijaryhmaDAO;

  @Autowired private HakijaryhmaRepository repo;
  private final User auditUser = null;

  private static final String HAKUKOHDE_OID = "1.2.246.562.20.18895322503";

  @BeforeEach
  public void clear() {
    repo.deleteAll();
  }

  @Test
  public void testSavingAndLoadingNewHakijaryhma() {
    Hakijaryhma hakijaryhma =
        new Hakijaryhma(
            Arrays.asList(
                createJonosija("Ruhtinas", "Nukettaja"), createJonosija("Silakka", "Markkinat")));
    hakijaryhma.hakijaryhmaOid = "uusiHakijaryhmaOid";
    hakijaryhmaDAO.create(hakijaryhma, auditUser);
    Hakijaryhma savedHakijaryhma = hakijaryhmaDAO.haeHakijaryhma("uusiHakijaryhmaOid").get();
    assertEquals(savedHakijaryhma.hakijaryhmaOid, "uusiHakijaryhmaOid");
    assertEquals(2, savedHakijaryhma.jonosija.size());
  }

  @Test
  public void allCallsSortHakijaryhmaEntitiesInPriorityAscendingOrder() {
    hakijaryhmaDAO.create(createHakijaryhma("middlePriorityOid", 2), auditUser);
    hakijaryhmaDAO.create(createHakijaryhma("lowestPriorityOid", 3), auditUser);
    hakijaryhmaDAO.create(createHakijaryhma("highestPriorityOid", 1), auditUser);
    List<Hakijaryhma> fetched = hakijaryhmaDAO.haeHakijaryhmat(HAKUKOHDE_OID);
    assertEquals(3, fetched.size());

    List<Hakijaryhma> sorted = new ArrayList<>(fetched);
    sorted.sort(Comparator.comparing(h -> h.prioriteetti));

    assertEquals("Hakijaryhma entries are not sorted based on priority!", sorted, fetched);

    assertEquals(
        "lowest numeric priority should come first meaning it is the most important",
        asList("highestPriorityOid", "middlePriorityOid", "lowestPriorityOid"),
        sorted.stream().map(h -> h.hakijaryhmaOid).collect(toList()));
  }

  private Hakijaryhma createHakijaryhma(String oid, int prioriteetti) {
    Hakijaryhma hakijaryhma = new Hakijaryhma();
    hakijaryhma.hakijaryhmaOid = oid;
    hakijaryhma.prioriteetti = prioriteetti;
    hakijaryhma.hakukohdeOid = HAKUKOHDE_OID;
    return hakijaryhma;
  }

  private Jonosija createJonosija(String etunimi, String sukunimi) {
    Jonosija jonosija = new Jonosija();
    jonosija.setEtunimi(etunimi);
    jonosija.setSukunimi(sukunimi);
    return jonosija;
  }
}
