package fi.vm.sade.valintalaskenta.laskenta.dao;

import static java.util.Arrays.asList;
import static java.util.stream.Collectors.toList;
import static org.junit.jupiter.api.Assertions.assertEquals;

import fi.vm.sade.auditlog.User;
import fi.vm.sade.valintalaskenta.domain.valinta.Hakijaryhma;
import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import fi.vm.sade.valintalaskenta.laskenta.dao.impl.HakijaryhmaDAOImpl;
import fi.vm.sade.valintalaskenta.laskenta.dao.repository.HakijaryhmaHistoryRepository;
import fi.vm.sade.valintalaskenta.testing.AbstractIntegrationTest;
import java.util.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

public class HakijaryhmaDAOTest extends AbstractIntegrationTest {

  @Autowired private HakijaryhmaDAOImpl hakijaryhmaDAO;

  @Autowired private HakijaryhmaHistoryRepository historyRepo;
  private final User auditUser = null;

  private static final String HAKUKOHDE_OID = "1.2.246.562.20.18895322503";

  @BeforeEach
  public void clear() {
    historyRepo.deleteAll();
  }

  @Test
  public void testSavingAndLoadingNewHakijaryhma() {
    Hakijaryhma hakijaryhma =
        new Hakijaryhma(Arrays.asList(createJonosija("ruh-nuk"), createJonosija("sil-mak")));
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

    assertEquals(sorted, fetched, "Hakijaryhma entries are not sorted based on priority!");

    assertEquals(
        asList("highestPriorityOid", "middlePriorityOid", "lowestPriorityOid"),
        sorted.stream().map(h -> h.hakijaryhmaOid).collect(toList()),
        "lowest numeric priority should come first meaning it is the most important");
  }

  @Test
  public void modifyingHakijaryhmaInsertsRowToHistory() {
    Hakijaryhma hakijaryhma =
      new Hakijaryhma(Arrays.asList(createJonosija("ruh-nuk"), createJonosija("sil-mak")));
    hakijaryhma.hakijaryhmaOid = "wanhaHakijaryhmaOid";
    hakijaryhmaDAO.create(hakijaryhma, auditUser);

    assertEquals(0, historyRepo.findAll().spliterator().estimateSize());

    hakijaryhma = hakijaryhmaDAO.haeHakijaryhma("wanhaHakijaryhmaOid").orElseThrow();
    hakijaryhma.prioriteetti = 8;
    hakijaryhmaDAO.create(hakijaryhma, auditUser);

    assertEquals(1, historyRepo.findAll().spliterator().estimateSize());
  }

  @Test
  public void deletingHakijaryhmaInsertsRowToHistory() {
    Hakijaryhma hakijaryhma =
      new Hakijaryhma(Arrays.asList(createJonosija("ruh-nuk"), createJonosija("sil-mak")));
    hakijaryhma.hakijaryhmaOid = "wanhaHakijaryhmaOid";
    hakijaryhmaDAO.create(hakijaryhma, auditUser);

    assertEquals(0, historyRepo.findAll().spliterator().estimateSize());

    hakijaryhmaDAO.poistaHakijaryhma(hakijaryhma);

    assertEquals(1, historyRepo.findAll().spliterator().estimateSize());
  }

  private Hakijaryhma createHakijaryhma(String oid, int prioriteetti) {
    Hakijaryhma hakijaryhma = new Hakijaryhma();
    hakijaryhma.hakijaryhmaOid = oid;
    hakijaryhma.prioriteetti = prioriteetti;
    hakijaryhma.hakukohdeOid = HAKUKOHDE_OID;
    return hakijaryhma;
  }

  private Jonosija createJonosija(String hakijaOid) {
    Jonosija jonosija = new Jonosija();
    jonosija.setHakijaOid(hakijaOid);
    return jonosija;
  }
}
