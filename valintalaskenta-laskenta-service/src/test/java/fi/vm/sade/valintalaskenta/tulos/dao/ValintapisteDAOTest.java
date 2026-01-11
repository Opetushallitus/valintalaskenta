package fi.vm.sade.valintalaskenta.tulos.dao;

import static org.assertj.core.api.Assertions.assertThat;

import fi.vm.sade.valintalaskenta.domain.dto.valintapiste.Osallistumistieto;
import fi.vm.sade.valintalaskenta.domain.valintapiste.Valintapiste;
import fi.vm.sade.valintalaskenta.domain.valintapiste.ValintapisteWithLastModified;
import fi.vm.sade.valintalaskenta.testing.AbstractIntegrationTest;
import fi.vm.sade.valintalaskenta.tulos.dao.repository.ValintapisteRepository;
import java.sql.Timestamp;
import java.time.Instant;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

public class ValintapisteDAOTest extends AbstractIntegrationTest {
  @Autowired ValintapisteDAO dao;
  @Autowired ValintapisteRepository valintapisteRepository;

  static final String HAKEMUS_OID = "hakemus-oid";
  static final Valintapiste DEFAULT_VALINTAPISTE =
      new Valintapiste(HAKEMUS_OID, "tunniste", "arvo", Osallistumistieto.OSALLISTUI, "Joku");

  @Nested
  public class FindValintapisteBulkByTimerange {
    @Test
    public void returnsEmptyListWhenNoValintapiste() {
      List<ValintapisteWithLastModified> result =
          dao.findValintapisteBulkByTimerange(LocalDateTime.MIN, LocalDateTime.now(), 100, 0);

      assertThat(result).isEmpty();
    }

    @Test
    public void returnsValintapisteInSeveralPages() {
      IntStream.range(1, 6)
          .forEach(
              i -> dao.upsertValintapiste(DEFAULT_VALINTAPISTE.withTunniste(Integer.toString(i))));

      List<ValintapisteWithLastModified> firstPage =
          dao.findValintapisteBulkByTimerange(LocalDateTime.MIN, LocalDateTime.now(), 3, 0);
      List<ValintapisteWithLastModified> secondPage =
          dao.findValintapisteBulkByTimerange(
              LocalDateTime.MIN, LocalDateTime.now(), 3, firstPage.size());

      assertThat(firstPage).hasSize(3);
      assertThat(secondPage).hasSize(2);
      Set<Integer> tunnisteet =
          Stream.concat(firstPage.stream(), secondPage.stream())
              .map(v -> Integer.parseInt(v.tunniste()))
              .collect(Collectors.toSet());
      assertThat(tunnisteet).containsExactlyInAnyOrder(1, 2, 3, 4, 5);
      List<Timestamp> lastModifieds =
          Stream.concat(firstPage.stream(), secondPage.stream())
              .map(ValintapisteWithLastModified::lastModified)
              .toList();
      assertThat(lastModifieds).allSatisfy(v -> assertThat(v).isCloseTo(Instant.now(), 100));
    }
  }

  @Nested
  public class FindDeleted {
    @Test
    public void returnsEmptyListWhenNoValintapiste() {
      List<String> result = dao.findDeleted(LocalDateTime.MIN, LocalDateTime.now());

      assertThat(result).isEmpty();
    }

    @Test
    public void findsDeleted() {
      dao.upsertValintapiste(DEFAULT_VALINTAPISTE);
      valintapisteRepository.deleteByHakemusOid(HAKEMUS_OID);
      assertThat(valintapisteRepository.findByHakemusOidIn(List.of(HAKEMUS_OID))).isEmpty();

      List<String> result = dao.findDeleted(LocalDateTime.MIN, LocalDateTime.now());

      assertThat(result).containsExactly(HAKEMUS_OID);
    }
  }
}
