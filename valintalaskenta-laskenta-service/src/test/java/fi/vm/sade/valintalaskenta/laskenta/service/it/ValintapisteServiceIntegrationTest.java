package fi.vm.sade.valintalaskenta.laskenta.service.it;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.within;

import fi.vm.sade.valintalaskenta.domain.dto.valintapiste.Osallistumistieto;
import fi.vm.sade.valintalaskenta.domain.dto.valintapiste.Pistetieto;
import fi.vm.sade.valintalaskenta.domain.dto.valintapiste.PistetietoWrapper;
import fi.vm.sade.valintalaskenta.domain.valintapiste.Valintapiste;
import fi.vm.sade.valintalaskenta.laskenta.service.valintapiste.ValintapisteService;
import fi.vm.sade.valintalaskenta.testing.AbstractIntegrationTest;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoUnit;
import java.util.Comparator;
import java.util.List;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EmptySource;
import org.junit.jupiter.params.provider.NullSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.beans.factory.annotation.Autowired;

public class ValintapisteServiceIntegrationTest extends AbstractIntegrationTest {

  @Autowired private ValintapisteService valintapisteService;

  private static final String HAKEMUS_OID = "hakemus-oid";
  private static final String OPPIJA_OID = "oppija-oid";
  private static final Valintapiste DEFAULT_PISTE =
      new Valintapiste(
          HAKEMUS_OID, "Tunniste", "42.0", Osallistumistieto.OSALLISTUI, "tallettaja-oid");
  private static final Valintapiste OTHER_PISTE =
      new Valintapiste(
          HAKEMUS_OID, "Tunniste2", null, Osallistumistieto.EI_OSALLISTUNUT, "toinen-talletttaja");

  @Nested
  public class FindValintapisteetForHakemus {
    @Test
    public void returnsEmptyPisteetWhenHakemusOidNotFound() {
      Pair<ZonedDateTime, PistetietoWrapper> result =
          valintapisteService.findValintapisteetForHakemus("Non-existing", OPPIJA_OID);

      assertThat(result.getLeft()).isNull();
      assertThat(result.getRight())
          .isEqualTo(new PistetietoWrapper("Non-existing", OPPIJA_OID, List.of()));
    }

    @Test
    public void returnsPisteetWhenHakemusOidFound() {
      valintapisteService.upsertValintapiste(DEFAULT_PISTE);
      valintapisteService.upsertValintapiste(OTHER_PISTE);

      Pair<ZonedDateTime, PistetietoWrapper> result =
          valintapisteService.findValintapisteetForHakemus(HAKEMUS_OID, OPPIJA_OID);

      assertThat(result.getLeft())
          .isNotNull()
          .isCloseTo(ZonedDateTime.now(), within(1, ChronoUnit.SECONDS));
      List<Pistetieto> expectedPisteet =
          List.of(DEFAULT_PISTE.toPistetieto(), OTHER_PISTE.toPistetieto());
      assertThat(result.getRight())
          .isEqualTo(new PistetietoWrapper(HAKEMUS_OID, OPPIJA_OID, expectedPisteet));
    }

    @ParameterizedTest
    @NullSource
    @EmptySource
    @ValueSource(strings = "oppija")
    public void echoesOppijaOidBlindly(String oppijaOid) {
      Pair<ZonedDateTime, PistetietoWrapper> result =
          valintapisteService.findValintapisteetForHakemus(HAKEMUS_OID, oppijaOid);

      assertThat(result.getRight().oppijaOID()).isEqualTo(oppijaOid);
    }
  }

  @Nested
  public class FindValintapisteetForHakemukset {
    @Test
    public void returnsEmptyListWhenHakemusOidsIsEmpty() {
      Pair<ZonedDateTime, List<PistetietoWrapper>> result =
          valintapisteService.findValintapisteetForHakemukset(List.of());

      assertThat(result.getLeft()).isNull();
      assertThat(result.getRight()).isEmpty();
    }

    @Test
    public void returnsBothMatchingAndNotMatching() {
      valintapisteService.upsertValintapiste(DEFAULT_PISTE);
      valintapisteService.upsertValintapiste(OTHER_PISTE);

      Pair<ZonedDateTime, List<PistetietoWrapper>> result =
          valintapisteService.findValintapisteetForHakemukset(List.of(HAKEMUS_OID, "Non-existing"));

      assertThat(result.getLeft())
          .isNotNull()
          .isCloseTo(ZonedDateTime.now(), within(1, ChronoUnit.SECONDS));
      assertThat(result.getRight()).hasSize(2);
      List<PistetietoWrapper> pistetiedot = sortByOid(result.getRight());
      assertThat(pistetiedot.get(0)).isEqualTo(new PistetietoWrapper("Non-existing", List.of()));
      List<Pistetieto> expectedPisteet =
          List.of(DEFAULT_PISTE.toPistetieto(), OTHER_PISTE.toPistetieto());
      assertThat(pistetiedot.get(1)).isEqualTo(new PistetietoWrapper(HAKEMUS_OID, expectedPisteet));
    }

    private List<PistetietoWrapper> sortByOid(List<PistetietoWrapper> pistetiedot) {
      return pistetiedot.stream()
          .sorted(Comparator.comparing(PistetietoWrapper::hakemusOID))
          .toList();
    }
  }
}
