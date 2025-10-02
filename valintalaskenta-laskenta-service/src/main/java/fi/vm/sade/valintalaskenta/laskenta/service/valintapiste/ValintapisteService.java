package fi.vm.sade.valintalaskenta.laskenta.service.valintapiste;

import static java.time.temporal.ChronoUnit.MILLIS;

import fi.vm.sade.valintalaskenta.domain.dto.valintapiste.Osallistumistieto;
import fi.vm.sade.valintalaskenta.domain.dto.valintapiste.Pistetieto;
import fi.vm.sade.valintalaskenta.domain.dto.valintapiste.PistetietoWrapper;
import fi.vm.sade.valintalaskenta.domain.valintapiste.Valintapiste;
import fi.vm.sade.valintalaskenta.domain.valintapiste.ValintapisteWithLastModified;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValintapisteDAO;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.ApplicationContext;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class ValintapisteService {
  private static final Logger LOG = LoggerFactory.getLogger(ValintapisteService.class);

  private final ValintapisteDAO valintapisteDAO;
  private final ApplicationContext applicationContext;

  @Autowired
  ValintapisteService(ValintapisteDAO dao, ApplicationContext applicationContext) {
    this.valintapisteDAO = dao;
    this.applicationContext = applicationContext;
  }

  @EventListener(ApplicationReadyEvent.class)
  public void asdf() {
    insertLots(20000, 20000);

    LOG.info("Init ValintapisteService");

    Optional<ZonedDateTime> lastModified = lastModified();
    LOG.info("Last modified: {}", lastModified);

    List<ValintapisteWithLastModified> bulk =
        findValintapisteBulkByTimerange(lastModified.orElseThrow(), ZonedDateTime.now(), 100, 0);
    LOG.info("ValintapisteDaoImpl bulk: {}", bulk);

    String originalLastPart = bulk.get(0).hakemusOid().split("\\.")[5];
    LOG.info("originalLastPart: {}", originalLastPart);
    int number = Integer.parseInt(originalLastPart) + 1;
    String newLastPart = String.format("%020d", number);
    LOG.info("newLastPart: {}", newLastPart);
    String hakemusOid = bulk.get(0).hakemusOid().replace(originalLastPart, newLastPart);
    LOG.info("hakemusOid: {}", hakemusOid);

    Valintapiste valintapiste =
        new Valintapiste(hakemusOid, "tunniste1", "arvo?", Osallistumistieto.MERKITSEMATTA, "Minä");
    upsertValintapiste(valintapiste);

    bulk =
        findValintapisteBulkByTimerange(
            lastModified.orElseThrow().minusYears(1), ZonedDateTime.now(), 100, 0);
    Pair<ZonedDateTime, List<PistetietoWrapper>> pisteet =
        findValintapisteetForHakemukset(
            bulk.stream().map(ValintapisteWithLastModified::hakemusOid).toList());
    LOG.info("ValintapisteDaoImpl All pisteet: {}", pisteet);

    valintapiste = valintapiste.withArvo("Asetettu Arvo!");
    upsertValintapiste(valintapiste);
    pisteet = findValintapisteetForHakemukset(List.of(hakemusOid));
    LOG.info("ValintapisteDaoImpl pisteet: {}", pisteet);

    lastModified = lastModifiedForHakemukset(List.of(hakemusOid));
    LOG.info("Last modified: {}", lastModified);

    bulk =
        findValintapisteBulkByTimerange(
            ZonedDateTime.parse("2020-01-01T00:00:00Z"), ZonedDateTime.now(), 100, 0);
    LOG.info("ValintapisteDaoImpl bulk: {}", bulk);

    List<String> oids =
        modifiedSinceHakemukset(List.of(hakemusOid), lastModified.orElseThrow().minus(1, MILLIS));
    LOG.info("ValintapisteDaoImpl modified since: {}", oids);

    // SpringApplication.exit(applicationContext);
  }

  @Transactional(readOnly = true)
  public Pair<ZonedDateTime, PistetietoWrapper> findValintapisteetForHakemus(
      String hakemusOid, String oppijaId) {
    Pair<ZonedDateTime, List<PistetietoWrapper>> result =
        findValintapisteetForHakemukset(List.of(hakemusOid));
    return Pair.of(result.getLeft(), result.getRight().get(0).withOppijaOID(oppijaId));
  }

  @Transactional(readOnly = true)
  public Pair<ZonedDateTime, List<PistetietoWrapper>> findValintapisteetForHakemukset(
      List<String> hakemusOids) {
    if (hakemusOids.size() > 32767) {
      throw new IllegalArgumentException("Max number of hakemukset is 32767!");
    }
    if (hakemusOids.isEmpty()) {
      return Pair.of(null, List.of());
    }

    Map<String, List<Pistetieto>> pisteetByHakemukset =
        valintapisteDAO.findValintapisteetForHakemukset(hakemusOids).stream()
            .collect(
                Collectors.groupingBy(
                    Valintapiste::hakemusOid,
                    Collectors.mapping(Valintapiste::toPistetieto, Collectors.toList())));

    Stream<PistetietoWrapper> missingHakemusOids =
        hakemusOids.stream()
            .filter(oid -> !pisteetByHakemukset.containsKey(oid))
            .map(PistetietoWrapper::new);

    List<PistetietoWrapper> pisteet =
        Stream.concat(
                pisteetByHakemukset.entrySet().stream()
                    .map(kv -> new PistetietoWrapper(kv.getKey(), kv.getValue())),
                missingHakemusOids)
            .toList();

    Optional<ZonedDateTime> lastModified = lastModifiedForHakemukset(hakemusOids);

    return Pair.of(lastModified.orElse(null), pisteet);
  }

  @Transactional(readOnly = true)
  public List<ValintapisteWithLastModified> findValintapisteBulkByTimerange(
      ZonedDateTime start, ZonedDateTime end, int limit, int offset) {
    return valintapisteDAO.findValintapisteBulkByTimerange(start, end, limit, offset);
  }

  @Transactional(readOnly = true)
  public Optional<ZonedDateTime> lastModifiedForHakemukset(List<String> hakemusOids) {
    // En tiedä miksi tähän lisätään 1 sekunti. Näin tehtiin valintapiste-servicessä.
    return valintapisteDAO
        .lastModifiedForHakemukset(hakemusOids)
        .map(a -> a.withZoneSameInstant(ZoneId.of("Europe/Helsinki")).plusSeconds(1));
  }

  @Transactional
  public void upsertValintapiste(Valintapiste valintapiste) {
    valintapisteDAO.upsertValintapiste(valintapiste);
  }

  @Transactional(readOnly = true)
  public List<String> modifiedSinceHakemukset(
      List<String> hakemusOids, ZonedDateTime unmodifiedSince) {
    return valintapisteDAO.modifiedSinceHakemukset(hakemusOids, unmodifiedSince);
  }

  @Transactional(readOnly = true)
  public Optional<ZonedDateTime> lastModified() {
    return valintapisteDAO.lastModifiedASDF();
  }

  private void insertLots(int start, int n) {
    for (int i = start; i <= start + n; i++) {

      String hakemusOid = String.format("1.2.246.562.11.%020d", i);
      if (i % 100 == 0) {
        LOG.info("Inserting Valintapiste {}", hakemusOid);
      }

      for (int tunniste = 1; tunniste <= i % 10; ++tunniste) {
        Valintapiste valintapiste =
            new Valintapiste(
                hakemusOid,
                String.format("tunniste-%02d", tunniste),
                Integer.toString((i + tunniste) % 99),
                Osallistumistieto.values()[(i + tunniste) % 4],
                "Minä");

        upsertValintapiste(valintapiste);
      }
    }
  }
}
