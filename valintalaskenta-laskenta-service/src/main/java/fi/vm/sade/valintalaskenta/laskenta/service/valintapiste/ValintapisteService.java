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
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.SpringApplication;
import org.springframework.context.ApplicationContext;
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

  // @EventListener(ApplicationReadyEvent.class)
  @Transactional
  public void asdf() {
    // insertLots(20000, 20000);

    LOG.info("Init ValintapisteService");

    Optional<ZonedDateTime> lastModified = valintapisteDAO.lastModifiedASDF();
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
    valintapisteDAO.upsertValintapiste(valintapiste);

    bulk =
        findValintapisteBulkByTimerange(
            lastModified.orElseThrow().minusYears(1), ZonedDateTime.now(), 100, 0);
    Pair<ZonedDateTime, List<PistetietoWrapper>> pisteet =
        findValintapisteetForHakemukset(
            bulk.stream().map(ValintapisteWithLastModified::hakemusOid).toList());
    LOG.info("ValintapisteDaoImpl All pisteet: {}", pisteet);

    valintapiste = valintapiste.withArvo("Asetettu Arvo!");
    valintapisteDAO.upsertValintapiste(valintapiste);
    pisteet = findValintapisteetForHakemukset(List.of(hakemusOid));
    LOG.info("ValintapisteDaoImpl pisteet: {}", pisteet);

    lastModified = lastModifiedForHakemukset(List.of(hakemusOid));
    LOG.info("Last modified: {}", lastModified);

    bulk =
        findValintapisteBulkByTimerange(
            ZonedDateTime.parse("2020-01-01T00:00:00Z"), ZonedDateTime.now(), 100, 0);
    LOG.info("ValintapisteDaoImpl bulk: {}", bulk);

    String unmodifiedSince =
        lastModified.orElseThrow().minus(1, MILLIS).format(DateTimeFormatter.ISO_OFFSET_DATE_TIME);
    List<String> oids =
        valintapisteDAO.modifiedSinceHakemukset(List.of(hakemusOid), unmodifiedSince);
    LOG.info("ValintapisteDaoImpl modified since: {}", oids);

    SpringApplication.exit(applicationContext);
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
        groupByHakemus(valintapisteDAO.findValintapisteetForHakemukset(hakemusOids));

    Stream<PistetietoWrapper> missingHakemusOids =
        hakemusOids.stream()
            .filter(oid -> !pisteetByHakemukset.containsKey(oid))
            .map(PistetietoWrapper::new);

    List<PistetietoWrapper> pisteet =
        Stream.concat(convertToWrappers(pisteetByHakemukset), missingHakemusOids).toList();

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
    // Tietokannan aikaleima on tarkempi kuin Javan. Lisätään aikaan sekunti, jotta palautetun
    // leiman uudelleenkäyttö ei aiheuta virheitä kutsujalle.
    return valintapisteDAO
        .lastModifiedForHakemukset(hakemusOids)
        .map(a -> a.withZoneSameInstant(ZoneId.of("Europe/Helsinki")).plusSeconds(1));
  }

  @Transactional
  public List<String> insertValintapisteet(
      List<PistetietoWrapper> pisteet, boolean savePartially, String ifUnmodifiedSince) {
    List<String> hakemusOids = pisteet.stream().map(PistetietoWrapper::hakemusOID).toList();
    List<String> conflictingOids = checkUpdateConflict(hakemusOids, ifUnmodifiedSince);
    LOG.info("ASDF insertValintapisteet Conflicting Oids {}", conflictingOids);

    if (conflictingOids.isEmpty() || savePartially) {
      pisteet.stream()
          .filter(wrapper -> !conflictingOids.contains(wrapper.hakemusOID()))
          .flatMap(this::createValintapisteet)
          .forEach(valintapisteDAO::upsertValintapiste);
    }

    return conflictingOids;
  }

  private Stream<Valintapiste> createValintapisteet(PistetietoWrapper wrapper) {
    return wrapper.pisteet().stream().map(piste -> createValintapiste(piste, wrapper.hakemusOID()));
  }

  private Valintapiste createValintapiste(Pistetieto pistetieto, String hakemusOid) {
    return new Valintapiste(
        hakemusOid,
        pistetieto.tunniste(),
        pistetieto.arvo(),
        pistetieto.osallistuminen(),
        pistetieto.tallettaja());
  }

  private List<String> checkUpdateConflict(List<String> hakemusOids, String unmodifiedSince) {
    if (unmodifiedSince == null) {
      return List.of();
    }
    return valintapisteDAO.modifiedSinceHakemukset(hakemusOids, unmodifiedSince);
  }

  public static Map<String, List<Pistetieto>> groupByHakemus(List<Valintapiste> valintapisteet) {
    return valintapisteet.stream()
        .collect(
            Collectors.groupingBy(
                Valintapiste::hakemusOid,
                Collectors.mapping(Valintapiste::toPistetieto, Collectors.toList())));
  }

  public static Stream<PistetietoWrapper> convertToWrappers(Map<String, List<Pistetieto>> pisteet) {
    return pisteet.entrySet().stream().map(kv -> new PistetietoWrapper(kv.getKey(), kv.getValue()));
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

        valintapisteDAO.upsertValintapiste(valintapiste);
      }
    }
  }
}
