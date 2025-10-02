package fi.vm.sade.valintalaskenta.laskenta.service.valintapiste;

import static java.time.temporal.ChronoUnit.MILLIS;

import fi.vm.sade.valintalaskenta.domain.dto.valintapiste.Osallistumistieto;
import fi.vm.sade.valintalaskenta.domain.valintapiste.Valintapiste;
import fi.vm.sade.valintalaskenta.domain.valintapiste.ValintapisteWithLastModified;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValintapisteDAO;
import java.time.ZonedDateTime;
import java.util.List;
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
    LOG.info("Init ValintapisteDaoImpl");

    ZonedDateTime lastModified = lastModified();
    LOG.info("Last modified: {}", lastModified);

    List<ValintapisteWithLastModified> bulk =
        findValintapisteBulkByTimerange(lastModified, ZonedDateTime.now(), 100, 0);
    LOG.info("ValintapisteDaoImpl bulk: {}", bulk);

    String originalLastPart = bulk.get(0).hakemusOid().split("\\.")[5];
    LOG.info("originalLastPart: {}", originalLastPart);
    String newLastPart = String.format("%020d", Integer.parseInt(originalLastPart) + 1);
    LOG.info("newLastPart: {}", newLastPart);
    String hakemusOid = bulk.get(0).hakemusOid().replace(originalLastPart, newLastPart);
    LOG.info("hakemusOid: {}", hakemusOid);

    Valintapiste valintapiste =
        new Valintapiste(hakemusOid, "tunniste1", "arvo?", Osallistumistieto.MERKITSEMATTA, "Minä");
    upsertValintapiste(valintapiste);

    bulk = findValintapisteBulkByTimerange(lastModified.minusYears(1), ZonedDateTime.now(), 100, 0);
    List<Valintapiste> pisteet =
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

    List<String> oids = modifiedSinceHakemukset(List.of(hakemusOid), lastModified.minus(1, MILLIS));
    LOG.info("ValintapisteDaoImpl modified since: {}", oids);

    // SpringApplication.exit(applicationContext);
  }

  @Transactional(readOnly = true)
  public List<Valintapiste> findValintapisteetForHakemukset(List<String> hakemusOids) {
    if (hakemusOids.size() > 32767) {
      throw new IllegalArgumentException("Max number of hakemukset is 32767!");
    }
    return valintapisteDAO.findValintapisteetForHakemukset(hakemusOids);
  }

  @Transactional(readOnly = true)
  public List<ValintapisteWithLastModified> findValintapisteBulkByTimerange(
      ZonedDateTime start, ZonedDateTime end, int limit, int offset) {
    return valintapisteDAO.findValintapisteBulkByTimerange(start, end, limit, offset);
  }

  @Transactional(readOnly = true)
  public ZonedDateTime lastModifiedForHakemukset(List<String> hakemusOids) {
    return valintapisteDAO.lastModifiedForHakemukset(hakemusOids);
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
  public ZonedDateTime lastModified() {
    return valintapisteDAO.lastModifiedASDF();
  }
}
