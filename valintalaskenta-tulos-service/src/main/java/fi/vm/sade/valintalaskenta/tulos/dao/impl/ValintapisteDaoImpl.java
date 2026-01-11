package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import fi.vm.sade.valintalaskenta.domain.valintapiste.Valintapiste;
import fi.vm.sade.valintalaskenta.domain.valintapiste.ValintapisteWithLastModified;
import fi.vm.sade.valintalaskenta.tulos.dao.ValintapisteDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.repository.ValintapisteRepository;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

@Component
@Transactional
public class ValintapisteDaoImpl implements ValintapisteDAO {
  private static final Logger LOG = LoggerFactory.getLogger(ValintapisteDaoImpl.class);
  private static final ZoneId UTC = ZoneId.of("UTC");

  private final ValintapisteRepository repo;

  @Autowired
  public ValintapisteDaoImpl(ValintapisteRepository repo) {
    this.repo = repo;
  }

  @Override
  @Transactional(readOnly = true)
  public List<Valintapiste> findByHakemusId(String hakemusOid) {
    return repo.findByHakemusOidIn(List.of(hakemusOid));
  }

  @Override
  @Transactional(readOnly = true)
  public List<Valintapiste> findValintapisteetForHakemukset(Collection<String> hakemusOids) {
    return repo.findByHakemusOidIn(hakemusOids);
  }

  @Override
  public List<ValintapisteWithLastModified> findValintapisteBulkByTimerange(
      LocalDateTime start, LocalDateTime end, int limit, int offset) {
    return repo.findValintapisteBulkByTimerange(start, end, limit, offset);
  }

  @Override
  public List<String> findDeleted(LocalDateTime start, LocalDateTime end) {
    return repo.findDeleted(start, end);
  }

  @Override
  public Optional<ZonedDateTime> lastModifiedForHakemukset(Collection<String> hakemusOids) {
    return Optional.ofNullable(repo.findLastModifiedByHakemusOids(hakemusOids))
        .map(a -> a.toInstant().atZone(UTC));
  }

  @Override
  public List<String> modifiedSinceHakemukset(List<String> hakemusOids, String unmodifiedSince) {
    return repo.findModifiedSince(hakemusOids, unmodifiedSince);
  }

  @Override
  @Transactional
  public void upsertValintapiste(Valintapiste valintapiste) {
    LOG.info(
        "Kirjoitetaan kantaan pistetieto hakemukselle {} tunnisteella {}. Arvo: {}, osallistuminen: {}",
        valintapiste.hakemusOid(),
        valintapiste.tunniste(),
        valintapiste.arvo(),
        valintapiste.osallistuminen());
    repo.upsertValintapiste(
        valintapiste.hakemusOid(),
        valintapiste.tunniste(),
        valintapiste.arvo(),
        valintapiste.osallistuminen().name(),
        valintapiste.tallettaja());
  }
}
