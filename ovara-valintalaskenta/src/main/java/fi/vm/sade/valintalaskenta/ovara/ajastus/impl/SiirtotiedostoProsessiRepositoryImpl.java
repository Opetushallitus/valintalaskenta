package fi.vm.sade.valintalaskenta.ovara.ajastus.impl;

import fi.vm.sade.valintalaskenta.ovara.ajastus.SiirtotiedostoProsessi;
import fi.vm.sade.valintalaskenta.ovara.ajastus.SiirtotiedostoProsessiRepository;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
import javax.transaction.Transactional;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;

@Repository
@Transactional
public class SiirtotiedostoProsessiRepositoryImpl implements SiirtotiedostoProsessiRepository {

  private static final Logger logger =
      LoggerFactory.getLogger(SiirtotiedostoProsessiRepositoryImpl.class);

  @PersistenceContext private EntityManager entityManager;
  private final JdbcTemplate jdbcTemplate;

  public SiirtotiedostoProsessiRepositoryImpl(JdbcTemplate jdbcTemplate) {
    this.jdbcTemplate = jdbcTemplate;
  }

  @Override
  public SiirtotiedostoProsessi findLatestSuccessful() {
    String sT =
        """
                        SELECT execution_uuid, window_start, window_end, run_start, run_end, cast(info as varchar), success, error_message from siirtotiedosto where success order by run_end desc limit 1""";
    Query query = entityManager.createNativeQuery(sT);
    Object[] result = (Object[]) query.getSingleResult();
    return new SiirtotiedostoProsessi(result);
  }

  @Override
  public void persist(SiirtotiedostoProsessi sp) {
    logger.info("Persisting: {}", sp);
    String infoStr = sp.getInfo() != null ? sp.getInfo() : "{}";
    this.jdbcTemplate.update(
        "insert into siirtotiedosto (execution_uuid, window_start, window_end, run_start, run_end, info, success, error_message) "
            + "values (?::uuid, ?::timestamptz, ?::timestamptz, ?::timestamptz, ?::timestamptz, ?::jsonb, ?, ?) on conflict (execution_uuid) do update "
            + "set run_end = ?::timestamptz, info = ?::jsonb, success = ?, error_message = ?",
        sp.getExecutionUuid(),
        sp.getWindowStart(),
        sp.getWindowEnd(),
        sp.getRunStart(),
        sp.getRunEnd(),
        infoStr,
        sp.getSuccess(),
        sp.getErrorMessage(),
        sp.getRunEnd(),
        sp.getInfo(),
        sp.getSuccess(),
        sp.getErrorMessage());
  }
}
