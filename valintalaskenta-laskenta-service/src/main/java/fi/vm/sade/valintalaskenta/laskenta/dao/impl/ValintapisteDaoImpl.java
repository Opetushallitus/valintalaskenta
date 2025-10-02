package fi.vm.sade.valintalaskenta.laskenta.dao.impl;

import fi.vm.sade.valintalaskenta.domain.dto.valintapiste.Osallistumistieto;
import fi.vm.sade.valintalaskenta.domain.valintapiste.Valintapiste;
import fi.vm.sade.valintalaskenta.domain.valintapiste.ValintapisteWithLastModified;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValintapisteDAO;
import fi.vm.sade.valintalaskenta.laskenta.dao.repository.ValintapisteRepository;
import java.sql.Timestamp;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Optional;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.core.simple.JdbcClient;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

@Component
@Transactional
public class ValintapisteDaoImpl implements ValintapisteDAO {
  private static final Logger LOG = LoggerFactory.getLogger(ValintapisteDaoImpl.class);
  private static final DateTimeFormatter timeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME;
  private static final ZoneId UTC = ZoneId.of("UTC");

  private final ValintapisteRepository repo;
  private final JdbcClient jdbcClient;

  @Autowired
  public ValintapisteDaoImpl(
      ValintapisteRepository repo, JdbcClient jdbcClient, ApplicationContext applicationContext) {
    this.repo = repo;
    this.jdbcClient = jdbcClient;
  }

  @Override
  @Transactional(readOnly = true)
  public List<Valintapiste> findValintapisteetForHakemukset(List<String> hakemusOids) {
    return repo.findAsdfASDfByHakemusOidIn(hakemusOids);
  }

  RowMapper<ValintapisteWithLastModified> valintapisteWithLastModifiedRowMapper =
      (rs, rowNum) ->
          new ValintapisteWithLastModified(
              rs.getString("hakemus_oid"),
              rs.getString("tunniste"),
              rs.getString("arvo"),
              Osallistumistieto.valueOf(rs.getString("osallistuminen")),
              rs.getString("tallettaja"),
              rs.getTimestamp("last_modified"));

  @Override
  public List<ValintapisteWithLastModified> findValintapisteBulkByTimerange(
      ZonedDateTime start, ZonedDateTime end, int limit, int offset) {
    return jdbcClient
        .sql(
            """

               select hakemus_oid,
                      tunniste,
                      arvo,
                      osallistuminen,
                      tallettaja,
                      lower(system_time) as last_modified
               from valintapiste
               where lower(system_time) >= :start::timestamptz
               and lower(system_time) < :end::timestamptz
               limit :limit offset :offset
               """)
        .param("start", start.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME))
        .param("end", end.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME))
        .param("limit", limit)
        .param("offset", offset)
        .query(valintapisteWithLastModifiedRowMapper)
        .list();
  }

  @Override
  public ZonedDateTime lastModifiedForHakemukset(List<String> hakemusOids) {
    return Optional.ofNullable(repo.findLastModifiedByHakemusOids(hakemusOids))
        .map(a -> a.toInstant().atZone(UTC))
        .orElse(null);
  }

  @Override
  public ZonedDateTime lastModifiedASDF() {
    Timestamp value =
        (Timestamp)
            jdbcClient
                .sql("select max(lower(system_time))::timestamptz from valintapiste")
                .query()
                .singleValue();
    return value.toInstant().atZone(UTC);
  }

  @Override
  public List<String> modifiedSinceHakemukset(
      List<String> hakemusOids, ZonedDateTime unmodifiedSince) {
    return repo.findModifiedSince(hakemusOids, unmodifiedSince.format(timeFormatter));
  }

  @Override
  @Transactional
  public void upsertValintapiste(Valintapiste valintapiste) {
    jdbcClient
        .sql(
            """

                insert into valintapiste (hakemus_oid, tunniste, arvo, osallistuminen, tallettaja)
                values (:hakemusOid, :tunniste, :arvo, :osallistuminen::osallistumistieto, :tallettaja)
                on conflict (hakemus_oid, tunniste) do update
                set arvo = :arvo,
                osallistuminen = :osallistuminen::osallistumistieto,
                tallettaja = :tallettaja,
                system_time = tstzrange(now(), null, '[)')
                where valintapiste.hakemus_oid = :hakemusOid and valintapiste.tunniste = :tunniste
                """)
        .param("hakemusOid", valintapiste.hakemusOid())
        .param("tunniste", valintapiste.tunniste())
        .param("arvo", valintapiste.arvo())
        .param("osallistuminen", valintapiste.osallistuminen().name())
        .param("tallettaja", valintapiste.tallettaja())
        .update();
  }

  /*

  -- name: latest-siirtotiedosto-data
  -- Returns latest successful siirtotiedosto-operation data
      select id, window_start, window_end, run_start, run_end, info, success, error_message from siirtotiedostot
      where success order by run_start desc limit 1;

  -- name: upsert-siirtotiedosto-data!
              -- Upserts siirtotiedosto-operation data
      insert into siirtotiedostot (id, window_start, window_end, run_start, run_end, info, success, error_message)
      values (:id::uuid, :window_start, :window_end, :run_start::timestamp, :run_end::timestamp,
      :info::jsonb, :success, :error_message)
      on conflict on constraint siirtotiedostot_pkey do update
      set window_start = :window_start,
      window_end = :window_end,
      run_start = :run_start,
      run_end = :run_end,
      info = :info,
      success = :success,
      error_message = :error_message;

  -- name: find-deleted
  -- Returns hakemusOids deleted in given timerange
      select distinct(h.hakemus_oid) hakemus_oid from valintapiste_history h
      where upper(h.system_time) is not null
      and upper(h.system_time) >= :start::timestamptz
      and upper(h.system_time) < :end::timestamptz
      and not exists (select 1 from valintapiste where hakemus_oid = h.hakemus_oid)
  */

}
