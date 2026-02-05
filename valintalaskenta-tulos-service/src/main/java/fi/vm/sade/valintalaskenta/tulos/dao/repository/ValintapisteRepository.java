package fi.vm.sade.valintalaskenta.tulos.dao.repository;

import fi.vm.sade.valintalaskenta.domain.valintapiste.Valintapiste;
import fi.vm.sade.valintalaskenta.domain.valintapiste.ValintapisteWithLastModified;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.Collection;
import java.util.List;
import org.springframework.data.jdbc.repository.query.Modifying;
import org.springframework.data.jdbc.repository.query.Query;
import org.springframework.data.repository.Repository;

// Spring Data JDBC ei tue vielä composite keytä, joten id on tyhjä.
// CrudRepositoryn metodit eivät toimisi, joten käytetään pelkkää Repositorya.
// 4.0 lisää tuen composite keylle, joten tähän voi palata kun se on julkaistu.
public interface ValintapisteRepository extends Repository<Valintapiste, Void> {
  List<Valintapiste> findByHakemusOidIn(Collection<String> hakemusOids);

  @Query(
      """
         select max(lower(system_time))
         from valintapiste
         where hakemus_oid in (:hakemusOids)""")
  Timestamp findLastModifiedByHakemusOids(Collection<String> hakemusOids);

  @Query(
      """
         select hakemus_oid
         from valintapiste
         where hakemus_oid in (:hakemusOids)
         and not system_time @> :unmodifiedSince::timestamptz""")
  List<String> findModifiedSince(List<String> hakemusOids, String unmodifiedSince);

  @Query(
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
         limit :limit offset :offset""")
  List<ValintapisteWithLastModified> findValintapisteBulkByTimerange(
      LocalDateTime start, LocalDateTime end, int limit, int offset);

  @Query(
      """
         select distinct(h.hakemus_oid) hakemus_oid from valintapiste_history h
         where upper(h.system_time) is not null
         and upper(h.system_time) >= :start::timestamptz
         and upper(h.system_time) < :end::timestamptz
         and not exists (select 1 from valintapiste where hakemus_oid = h.hakemus_oid)""")
  List<String> findDeleted(LocalDateTime start, LocalDateTime end);

  @Modifying
  @Query(
      """
         insert into valintapiste (hakemus_oid, tunniste, arvo, osallistuminen, tallettaja)
         values (:hakemusOid, :tunniste, :arvo, :osallistuminen::osallistumistieto, :tallettaja)
         on conflict (hakemus_oid, tunniste) do update
         set arvo = :arvo,
         osallistuminen = :osallistuminen::osallistumistieto,
         tallettaja = :tallettaja,
         system_time = tstzrange(now(), null, '[)')
         where valintapiste.hakemus_oid = :hakemusOid and valintapiste.tunniste = :tunniste""")
  void upsertValintapiste(
      String hakemusOid, String tunniste, String arvo, String osallistuminen, String tallettaja);

  @Modifying
  @Query("truncate table valintapiste, valintapiste_history")
  void truncateWithHistory();

  @Modifying
  @Query("delete from valintapiste where hakemus_oid = :hakemusOid")
  void deleteByHakemusOid(String hakemusOid);
}
