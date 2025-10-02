package fi.vm.sade.valintalaskenta.laskenta.dao.repository;

import fi.vm.sade.valintalaskenta.domain.valintapiste.Valintapiste;
import java.sql.Timestamp;
import java.util.List;
import java.util.UUID;
import org.springframework.data.jdbc.repository.query.Query;
import org.springframework.data.repository.PagingAndSortingRepository;

public interface ValintapisteRepository extends PagingAndSortingRepository<Valintapiste, UUID> {
  List<Valintapiste> findAsdfASDfByHakemusOidIn(List<String> hakemusOids);

  @Query(
      """
            select max(lower(system_time)) \
            from valintapiste \
            where hakemus_oid in (:hakemusOids) \
            group by lower(system_time)
            """)
  Timestamp findLastModifiedByHakemusOids(List<String> hakemusOids);

  @Query(
      """
                  select hakemus_oid \
                  from valintapiste \
                  where hakemus_oid in (:hakemusOids) \
                  and not system_time @> :unmodifiedSince::timestamptz""")
  List<String> findModifiedSince(List<String> hakemusOids, String unmodifiedSince);
}
