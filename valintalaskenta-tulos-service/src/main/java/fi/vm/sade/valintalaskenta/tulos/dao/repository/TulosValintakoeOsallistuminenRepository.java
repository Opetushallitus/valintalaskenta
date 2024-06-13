package fi.vm.sade.valintalaskenta.tulos.dao.repository;

import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import org.springframework.data.jdbc.repository.query.Query;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.query.Param;

public interface TulosValintakoeOsallistuminenRepository
    extends CrudRepository<ValintakoeOsallistuminen, UUID> {

  Optional<ValintakoeOsallistuminen> findValintakoeOsallistuminenByHakemusOid(String hakemusOid);

  @Query(
      "select distinct vo.* from valintakoe_osallistuminen vo join hakutoive h on h.valintakoe_osallistuminen = vo.id where h.hakukohde_oid = :hakutoive")
  List<ValintakoeOsallistuminen> findDistinctValintakoeOsallistuminensByHakutoive(
      @Param("hakutoive") String hakukohdeOid);

  @Query(
      "select distinct vo.* from valintakoe_osallistuminen vo join hakutoive h on h.valintakoe_osallistuminen = vo.id where h.hakukohde_oid in (:hakutoiveet)")
  List<ValintakoeOsallistuminen> findDistinctValintakoeOsallistuminensByHakutoiveet(
      @Param("hakutoiveet") List<String> hakukohdeOid);

  List<ValintakoeOsallistuminen> findDistinctValintakoeOsallistuminensByHakijaOidIn(
      List<String> hakijaOids);

  @Query(
      "select distinct vo.* from valintakoe_osallistuminen vo join hakutoive h on h.valintakoe_osallistuminen = vo.id join valintakoe_valinnanvaihe vvv on vvv.hakutoive = h.id join valintakoe vk on vk.valintakoe_valinnanvaihe = vvv.id where vo.haku_oid = :hakuOid and vk.osallistuminen = :osallistuminen")
  List<ValintakoeOsallistuminen> findDistinctByHakuAndOsallistuminen(
      @Param("hakuOid") String hakuOid, @Param("osallistuminen") Osallistuminen osallistuminen);

  @Query(
      """
      select h_oid from (
        select hakemus_oid h_oid from valintakoe_osallistuminen where created_at >= :start and created_at < :end
              union
        select vo.hakemus_oid h_oid from valintakoe_osallistuminen vo
          join hakutoive h on h.valintakoe_osallistuminen = vo.id where h.created_at >= :start and h.created_at < :end
              union
        select vo.hakemus_oid h_oid from valintakoe_osallistuminen vo
          join hakutoive h on h.valintakoe_osallistuminen =  vo.id
          join valintakoe_valinnanvaihe vvv on vvv.hakutoive = h.id
            where vvv.last_modified >= :start and vvv.last_modified < :end
              union
        select vo.hakemus_oid h_oid from valintakoe_osallistuminen vo
          join hakutoive h on h.valintakoe_osallistuminen =  vo.id
          join valintakoe_valinnanvaihe vvv on vvv.hakutoive = h.id
          join valintakoe vk on vk.valintakoe_valinnanvaihe = vvv.id
            where vk.last_modified >= :start and vk.last_modified < :end
      ) hakemus""")
  List<String> findHakemusOidsByTimeRange(
      @Param("start") LocalDateTime start, @Param("end") LocalDateTime endDatetime);

  @Query(
      """
      select h_oid from (
        select hakemus_oid h_oid from valintakoe_osallistuminen where created_at < :end
              union
        select vo.hakemus_oid h_oid from valintakoe_osallistuminen vo
          join hakutoive h on h.valintakoe_osallistuminen = vo.id where h.created_at < :end
              union
        select vo.hakemus_oid h_oid from valintakoe_osallistuminen vo
          join hakutoive h on h.valintakoe_osallistuminen =  vo.id
          join valintakoe_valinnanvaihe vvv on vvv.hakutoive = h.id where vvv.last_modified < :end
              union
        select vo.hakemus_oid h_oid from valintakoe_osallistuminen vo
          join hakutoive h on h.valintakoe_osallistuminen =  vo.id
          join valintakoe_valinnanvaihe vvv on vvv.hakutoive = h.id
          join valintakoe vk on vk.valintakoe_valinnanvaihe = vvv.id where vk.last_modified < :end) hakemus""")
  List<String> findHakemusOidsByEndTime(@Param("end") LocalDateTime endDatetime);
}
