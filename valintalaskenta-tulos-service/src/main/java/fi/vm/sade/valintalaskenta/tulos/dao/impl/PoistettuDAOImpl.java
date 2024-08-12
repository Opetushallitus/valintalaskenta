package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import fi.vm.sade.valintalaskenta.domain.siirtotiedosto.Poistettu;
import fi.vm.sade.valintalaskenta.tulos.dao.PoistettuDAO;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
import org.springframework.stereotype.Repository;

@Repository
public class PoistettuDAOImpl implements PoistettuDAO {
  @PersistenceContext private EntityManager entityManager;

  @Override
  public List<Poistettu> findPoistetut(
      EntityType entityType, LocalDateTime startDateTime, LocalDateTime endDateTime) {
    return switch (entityType) {
      case VALINNANVAIHE -> doFindPoistetut(
          "valinnanvaihe",
          "valinnanvaihe_history",
          "null",
          "valinnanvaihe_oid",
          startDateTime,
          endDateTime);
      case VALINTATAPAJONO -> doFindPoistetut(
          "valintatapajono",
          "valintatapajono_history",
          "valinnanvaihe",
          "valintatapajono_oid",
          startDateTime,
          endDateTime);
      case JONOSIJA -> doFindPoistetut(
          "jonosija",
          "jonosija_history",
          "valintatapajono",
          "hakemus_oid",
          startDateTime,
          endDateTime);
      case VALINTAKOE_OSALLISTUMINEN -> doFindPoistetut(
          "valintakoe_osallistuminen",
          "valintakoe_osallistuminen_history",
          "null",
          "hakemus_oid",
          startDateTime,
          endDateTime);
      case HAKUTOIVE -> doFindPoistetut(
          "hakutoive",
          "hakutoive_history",
          "valintakoe_osallistuminen",
          "hakukohde_oid",
          startDateTime,
          endDateTime);
      case VALINTAKOE_VALINNANVAIHE -> doFindPoistetut(
          "valintakoe_valinnanvaihe",
          "valintakoe_valinnanvaihe_history",
          "hakutoive",
          "valinnanvaihe_oid",
          startDateTime,
          endDateTime);
      case VALINTAKOE -> doFindPoistetut(
          "valintakoe",
          "valintakoe_history",
          "valintakoe_valinnanvaihe",
          "valintakoe_oid",
          startDateTime,
          endDateTime);
    };
  }

  private List<Poistettu> doFindPoistetut(
      String tableName,
      String historyTableName,
      String parentIdFieldName,
      String tunnisteFieldName,
      LocalDateTime start,
      LocalDateTime end) {
    String sqlTemplate =
        """
            select distinct on (id) id, %s as parentId, %s from %s hist
              where upper(hist.system_time) is not null and
                upper(hist.system_time) >= :startDateTime and
                upper(hist.system_time) <  :endDateTime and
                not exists (select 1 from %s where id = hist.id)""";
    String tunnisteSelectPart =
        "tunniste".equals(tunnisteFieldName)
            ? "tunniste"
            : String.format("%s as tunniste", tunnisteFieldName);
    String sql =
        String.format(
            sqlTemplate, parentIdFieldName, tunnisteSelectPart, historyTableName, tableName);
    Query query =
        entityManager
            .createNativeQuery(sql)
            .setParameter("startDateTime", start)
            .setParameter("endDateTime", end);
    @SuppressWarnings("unchecked")
    List<Object[]> resultList = (List<Object[]>) query.getResultList();
    return resultList.stream().map(Poistettu::new).collect(Collectors.toList());
  }

  @Override
  public List<Poistettu> findParents(EntityType parentType, Collection<UUID> ids) {
    return switch (parentType) {
      case VALINNANVAIHE -> doFindParents("valinnanvaihe", "null", "valinnanvaihe_oid", ids);
      case VALINTATAPAJONO -> doFindParents(
          "valintatapajono", "valinnanvaihe", "valintatapajono_oid", ids);
      case VALINTAKOE_OSALLISTUMINEN -> doFindParents(
          "valintakoe_osallistuminen", "null", "hakemus_oid", ids);
      case HAKUTOIVE -> doFindParents(
          "hakutoive", "valintakoe_osallistuminen", "hakukohde_oid", ids);
      case VALINTAKOE_VALINNANVAIHE -> doFindParents(
          "valintakoe_valinnanvaihe", "hakutoive", "valinnanvaihe_oid", ids);
      default -> new ArrayList<>();
    };
  }

  private List<Poistettu> doFindParents(
      String tableName, String parentIdFieldName, String tunnisteFieldName, Collection<UUID> ids) {
    String sqlTemplate =
        """
        select distinct on (id) id, %s as parentId, %s as tunniste from %s
        where id in (:ids)""";
    String sql = String.format(sqlTemplate, parentIdFieldName, tunnisteFieldName, tableName);
    Query query = entityManager.createNativeQuery(sql).setParameter("ids", ids);
    @SuppressWarnings("unchecked")
    List<Object[]> resultList = (List<Object[]>) query.getResultList();
    return resultList.stream()
        .map(Poistettu::new)
        .map(p -> p.setDeletedItself(false))
        .collect(Collectors.toList());
  }
}
