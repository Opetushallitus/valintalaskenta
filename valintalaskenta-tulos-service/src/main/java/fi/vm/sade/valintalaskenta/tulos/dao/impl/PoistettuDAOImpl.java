package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import static fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto.SiirtotiedostoConstants.SIIRTOTIEDOSTO_TIMEZONE;

import fi.vm.sade.valintalaskenta.domain.siirtotiedosto.Poistettu;
import fi.vm.sade.valintalaskenta.tulos.dao.PoistettuDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.util.PoistettuRowMapper;
import java.time.LocalDateTime;
import java.util.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.stereotype.Repository;

@Repository
public class PoistettuDAOImpl implements PoistettuDAO {
  private final NamedParameterJdbcTemplate jdbcTemplate;

  private PoistettuRowMapper rowMapper = new PoistettuRowMapper();

  @Autowired
  public PoistettuDAOImpl(final NamedParameterJdbcTemplate jdbcTemplate) {
    this.jdbcTemplate = jdbcTemplate;
  }

  @Override
  public List<Poistettu> findPoistetut(
      EntityType entityType, LocalDateTime startDateTime, LocalDateTime endDateTime) {
    Date start = Date.from(startDateTime.atZone(SIIRTOTIEDOSTO_TIMEZONE).toInstant());
    Date end = Date.from(endDateTime.atZone(SIIRTOTIEDOSTO_TIMEZONE).toInstant());
    return switch (entityType) {
      case VALINNANVAIHE -> doFindPoistetut(
          "valinnanvaihe", "valinnanvaihe_history", "null", "valinnanvaihe_oid", start, end);
      case VALINTATAPAJONO -> doFindPoistetut(
          "valintatapajono",
          "valintatapajono_history",
          "valinnanvaihe",
          "valintatapajono_oid",
          start,
          end);
      case JONOSIJA -> doFindPoistetut(
          "jonosija", "jonosija_history", "valintatapajono", "hakemus_oid", start, end);
      case VALINTAKOE_OSALLISTUMINEN -> doFindPoistetut(
          "valintakoe_osallistuminen",
          "valintakoe_osallistuminen_history",
          "null",
          "hakemus_oid",
          start,
          end);
      case HAKUTOIVE -> doFindPoistetut(
          "hakutoive",
          "hakutoive_history",
          "valintakoe_osallistuminen",
          "hakukohde_oid",
          start,
          end);
      case VALINTAKOE_VALINNANVAIHE -> doFindPoistetut(
          "valintakoe_valinnanvaihe",
          "valintakoe_valinnanvaihe_history",
          "hakutoive",
          "valinnanvaihe_oid",
          start,
          end);
      case VALINTAKOE -> doFindPoistetut(
          "valintakoe",
          "valintakoe_history",
          "valintakoe_valinnanvaihe",
          "valintakoe_oid",
          start,
          end);
    };
  }

  private List<Poistettu> doFindPoistetut(
      String tableName,
      String historyTableName,
      String parentIdFieldName,
      String tunnisteFieldName,
      Date start,
      Date end) {
    String sqlTemplate =
        """
            select distinct on (id) id, %s as parentId, %s from %s hist
              where upper(hist.system_time) is not null and
                upper(hist.system_time) >= :start and
                upper(hist.system_time) <  :end and
                not exists (select 1 from %s where id = hist.id)""";
    String tunnisteSelectPart =
        "tunniste".equals(tunnisteFieldName)
            ? "tunniste"
            : String.format("%s as tunniste", tunnisteFieldName);
    String sql =
        String.format(
            sqlTemplate, parentIdFieldName, tunnisteSelectPart, historyTableName, tableName);
    return jdbcTemplate.query(
        sql, new MapSqlParameterSource("start", start).addValue("end", end), rowMapper);
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
    return jdbcTemplate.query(sql, new MapSqlParameterSource("ids", ids), rowMapper).stream()
        .map(p -> p.setDeletedItself(false))
        .toList();
  }
}
