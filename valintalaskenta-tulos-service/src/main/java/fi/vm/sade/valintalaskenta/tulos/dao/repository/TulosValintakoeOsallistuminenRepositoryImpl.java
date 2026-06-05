package fi.vm.sade.valintalaskenta.tulos.dao.repository;

import fi.vm.sade.service.valintaperusteet.dto.model.Koekutsu;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Hakutoive;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Valintakoe;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeValinnanvaihe;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import org.springframework.jdbc.core.ResultSetExtractor;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcOperations;

public class TulosValintakoeOsallistuminenRepositoryImpl
    implements TulosValintakoeOsallistuminenRepositoryCustom {

  private static final String SELECT_COLUMNS =
      """
      vo.id AS vo_id, vo.haku_oid AS vo_haku_oid, vo.hakemus_oid AS vo_hakemus_oid,
      vo.hakija_oid AS vo_hakija_oid, vo.created_at AS vo_created_at,
      h.id AS h_id, h.hakukohde_oid AS h_hakukohde_oid,
      vvv.id AS vvv_id, vvv.valinnanvaihe_oid AS vvv_valinnanvaihe_oid,
      vvv.valinnan_vaihe_jarjestysluku AS vvv_jarjestys, vvv.last_modified AS vvv_last_modified,
      vk.id AS vk_id, vk.valintakoe_oid AS vk_valintakoe_oid,
      vk.valintakoe_tunniste AS vk_valintakoe_tunniste, vk.nimi AS vk_nimi,
      vk.aktiivinen AS vk_aktiivinen, vk.osallistuminen AS vk_osallistuminen,
      vk.lahetetaanko_koekutsut AS vk_lahetetaanko, vk.kutsuttavien_maara AS vk_kutsuttavien,
      vk.kutsun_kohde AS vk_kutsun_kohde, vk.kutsun_kohde_avain AS vk_kutsun_kohde_avain,
      vk.kuvaus_fi AS vk_kuvaus_fi, vk.kuvaus_en AS vk_kuvaus_en, vk.kuvaus_sv AS vk_kuvaus_sv,
      vk.laskenta_tila AS vk_laskenta_tila, vk.laskenta_tulos AS vk_laskenta_tulos,
      vk.tekninen_kuvaus AS vk_tekninen_kuvaus, vk.last_modified AS vk_last_modified
      """;

  private static final String JOINS =
      """
      FROM valintakoe_osallistuminen vo
      LEFT JOIN hakutoive h ON h.valintakoe_osallistuminen = vo.id
      LEFT JOIN valintakoe_valinnanvaihe vvv ON vvv.hakutoive = h.id
      LEFT JOIN valintakoe vk ON vk.valintakoe_valinnanvaihe = vvv.id
      """;

  private static final String ORDER_BY = " ORDER BY vo.id, h.id, vvv.id, vk.id";

  private final NamedParameterJdbcOperations jdbc;

  public TulosValintakoeOsallistuminenRepositoryImpl(NamedParameterJdbcOperations jdbc) {
    this.jdbc = jdbc;
  }

  @Override
  public List<ValintakoeOsallistuminen> findDistinctValintakoeOsallistuminensByHakutoive(
      String hakukohdeOid) {
    String sql =
        "SELECT "
            + SELECT_COLUMNS
            + JOINS
            + "WHERE vo.id IN ("
            + "  SELECT DISTINCT h2.valintakoe_osallistuminen FROM hakutoive h2"
            + "  WHERE h2.hakukohde_oid = :hakukohdeOid"
            + "    AND h2.valintakoe_osallistuminen IS NOT NULL)"
            + ORDER_BY;
    return jdbc.query(
        sql, new MapSqlParameterSource("hakukohdeOid", hakukohdeOid), new Extractor());
  }

  @Override
  public List<ValintakoeOsallistuminen> findDistinctValintakoeOsallistuminensByHakutoiveet(
      List<String> hakukohdeOids) {
    if (hakukohdeOids.isEmpty()) {
      return List.of();
    }
    String sql =
        "SELECT "
            + SELECT_COLUMNS
            + JOINS
            + "WHERE vo.id IN ("
            + "  SELECT DISTINCT h2.valintakoe_osallistuminen FROM hakutoive h2"
            + "  WHERE h2.hakukohde_oid IN (:hakukohdeOids)"
            + "    AND h2.valintakoe_osallistuminen IS NOT NULL)"
            + ORDER_BY;
    return jdbc.query(
        sql, new MapSqlParameterSource("hakukohdeOids", hakukohdeOids), new Extractor());
  }

  @Override
  public List<ValintakoeOsallistuminen> findDistinctByHakuAndOsallistuminen(
      String hakuOid, Osallistuminen osallistuminen) {
    String sql =
        "SELECT "
            + SELECT_COLUMNS
            + JOINS
            + "WHERE vo.id IN ("
            + "  SELECT DISTINCT vo2.id FROM valintakoe_osallistuminen vo2"
            + "  JOIN hakutoive h2 ON h2.valintakoe_osallistuminen = vo2.id"
            + "  JOIN valintakoe_valinnanvaihe vvv2 ON vvv2.hakutoive = h2.id"
            + "  JOIN valintakoe vk2 ON vk2.valintakoe_valinnanvaihe = vvv2.id"
            + "  WHERE vo2.haku_oid = :hakuOid AND vk2.osallistuminen = :osallistuminen)"
            + ORDER_BY;
    MapSqlParameterSource params =
        new MapSqlParameterSource()
            .addValue("hakuOid", hakuOid)
            .addValue("osallistuminen", osallistuminen.name());
    return jdbc.query(sql, params, new Extractor());
  }

  private static final class Extractor
      implements ResultSetExtractor<List<ValintakoeOsallistuminen>> {

    @Override
    public List<ValintakoeOsallistuminen> extractData(ResultSet rs) throws SQLException {
      Map<UUID, ValintakoeOsallistuminen> osallistumiset = new LinkedHashMap<>();
      Map<UUID, Hakutoive> hakutoiveet = new HashMap<>();
      Map<UUID, ValintakoeValinnanvaihe> vaiheet = new HashMap<>();

      while (rs.next()) {
        UUID voId = rs.getObject("vo_id", UUID.class);
        ValintakoeOsallistuminen vo =
            osallistumiset.computeIfAbsent(voId, id -> readOsallistuminen(rs, id));

        UUID hId = rs.getObject("h_id", UUID.class);
        if (hId == null) {
          continue;
        }
        Hakutoive ht =
            hakutoiveet.computeIfAbsent(
                hId,
                id -> {
                  Hakutoive newHt = readHakutoive(rs, id);
                  vo.getHakutoiveet().add(newHt);
                  return newHt;
                });

        UUID vvvId = rs.getObject("vvv_id", UUID.class);
        if (vvvId == null) {
          continue;
        }
        ValintakoeValinnanvaihe vvv =
            vaiheet.computeIfAbsent(
                vvvId,
                id -> {
                  ValintakoeValinnanvaihe newVvv = readValinnanvaihe(rs, id);
                  ht.getValintakoeValinnanvaiheet().add(newVvv);
                  return newVvv;
                });

        UUID vkId = rs.getObject("vk_id", UUID.class);
        if (vkId == null) {
          continue;
        }
        vvv.getValintakokeet().add(readValintakoe(rs, vkId));
      }
      return new ArrayList<>(osallistumiset.values());
    }

    private static ValintakoeOsallistuminen readOsallistuminen(ResultSet rs, UUID id) {
      try {
        ValintakoeOsallistuminen vo = new ValintakoeOsallistuminen();
        vo.setId(id);
        vo.setHakuOid(rs.getString("vo_haku_oid"));
        vo.setHakemusOid(rs.getString("vo_hakemus_oid"));
        vo.setHakijaOid(rs.getString("vo_hakija_oid"));
        Date createdAt = toDate(rs.getTimestamp("vo_created_at"));
        if (createdAt != null) {
          vo.setCreatedAt(createdAt);
        }
        return vo;
      } catch (SQLException e) {
        throw new RuntimeException(e);
      }
    }

    private static Hakutoive readHakutoive(ResultSet rs, UUID id) {
      try {
        Hakutoive ht = new Hakutoive();
        ht.setId(id);
        ht.setHakukohdeOid(rs.getString("h_hakukohde_oid"));
        return ht;
      } catch (SQLException e) {
        throw new RuntimeException(e);
      }
    }

    private static ValintakoeValinnanvaihe readValinnanvaihe(ResultSet rs, UUID id) {
      try {
        ValintakoeValinnanvaihe vvv = new ValintakoeValinnanvaihe();
        vvv.setId(id);
        vvv.setValinnanvaiheOid(rs.getString("vvv_valinnanvaihe_oid"));
        Integer jarjestys = rs.getObject("vvv_jarjestys", Integer.class);
        vvv.setValinnanVaiheJarjestysluku(jarjestys);
        vvv.setLastModified(toDate(rs.getTimestamp("vvv_last_modified")));
        return vvv;
      } catch (SQLException e) {
        throw new RuntimeException(e);
      }
    }

    private static Valintakoe readValintakoe(ResultSet rs, UUID id) throws SQLException {
      Valintakoe vk = new Valintakoe();
      vk.setId(id);
      vk.setValintakoeOid(rs.getString("vk_valintakoe_oid"));
      vk.setValintakoeTunniste(rs.getString("vk_valintakoe_tunniste"));
      vk.setNimi(rs.getString("vk_nimi"));
      vk.setAktiivinen(rs.getBoolean("vk_aktiivinen"));
      String osallistuminen = rs.getString("vk_osallistuminen");
      if (osallistuminen != null) {
        vk.setOsallistuminen(Osallistuminen.valueOf(osallistuminen));
      }
      vk.setLahetetaankoKoekutsut(rs.getBoolean("vk_lahetetaanko"));
      vk.setKutsuttavienMaara(rs.getObject("vk_kutsuttavien", Integer.class));
      String kutsunKohde = rs.getString("vk_kutsun_kohde");
      if (kutsunKohde != null) {
        vk.setKutsunKohde(Koekutsu.valueOf(kutsunKohde));
      }
      vk.setKutsunKohdeAvain(rs.getString("vk_kutsun_kohde_avain"));
      vk.setKuvausFI(rs.getString("vk_kuvaus_fi"));
      vk.setKuvausEN(rs.getString("vk_kuvaus_en"));
      vk.setKuvausSV(rs.getString("vk_kuvaus_sv"));
      vk.setLaskentaTila(rs.getString("vk_laskenta_tila"));
      vk.setLaskentaTulos(rs.getObject("vk_laskenta_tulos", Boolean.class));
      vk.setTekninenKuvaus(rs.getString("vk_tekninen_kuvaus"));
      vk.setLastModified(toDate(rs.getTimestamp("vk_last_modified")));
      return vk;
    }

    private static Date toDate(Timestamp ts) {
      return ts == null ? null : new Date(ts.getTime());
    }
  }
}
