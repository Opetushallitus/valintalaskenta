package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import fi.vm.sade.service.valintaperusteet.dto.model.Koekutsu;
import fi.vm.sade.valintalaskenta.domain.valintakoe.*;
import fi.vm.sade.valintalaskenta.tulos.dao.TulosValintakoeOsallistuminenDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.repository.TulosValintakoeOsallistuminenRepository;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.*;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcOperations;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class TulosValintakoeOsallistuminenDAOImpl implements TulosValintakoeOsallistuminenDAO {

  private final TulosValintakoeOsallistuminenRepository repo;
  private final NamedParameterJdbcOperations jdbc;

  public TulosValintakoeOsallistuminenDAOImpl(
      TulosValintakoeOsallistuminenRepository repo, NamedParameterJdbcOperations jdbc) {
    this.repo = repo;
    this.jdbc = jdbc;
  }

  @Override
  public ValintakoeOsallistuminen findByHakemusOid(String hakemusOid) {
    return repo.findValintakoeOsallistuminenByHakemusOid(hakemusOid).orElse(null);
  }

  @Override
  public List<ValintakoeOsallistuminen> findByHakutoive(String hakukohdeOid) {
    return repo.findDistinctValintakoeOsallistuminensByHakutoive(hakukohdeOid);
  }

  @Override
  public List<ValintakoeOsallistuminen> findByHakutoiveet(List<String> hakukohdeOids) {
    return repo.findDistinctValintakoeOsallistuminensByHakutoiveet(hakukohdeOids);
  }

  @Override
  public List<ValintakoeOsallistuminen> findByHakijaOids(List<String> hakijaOids) {
    return repo.findDistinctValintakoeOsallistuminensByHakijaOidIn(hakijaOids);
  }

  @Override
  public List<ValintakoeOsallistuminen> findByHakuAndOsallistuminen(
      String hakuOid, Osallistuminen osallistuminen) {
    return repo.findDistinctByHakuAndOsallistuminen(hakuOid, osallistuminen);
  }

  @Override
  public List<String> readNewOrModifiedHakemusOids(
      LocalDateTime startDatetime, LocalDateTime endDatatime) {
    return startDatetime != null
        ? repo.findHakemusOidsByTimeRange(startDatetime, endDatatime)
        : repo.findHakemusOidsByEndTime(endDatatime);
  }

  @Override
  @Transactional(readOnly = true)
  public List<ValintakoeOsallistuminen> findByHakutoiveetBatched(List<String> hakukohdeOids) {
    if (hakukohdeOids == null || hakukohdeOids.isEmpty()) {
      return List.of();
    }

    // Query 1: distinct ValintakoeOsallistuminen matching any of the hakukohde OIDs.
    // LinkedHashMap deduplicates VOs that appear in multiple chunks.
    Map<UUID, ValintakoeOsallistuminen> voById = new LinkedHashMap<>();
    for (List<String> chunk : partition(hakukohdeOids, 1000)) {
      jdbc.query(
          "SELECT DISTINCT vo.* FROM valintakoe_osallistuminen vo"
              + " JOIN hakutoive h ON h.valintakoe_osallistuminen = vo.id"
              + " WHERE h.hakukohde_oid IN (:oids)",
          Map.of("oids", chunk),
          rs -> {
            voById.putIfAbsent((UUID) rs.getObject("id"), mapVo(rs));
          });
    }

    if (voById.isEmpty()) return List.of();

    // Query 2: all Hakutoive belonging to the loaded VOs
    Map<UUID, Hakutoive> hakutoiveById = new HashMap<>();
    for (List<UUID> chunk : partition(new ArrayList<>(voById.keySet()), 1000)) {
      jdbc.query(
          "SELECT * FROM hakutoive WHERE valintakoe_osallistuminen IN (:voIds)",
          Map.of("voIds", chunk),
          rs -> {
            Hakutoive h = mapHakutoive(rs);
            UUID parentId = (UUID) rs.getObject("valintakoe_osallistuminen");
            ValintakoeOsallistuminen parent = voById.get(parentId);
            if (parent != null) parent.getHakutoiveet().add(h);
            hakutoiveById.put(h.getId(), h);
          });
    }

    if (hakutoiveById.isEmpty()) return new ArrayList<>(voById.values());

    // Query 3: all ValintakoeValinnanvaihe belonging to the loaded Hakutoiveet
    Map<UUID, ValintakoeValinnanvaihe> vvvById = new HashMap<>();
    for (List<UUID> chunk : partition(new ArrayList<>(hakutoiveById.keySet()), 1000)) {
      jdbc.query(
          "SELECT * FROM valintakoe_valinnanvaihe WHERE hakutoive IN (:hIds)",
          Map.of("hIds", chunk),
          rs -> {
            ValintakoeValinnanvaihe vvv = mapVvv(rs);
            UUID parentId = (UUID) rs.getObject("hakutoive");
            Hakutoive parent = hakutoiveById.get(parentId);
            if (parent != null) parent.getValintakoeValinnanvaiheet().add(vvv);
            vvvById.put(vvv.getId(), vvv);
          });
    }

    if (vvvById.isEmpty()) return new ArrayList<>(voById.values());

    // Query 4: all Valintakoe belonging to the loaded ValintakoeValinnanvaiheet
    for (List<UUID> chunk : partition(new ArrayList<>(vvvById.keySet()), 1000)) {
      jdbc.query(
          "SELECT * FROM valintakoe WHERE valintakoe_valinnanvaihe IN (:vvvIds)",
          Map.of("vvvIds", chunk),
          rs -> {
            Valintakoe vk = mapVk(rs);
            UUID parentId = (UUID) rs.getObject("valintakoe_valinnanvaihe");
            ValintakoeValinnanvaihe parent = vvvById.get(parentId);
            if (parent != null) parent.getValintakokeet().add(vk);
          });
    }

    return new ArrayList<>(voById.values());
  }

  private static <T> List<List<T>> partition(List<T> list, int size) {
    List<List<T>> result = new ArrayList<>();
    for (int i = 0; i < list.size(); i += size) {
      result.add(list.subList(i, Math.min(i + size, list.size())));
    }
    return result;
  }

  private static ValintakoeOsallistuminen mapVo(ResultSet rs) throws SQLException {
    ValintakoeOsallistuminen vo = new ValintakoeOsallistuminen();
    vo.setId((UUID) rs.getObject("id"));
    vo.setHakuOid(rs.getString("haku_oid"));
    vo.setHakemusOid(rs.getString("hakemus_oid"));
    vo.setHakijaOid(rs.getString("hakija_oid"));
    Timestamp createdAt = rs.getTimestamp("created_at");
    if (createdAt != null) vo.setCreatedAt(new Date(createdAt.getTime()));
    return vo;
  }

  private static Hakutoive mapHakutoive(ResultSet rs) throws SQLException {
    Hakutoive h = new Hakutoive();
    h.setId((UUID) rs.getObject("id"));
    h.setHakukohdeOid(rs.getString("hakukohde_oid"));
    return h;
  }

  private static ValintakoeValinnanvaihe mapVvv(ResultSet rs) throws SQLException {
    ValintakoeValinnanvaihe vvv = new ValintakoeValinnanvaihe();
    vvv.setId((UUID) rs.getObject("id"));
    vvv.setValinnanvaiheOid(rs.getString("valinnanvaihe_oid"));
    vvv.setValinnanVaiheJarjestysluku(rs.getObject("valinnan_vaihe_jarjestysluku", Integer.class));
    Timestamp lastModified = rs.getTimestamp("last_modified");
    if (lastModified != null) vvv.setLastModified(new Date(lastModified.getTime()));
    return vvv;
  }

  private static Valintakoe mapVk(ResultSet rs) throws SQLException {
    Valintakoe vk = new Valintakoe();
    vk.setId((UUID) rs.getObject("id"));
    vk.setValintakoeOid(rs.getString("valintakoe_oid"));
    vk.setValintakoeTunniste(rs.getString("valintakoe_tunniste"));
    vk.setNimi(rs.getString("nimi"));
    vk.setAktiivinen(rs.getBoolean("aktiivinen"));
    String osallistuminen = rs.getString("osallistuminen");
    if (osallistuminen != null) vk.setOsallistuminen(Osallistuminen.valueOf(osallistuminen));
    vk.setLahetetaankoKoekutsut(rs.getBoolean("lahetetaanko_koekutsut"));
    vk.setKutsuttavienMaara(rs.getObject("kutsuttavien_maara", Integer.class));
    String kutsunKohde = rs.getString("kutsun_kohde");
    if (kutsunKohde != null) vk.setKutsunKohde(Koekutsu.valueOf(kutsunKohde));
    vk.setKutsunKohdeAvain(rs.getString("kutsun_kohde_avain"));
    vk.setKuvausFI(rs.getString("kuvaus_fi"));
    vk.setKuvausSV(rs.getString("kuvaus_sv"));
    vk.setKuvausEN(rs.getString("kuvaus_en"));
    vk.setLaskentaTila(rs.getString("laskenta_tila"));
    vk.setLaskentaTulos(rs.getObject("laskenta_tulos", Boolean.class));
    vk.setTekninenKuvaus(rs.getString("tekninen_kuvaus"));
    Timestamp lastModified = rs.getTimestamp("last_modified");
    if (lastModified != null) vk.setLastModified(new Date(lastModified.getTime()));
    return vk;
  }
}
