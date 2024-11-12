package fi.vm.sade.valintalaskenta.laskenta.dao.impl;

import com.google.common.collect.ComparisonChain;
import com.google.common.hash.HashCode;
import com.google.common.hash.Hashing;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.*;
import fi.vm.sade.valintalaskenta.laskenta.dao.SeurantaDao;
import java.nio.charset.Charset;
import java.sql.Array;
import java.time.Instant;
import java.util.*;
import java.util.function.BiFunction;
import java.util.stream.Collectors;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.jdbc.core.namedparam.SqlParameterSource;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionTemplate;

@Component
@Transactional
public class SeurantaDaoImpl implements SeurantaDao {
  private static final Logger LOG = LoggerFactory.getLogger(SeurantaDaoImpl.class);

  private final JdbcTemplate jdbcTemplate;
  private final TransactionTemplate transactionTemplate;

  @Autowired
  public SeurantaDaoImpl(JdbcTemplate jdbcTemplate, TransactionTemplate transactionTemplate) {
    this.jdbcTemplate = jdbcTemplate;
    this.transactionTemplate = transactionTemplate;
  }

  private RowMapper<Laskenta> getLaskentaRowMapper(Collection<UUID> laskentaUuids) {
    SqlParameterSource parameters = new MapSqlParameterSource("uuids", laskentaUuids);

    // haetaan ilmoitukset mappiin, avain laskennan uuid + hakuoid
    RowMapper<ImmutablePair<ImmutablePair<UUID, String>, IlmoitusDto>> ilmoitusRowmapper =
        (rs, rownum) -> {
          Array data = rs.getArray("data");
          return new ImmutablePair<>(
              new ImmutablePair<>(
                  UUID.fromString(rs.getString("laskenta_uuid")), rs.getString("hakukohdeoid")),
              new IlmoitusDto(
                  IlmoitusTyyppi.valueOf(rs.getString("ilmoitustyyppi")),
                  rs.getString("otsikko"),
                  data == null ? null : Arrays.stream(((String[]) data.getArray())).toList(),
                  rs.getTimestamp("luotu").getTime()));
        };
    Map<ImmutablePair<UUID, String>, List<IlmoitusDto>> ilmoitukset =
        new NamedParameterJdbcTemplate(this.jdbcTemplate)
                .query(
                    "SELECT * FROM seuranta_hakukohde_ilmoitukset WHERE laskenta_uuid IN (:uuids)",
                    parameters,
                    ilmoitusRowmapper)
                .stream()
                .collect(
                    Collectors.groupingBy(
                        ilmoitus -> ilmoitus.getKey(),
                        Collectors.mapping(ilmoitus -> ilmoitus.getValue(), Collectors.toList())));

    // haetaan hakukohteet mappiin, avain laskennan uuid
    RowMapper<ImmutablePair<UUID, HakukohdeDto>> hakukohdeRowmapper =
        (rs, rownum) -> {
          UUID laskentaUUID = UUID.fromString(rs.getString("laskenta_uuid"));
          return new ImmutablePair<>(
              laskentaUUID,
              new HakukohdeDto(
                  rs.getString("hakukohdeoid"),
                  rs.getString("organisaatiooid"),
                  HakukohdeTila.valueOf(rs.getString("tila")),
                  ilmoitukset.getOrDefault(
                      new ImmutablePair<>(laskentaUUID, rs.getString("hakukohdeoid")),
                      Collections.emptyList())));
        };
    Map<UUID, List<HakukohdeDto>> hakukohteet =
        new NamedParameterJdbcTemplate(this.jdbcTemplate)
                .query(
                    "SELECT * FROM seuranta_laskenta_hakukohteet WHERE laskenta_uuid IN (:uuids)",
                    parameters,
                    hakukohdeRowmapper)
                .stream()
                .collect(
                    Collectors.groupingBy(
                        hakukohde -> hakukohde.getKey(),
                        Collectors.mapping(
                            hakukohde -> hakukohde.getValue(), Collectors.toList())));

    // konstruoidaan laskenta
    return (rs, rowNum) -> {
      UUID uuid = UUID.fromString(rs.getString("uuid"));
      return new Laskenta(
          uuid,
          rs.getString("useroid"),
          rs.getString("haunnimi"),
          rs.getString("nimi"),
          rs.getString("hakuoid"),
          rs.getTimestamp("luotu"),
          LaskentaTyyppi.valueOf(rs.getString("tyyppi")),
          rs.getBoolean("erillishaku"),
          Optional.ofNullable(rs.getObject("valinnanvaihe", Integer.class)),
          rs.getObject("valintakoelaskenta", Boolean.class),
          LaskentaTila.valueOf(rs.getString("tila")),
          hakukohteet.getOrDefault(uuid, Collections.emptyList()),
          ilmoitukset
              .getOrDefault(new ImmutablePair<>(uuid, null), Collections.emptyList())
              .stream()
              .findFirst()
              .orElse(null));
    };
  }

  private final RowMapper<UUID> uuidRowMapper =
      (rs, rowNum) -> UUID.fromString(rs.getString("uuid"));

  private List<Laskenta> getLaskennat(Collection<UUID> uuids) {
    if (uuids.isEmpty()) {
      return Collections.emptyList();
    }
    SqlParameterSource parameters = new MapSqlParameterSource("uuids", uuids);
    return new NamedParameterJdbcTemplate(this.jdbcTemplate)
        .query(
            "SELECT * FROM seuranta_laskennat WHERE uuid IN (:uuids)",
            parameters,
            getLaskentaRowMapper(uuids));
  }

  @Override
  public void siivoa(Date asti) {
    try {
      int poistettu =
          this.jdbcTemplate.update(
              "DELETE FROM seuranta_laskennat WHERE luotu<=?::timestamptz",
              asti.toInstant().toString());
      LOG.info("Seurantakannasta poistettiin {} laskentaa!", poistettu);
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }

  @Override
  public Optional<LaskentaDto> haeLaskenta(String uuid) {
    Optional<Laskenta> laskenta =
        this.getLaskennat(Collections.singleton(UUID.fromString(uuid))).stream().findFirst();
    if (!laskenta.isPresent()) {
      LOG.error("Laskentaa ei ole olemassa uuid:lla {}", uuid);
      throw new RuntimeException("Laskentaa ei ole olemassa uuid:lla " + uuid);
    }
    return laskenta.map(l -> l.asDto(jonosijaProvider(), true));
  }

  @Override
  public Collection<YhteenvetoDto> haeYhteenvetoKaikilleLaskennoille(Instant luotuAlkaen) {
    Collection<UUID> uuids =
        this.jdbcTemplate.query(
            "SELECT uuid FROM seuranta_laskennat WHERE luotu>=?::timestamptz",
            uuidRowMapper,
            luotuAlkaen.toString());
    return this.getLaskennat(uuids).stream()
        .map(laskenta -> laskentaAsYhteenvetoDto(laskenta, jonosijaProvider()))
        .collect(Collectors.toList());
  }

  @Override
  public YhteenvetoDto haeYhteenveto(String uuid) {
    return this.transactionTemplate.execute(t -> this.getLaskennat(Collections.singleton(UUID.fromString(uuid))).stream()
        .map(laskenta -> laskentaAsYhteenvetoDto(laskenta, jonosijaProvider()))
        .findFirst()
        .orElse(null));
  }

  private YhteenvetoDto laskentaAsYhteenvetoDto(
      Laskenta laskenta, BiFunction<Date, LaskentaTila, Integer> jonosijaSupplier) {
    if (laskenta == null) {
      return null;
    }
    final String uuid = laskenta.getUuid().toString();
    final String userOID = laskenta.getUserOID();
    final String hakuOid = laskenta.getHakuOid();
    final Date luotu = laskenta.getLuotu();
    final LaskentaTila tila = laskenta.getTila();
    final int hakukohteitaYhteensa = laskenta.getHakukohteitaYhteensa();
    final int hakukohteitaKeskeytetty = laskenta.getHakukohteitaOhitettu();
    final int hakukohteitaValmiina = laskenta.getHakukohteitaValmiina();
    final LaskentaTyyppi tyyppi = laskenta.getTyyppi();
    final Optional<Integer> valinnanvaihe = laskenta.getValinnanvaihe();
    final Boolean valintakoelaskenta = laskenta.getValintakoelaskenta();
    long luotuTimestamp;
    if (luotu == null) {
      luotuTimestamp = new Date().getTime();
    } else {
      luotuTimestamp = luotu.getTime();
    }
    return new YhteenvetoDto(
        uuid,
        userOID,
        laskenta.getHaunnimi(),
        laskenta.getNimi(),
        hakuOid,
        luotuTimestamp,
        tila,
        hakukohteitaYhteensa,
        hakukohteitaValmiina,
        hakukohteitaKeskeytetty,
        jonosijaSupplier.apply(luotu, tila),
        tyyppi,
        valinnanvaihe,
        valintakoelaskenta);
  }

  @Override
  public LaskentaDto resetoiLaskenta(String uuid, boolean nollaaIlmoitukset) {
    return this.transactionTemplate.execute(t -> {
      Laskenta laskenta =
          this.getLaskennat(Collections.singleton(UUID.fromString(uuid))).stream()
              .findFirst()
              .orElseThrow(() -> new RuntimeException("Laskentaa ei ole olemassa uuid:lla " + uuid));

      Optional<Laskenta> onGoing = orGetOnGoing(laskenta);
      if (onGoing.isPresent()) {
        return onGoing.get().asDto(jonosijaProvider(), false);
      }
      return resetLaskenta(nollaaIlmoitukset, LaskentaTila.ALOITTAMATTA, laskenta);
    });
  }

  private LaskentaDto resetLaskenta(
      boolean nollaaIlmoitukset, LaskentaTila resetointiTila, Laskenta m) {
    this.jdbcTemplate.update(
        "UPDATE seuranta_laskenta_hakukohteet SET tila=? WHERE laskenta_uuid=? AND tila=?",
        HakukohdeTila.TEKEMATTA.toString(),
        m.getUuid(),
        HakukohdeTila.KESKEYTETTY.toString()); // ohitettu==keskeytetty
    this.jdbcTemplate.update(
        "UPDATE seuranta_laskennat SET tila=? WHERE uuid=?",
        resetointiTila.toString(),
        m.getUuid());

    if (nollaaIlmoitukset) {
      this.jdbcTemplate.update(
          "DELETE FROM seuranta_hakukohde_ilmoitukset WHERE laskenta_uuid=?", m.getUuid());
    }

    return this.getLaskennat(Collections.singleton(m.getUuid())).stream()
        .map(laskenta -> laskenta.asDto(jonosijaProvider(), true))
        .findFirst()
        .orElse(null);
  }

  private BiFunction<Date, LaskentaTila, Integer> jonosijaProvider() {
    return (luotu, tila) -> {
      if (LaskentaTila.ALOITTAMATTA.equals(tila)) {
        return this.jdbcTemplate.queryForObject(
            "SELECT COUNT(1) FROM seuranta_laskennat WHERE tila=? AND luotu<=?::timestamptz",
            Integer.class,
            LaskentaTila.ALOITTAMATTA.toString(),
            luotu.toInstant().toString());
      } else {
        return null;
      }
    };
  }

  @Override
  public YhteenvetoDto peruutaLaskenta(
      String uuid,
      Optional<IlmoitusDto> ilmoitusDtoOptional) {
    return this.transactionTemplate.execute(t -> {
      Laskenta l =
          this.getLaskennat(Collections.singleton(UUID.fromString(uuid))).stream()
              .findFirst()
              .orElseThrow(() -> new RuntimeException("Laskenta with uuid: " + uuid + " not found"));
      if(l.getTila()==LaskentaTila.VALMIS || l.getTila()==LaskentaTila.PERUUTETTU) {
        throw new RuntimeException("Yritettiin peruuttaa laskenta " + l.getUuid() + " vaikka laskenta on tilassa " + l.getTila());
      }

      this.jdbcTemplate.update(
          "UPDATE seuranta_laskenta_hakukohteet SET tila=? WHERE laskenta_uuid=?::uuid",
          HakukohdeTila.KESKEYTETTY.toString(),
          uuid);

      // päivitetään tieto koska laskenta on lopetettu
      this.jdbcTemplate.update(
          "UPDATE seuranta_laskennat SET tila=?, lopetettu=?::timestamptz WHERE uuid=?::uuid",
          LaskentaTila.PERUUTETTU.toString(),
          Instant.now().toString(),
          uuid);

      // päivitä ilmoitus
      if (ilmoitusDtoOptional.isPresent()) {
        this.paivitaIlmoitus(uuid, ilmoitusDtoOptional.get());
      }

      return this.haeYhteenveto(uuid);
    });
  }

  private YhteenvetoDto lisaaIlmoitus(String uuid, String hakukohdeOid, IlmoitusDto ilmoitus) {
    this.jdbcTemplate.update(
        "INSERT INTO seuranta_hakukohde_ilmoitukset (laskenta_uuid, hakukohdeoid, ilmoitustyyppi, otsikko, luotu, data) "
            + "VALUES (?::uuid, ?, ?, ?, ?::timestamptz, ?)",
        uuid,
        hakukohdeOid,
        ilmoitus.getTyyppi().toString(),
        ilmoitus.getOtsikko(),
        Instant.ofEpochMilli(ilmoitus.getPaivamaara()).toString(),
        ilmoitus.getData() == null ? null : ilmoitus.getData().toArray(new String[] {}));

    return this.getLaskennat(Collections.singleton(UUID.fromString(uuid))).stream()
        .map(laskenta -> laskentaAsYhteenvetoDto(laskenta, jonosijaProvider()))
        .findFirst()
        .orElse(null);
  }

  public TunnisteDto luoLaskenta(
      String userOID,
      String haunnimi,
      String nimi,
      String hakuOid,
      LaskentaTyyppi tyyppi,
      Boolean erillishaku,
      Optional<Integer> valinnanvaihe,
      Boolean valintakoelaskenta,
      Collection<HakukohdeDto> hakukohdeOids) {
    if (hakukohdeOids == null || hakukohdeOids.isEmpty()) {
      throw new RuntimeException(
          "Seurantaa ei muodosteta tyhjalle hakukohdejoukolle. Onko haulla hakukohteita tai rajaako hakukohdemaski kaikki hakukohteet pois? HakuOid = "
              + hakuOid);
    }
    Laskenta l =
        new Laskenta(
            UUID.randomUUID(),
            userOID,
            haunnimi,
            nimi,
            hakuOid,
            new Date(),
            tyyppi,
            erillishaku,
            valinnanvaihe,
            valintakoelaskenta,
            LaskentaTila.ALOITTAMATTA,
            hakukohdeOids,
            null);
    Optional<Laskenta> onGoing = orGetOnGoing(l);
    if (onGoing.isPresent()) {
      return new TunnisteDto(onGoing.get().getUuid().toString(), false);
    }

    return this.transactionTemplate.execute(t -> {
      this.jdbcTemplate.update(
          "INSERT INTO seuranta_laskennat "
              + "(uuid, haunnimi, nimi, hakuoid, luotu, tila, tyyppi, valinnanvaihe, valintakoelaskenta, erillishaku, useroid, identityhash) "
              + "VALUES (?, ?, ?, ?, ?::timestamptz, ?, ?, ?, ?, ?, ?, ?)",
          l.getUuid(),
          l.getHaunnimi(),
          l.getNimi(),
          l.getHakuOid(),
          Instant.now().toString(),
          LaskentaTila.ALOITTAMATTA.toString(),
          l.getTyyppi().toString(),
          l.getValinnanvaihe().orElse(null),
          l.getValintakoelaskenta(),
          l.getErillishaku(),
          l.getUserOID(),
          l.getIdentityHash());

      for (HakukohdeDto hakukohdeDto : hakukohdeOids) {
        this.jdbcTemplate.update(
            "INSERT INTO seuranta_laskenta_hakukohteet (laskenta_uuid, hakukohdeoid, organisaatiooid, tila, luotu) "
                + "VALUES (?, ?, ?, ?, now())",
            l.getUuid(),
            hakukohdeDto.getHakukohdeOid(),
            hakukohdeDto.getOrganisaatioOid(),
            HakukohdeTila.TEKEMATTA.toString());
      }

      return new TunnisteDto(l.getUuid().toString(), true);
    });
  }

  private Optional<Laskenta> orGetOnGoing(Laskenta l) {
    final String identityHash = l.getIdentityHash();
    Collection<UUID> uuids =
        this.jdbcTemplate.query(
            "SELECT uuid FROM seuranta_laskennat WHERE identityhash=? AND (tila=? OR tila=?)",
            uuidRowMapper,
            identityHash,
            LaskentaTila.ALOITTAMATTA.toString(),
            LaskentaTila.MENEILLAAN.toString());
    return uuids.isEmpty() ? Optional.empty() : this.getLaskennat(uuids).stream().findFirst();
  }

  private void paivitaIlmoitus(String uuid, IlmoitusDto ilmoitus) {
    this.jdbcTemplate.update(
        "INSERT INTO seuranta_hakukohde_ilmoitukset (laskenta_uuid, hakukohdeoid, ilmoitustyyppi, otsikko, luotu, data) "
            + "VALUES (?::uuid, null, ?, ?, ?::timestamptz, ?) "
            + "ON CONFLICT (laskenta_uuid) WHERE hakukohdeoid IS null "
            + "DO UPDATE SET ilmoitustyyppi=?, otsikko=?, luotu=?::timestamptz, data=?",
        uuid,
        ilmoitus.getTyyppi().toString(),
        ilmoitus.getOtsikko(),
        Instant.ofEpochMilli(ilmoitus.getPaivamaara()).toString(),
        ilmoitus.getData() == null ? null : ilmoitus.getData().toArray(new String[] {}),
        ilmoitus.getTyyppi().toString(),
        ilmoitus.getOtsikko(),
        Instant.ofEpochMilli(ilmoitus.getPaivamaara()).toString(),
        ilmoitus.getData() == null ? null : ilmoitus.getData().toArray(new String[] {}));
  }

  @Override
  public Optional<ImmutablePair<UUID, Collection<String>>> otaSeuraavatHakukohteetTyonAlle(String noodiId, int maxYhtaaikaisetHakukohteet) {
    return this.transactionTemplate.execute(t -> {
      // haetaan hakukohteiden määrä
      int noodillaAjossa = this.jdbcTemplate.queryForObject(
          "SELECT count(1) " +
              "FROM seuranta_laskenta_hakukohteet " +
              "WHERE tila=? " +
              "AND noodi_id=?",
          Integer.class, HakukohdeTila.KESKEN.toString(), noodiId);
      if(noodillaAjossa>=maxYhtaaikaisetHakukohteet) {
        return Optional.empty();
      }

      // haetaan aloitettava hakukohde
      Optional<ImmutablePair<UUID, String>> hakukohde =
          this.jdbcTemplate
              .query(
                  "SELECT laskenta_uuid, hakukohdeoid " +
                      "FROM seuranta_laskennat " +
                      "JOIN seuranta_laskenta_hakukohteet ON uuid=laskenta_uuid " +
                      "WHERE seuranta_laskenta_hakukohteet.tila=? " +
                      "AND seuranta_laskennat.tila<>? " +
                      "ORDER BY seuranta_laskennat.luotu ASC, hakukohdeoid ASC " +
                      "LIMIT 1 " +
                      "FOR UPDATE",
                  (rs, rowNum) -> new ImmutablePair<>(UUID.fromString(rs.getString("laskenta_uuid")), rs.getString("hakukohdeoid")),
                  HakukohdeTila.TEKEMATTA.toString(),
                  LaskentaTila.PERUUTETTU.toString())
              .stream()
              .findFirst();

      return hakukohde.map(hk -> {
        // jos hakukohde löytyy
        UUID uuid = hk.getLeft();
        Laskenta laskenta = this.getLaskennat(Collections.singleton(uuid)).iterator().next();
        String hakukohdeOid = hk.getRight();

        // merkataan laskenta aloitetuksi
        this.jdbcTemplate.update("UPDATE seuranta_laskennat SET tila=?, aloitettu=?::timestamptz WHERE uuid=? AND tila=?",
            LaskentaTila.MENEILLAAN.toString(), Instant.now().toString(),uuid, LaskentaTila.ALOITTAMATTA.toString());

        if(laskenta.getTyyppi()==LaskentaTyyppi.VALINTARYHMA) {
          // jos hakukohde osa valintaryhmälaskentaa, aloitetaan kaikki hakukohteet samalla
          Collection<String> hakukohdeOids = this.jdbcTemplate.query(
              "UPDATE seuranta_laskenta_hakukohteet SET tila=?, yritykset=yritykset+1, noodi_id=?, aloitettu=now() WHERE laskenta_uuid=? RETURNING hakukohdeoid",
              (rs, rownum) -> rs.getString("hakukohdeoid"), HakukohdeTila.KESKEN.toString(), noodiId, uuid);
          return new ImmutablePair<>(uuid, hakukohdeOids);
        } else {
          // muuten aloitetaan vain kyseinen hakukohde
          this.jdbcTemplate.update("UPDATE seuranta_laskenta_hakukohteet SET tila=?, yritykset=yritykset+1, noodi_id=?, aloitettu=now() WHERE laskenta_uuid=? AND hakukohdeoid=?",
              HakukohdeTila.KESKEN.toString(), noodiId, uuid, hakukohdeOid);

          return new ImmutablePair<>(uuid, Collections.singleton(hakukohdeOid));
        }
      });
    });
  }

  private void lukitseHakukohteet(UUID uuid) {
    // Lukitaan hakukohteet (aina samassa järjestyksessa ettei tule deadlockeja). Jos tätä ei tehdä voi käydä niin että
    // jos useampaa hakukohdetta merkitään käsitellyksi yhtä aikaa, kaikki näkevät toisensa olevan vielä kesken ja
    // laskentaa ei merkitä valmiiksi vaikka hakukohteita ei enää olisi laskematta.
    this.jdbcTemplate.query(
        "SELECT hakukohdeoid " +
            "FROM seuranta_laskenta_hakukohteet " +
            "WHERE laskenta_uuid=? " +
            "ORDER BY hakukohdeoid " +
            "FOR UPDATE", (rs, rowNum) -> null, uuid);
  }

  private void merkkaaLaskentaKasitellyksi(UUID uuid) {
    // merkitään laskenta valmiiksi jos kaikki hakukohteet joko laskettuja tai epäonnistuineita
    this.jdbcTemplate.update(
        "UPDATE seuranta_laskennat " +
            "SET tila=?, lopetettu=?::timestamptz " +
            "WHERE uuid=? " +
            "AND tila=? " +
            "AND NOT EXISTS " +
            "(SELECT 1 FROM seuranta_laskenta_hakukohteet WHERE laskenta_uuid=? AND (tila=? OR tila=?))",
        LaskentaTila.VALMIS.toString(), Instant.now().toString(), uuid, LaskentaTila.MENEILLAAN.toString(), uuid,
        HakukohdeTila.TEKEMATTA.toString(), HakukohdeTila.KESKEN.toString());
  }

  private Map<String, HakukohdeTila> haeHakukohteidenTilat(UUID uuid, Collection<String> hakukohdeOids) {
    NamedParameterJdbcTemplate namedParameterJdbcTemplate = new NamedParameterJdbcTemplate(jdbcTemplate);
    MapSqlParameterSource parameters = new MapSqlParameterSource();
    parameters.addValue("uuid", uuid);
    parameters.addValue("hakukohdeOids", hakukohdeOids);

    // varmistetaan että hakukohteet työn alla
    return namedParameterJdbcTemplate.query(
        "SELECT hakukohdeoid, tila " +
            "FROM seuranta_laskenta_hakukohteet " +
            "WHERE laskenta_uuid=:uuid AND hakukohdeoid IN (:hakukohdeOids)",
        parameters, (rs, rowNum) -> new ImmutablePair(rs.getString("hakukohdeoid"), rs.getString("tila")))
        .stream().collect(Collectors.toMap(v -> v.getLeft().toString(), v -> HakukohdeTila.valueOf(v.getRight().toString())));
  }

  @Override
  public void merkkaaHakukohteetValmiiksi(UUID uuid, Collection<String> hakukohdeOids) {
    this.transactionTemplate.executeWithoutResult(t -> {
      this.lukitseHakukohteet(uuid);

      Map<String, HakukohdeTila> tilat = this.haeHakukohteidenTilat(uuid, hakukohdeOids);
      Collection<String> eiOlemassa = hakukohdeOids.stream().filter(oid -> !tilat.containsKey(oid)).toList();
      Collection<String> eiTyonAlla = tilat.entrySet().stream()
          .filter(e -> e.getValue()!=HakukohdeTila.KESKEN).map(e -> e.getKey()).toList();

      if(!eiOlemassa.isEmpty() || !eiTyonAlla.isEmpty()) {
        StringBuilder msg = new StringBuilder();
        if(!eiOlemassa.isEmpty()) {
          msg.append("Yritettiin merkita seuraavia hakukohteita valmiiksi vaikka niitä ei ole olemassa: "
              + eiOlemassa.stream().collect(Collectors.joining(",")));
        }
        if(!eiTyonAlla.isEmpty()) {
          msg.append("Yritettiin merkita seuraavia hakukohteita valmiiksi vaikka ne eivät ole työn alla: "
              + eiTyonAlla.stream().collect(Collectors.joining(",")));
        }
        throw new RuntimeException(msg.toString());
      }

      NamedParameterJdbcTemplate namedParameterJdbcTemplate = new NamedParameterJdbcTemplate(jdbcTemplate);
      MapSqlParameterSource parameters = new MapSqlParameterSource();
      parameters.addValue("uuid", uuid);
      parameters.addValue("hakukohdeOids", hakukohdeOids);

      // merkitään valmiiksi lasketut hakukohteet
      namedParameterJdbcTemplate.update(
          "UPDATE seuranta_laskenta_hakukohteet " +
              "SET tila='" + HakukohdeTila.VALMIS + "', lopetettu=now() " +
              "WHERE laskenta_uuid=:uuid " +
              "AND hakukohdeoid IN (:hakukohdeOids) ",
          parameters);

      this.merkkaaLaskentaKasitellyksi(uuid);
    });
  }

  @Override
  public void merkkaaHakukohteetEpaonnistuneeksi(UUID uuid, Collection<String> hakukohdeOids, int maxYritykset, String message) {
    this.transactionTemplate.executeWithoutResult(t -> {
      this.lukitseHakukohteet(uuid);

      Map<String, HakukohdeTila> tilat = this.haeHakukohteidenTilat(uuid, hakukohdeOids);
      Collection<String> eiOlemassa = hakukohdeOids.stream().filter(oid -> !tilat.containsKey(oid)).toList();
      Collection<String> eiTyonAlla = tilat.entrySet().stream()
          .filter(e -> e.getValue()!=HakukohdeTila.KESKEN).map(e -> e.getKey()).toList();

      if(!eiOlemassa.isEmpty() || !eiTyonAlla.isEmpty()) {
        StringBuilder msg = new StringBuilder();
        if(!eiOlemassa.isEmpty()) {
          msg.append("Yritettiin merkita seuraavia hakukohteita epäonnistuneiksi vaikka niitä ei ole olemassa: "
              + eiOlemassa.stream().collect(Collectors.joining(",")));
        }
        if(!eiTyonAlla.isEmpty()) {
          msg.append("Yritettiin merkita seuraavia hakukohteita epäonnistuneiksi vaikka ne eivät ole työn alla: "
              + eiTyonAlla.stream().collect(Collectors.joining(",")));
        }
        throw new RuntimeException(msg.toString());
      }

      NamedParameterJdbcTemplate namedParameterJdbcTemplate = new NamedParameterJdbcTemplate(jdbcTemplate);
      MapSqlParameterSource parameters = new MapSqlParameterSource();
      parameters.addValue("uuid", uuid);
      parameters.addValue("hakukohdeOids", hakukohdeOids);
      parameters.addValue("maxYritykset", maxYritykset);

      // merkitään keskeytetyiksi hakukohteet joita on jo yritetty uudestaan
      namedParameterJdbcTemplate.update(
          "UPDATE seuranta_laskenta_hakukohteet " +
              "SET tila='" + HakukohdeTila.KESKEYTETTY + "', lopetettu=now() " +
              "WHERE laskenta_uuid=:uuid " +
              "AND hakukohdeoid IN (:hakukohdeOids) " +
              "AND yritykset>=:maxYritykset",
          parameters);

      // merkitään tekemättömiksi hakukohteet joita ei ole yritetty uudestaan
      namedParameterJdbcTemplate.update(
          "UPDATE seuranta_laskenta_hakukohteet " +
              "SET tila='" + HakukohdeTila.TEKEMATTA + "' " +
              "WHERE laskenta_uuid=:uuid " +
              "AND hakukohdeoid IN (:hakukohdeOids) " +
              "AND yritykset<:maxYritykset",
          parameters);

      this.lisaaIlmoitus(uuid.toString(), hakukohdeOids.size()==1 ? hakukohdeOids.iterator().next() : null, IlmoitusDto.virheilmoitus(message));
      this.merkkaaLaskentaKasitellyksi(uuid);
    });
  }

  @Override
  public void merkkaaNoodiLiveksi(String noodiId) {
    this.jdbcTemplate.update("INSERT INTO noodit (noodi_id, alive) VALUES(?, now()) ON CONFLICT (noodi_id) DO UPDATE SET alive=now()", noodiId);
  }

  @Override
  public void resetoiKuolleidenNoodienLaskennat(int viive) {
    this.jdbcTemplate.update(
        "UPDATE seuranta_laskenta_hakukohteet " +
            "SET tila=?, yritykset=0, noodi_id=null " +
            "FROM noodit " +
            "WHERE seuranta_laskenta_hakukohteet.noodi_id=noodit.noodi_id " +
            "AND tila=? " +
            "AND noodit.alive<?::timestamptz",
        HakukohdeTila.TEKEMATTA.toString(), HakukohdeTila.KESKEN.toString(), Instant.now().minusSeconds(viive).toString());
  }

  private static class Laskenta {
    private static final Logger LOG = LoggerFactory.getLogger(Laskenta.class);

    private UUID uuid;
    private final String haunnimi;
    private final String nimi;
    private final String hakuOid;
    private final Date luotu;
    private final LaskentaTila tila;
    private final LaskentaTyyppi tyyppi;
    private final Collection<HakukohdeDto> hakukohteet;
    private final IlmoitusDto ilmoitus;
    private final Optional<Integer> valinnanvaihe;
    private final Boolean valintakoelaskenta;
    private final Boolean erillishaku;
    private final String userOID;
    private final String identityHash;

    public Laskenta(
        UUID uuid,
        String userOID,
        String haunnimi,
        String nimi,
        String hakuOid,
        Date luotu,
        LaskentaTyyppi tyyppi,
        Boolean erillishaku,
        Optional<Integer> valinnanvaihe,
        Boolean valintakoelaskenta,
        LaskentaTila laskentaTila,
        Collection<HakukohdeDto> hakukohdeOids,
        IlmoitusDto ilmoitus) {
      this.haunnimi = haunnimi;
      this.nimi = nimi;
      this.uuid = uuid;
      this.userOID = userOID;
      this.hakuOid = hakuOid;
      this.luotu = luotu;
      this.tila = laskentaTila;
      this.ilmoitus = ilmoitus;
      this.hakukohteet = hakukohdeOids;
      this.tyyppi = tyyppi;
      this.erillishaku = erillishaku;
      this.valinnanvaihe = valinnanvaihe;
      this.valintakoelaskenta = valintakoelaskenta;
      this.identityHash = createIdentityHash().toString();
    }

    public String getIdentityHash() {
      return identityHash;
    }

    public String getHaunnimi() {
      return haunnimi;
    }

    public String getNimi() {
      return nimi;
    }

    private HashCode createIdentityHash() {
      final long DELIMETER = 1000000000L;
      return Hashing.md5()
          .newHasher()
          .putString(hakuOid, Charset.forName("UTF-8"))
          .putLong(DELIMETER + 1L)
          .putInt(tyyppi != null ? tyyppi.ordinal() : -1)
          .putLong(DELIMETER + 2L)
          .putInt(this.hakukohteet.size())
          .putLong(DELIMETER + 3L)
          .putInt(valinnanvaihe.orElse(-1))
          .putLong(DELIMETER + 4L)
          .putBoolean(Boolean.TRUE.equals(valintakoelaskenta))
          .putLong(DELIMETER + 5L)
          .putBoolean(Boolean.TRUE.equals(erillishaku))
          .putLong(DELIMETER + 6L)
          .putObject(
              this.hakukohteet,
              (oids, sink) -> {
                Optional.ofNullable(oids).orElse(Collections.emptyList()).stream()
                    .sorted(
                        (h1, h2) ->
                            ComparisonChain.start()
                                .compare(h1.getHakukohdeOid(), h2.getHakukohdeOid())
                                .compare(h1.getOrganisaatioOid(), h2.getOrganisaatioOid())
                                .result())
                    .forEach(
                        h -> {
                          sink.putString(h.getHakukohdeOid(), Charset.forName("UTF-8"))
                              .putLong(DELIMETER + 7L)
                              .putString(h.getOrganisaatioOid(), Charset.forName("UTF-8"))
                              .putLong(DELIMETER + 8L);
                        });
              })
          .hash();
    }

    public List<String> getOhitettu() {
      return this.hakukohteet.stream()
          .filter(hk -> HakukohdeTila.KESKEYTETTY.equals(hk.getTila()))
          .map(hk -> hk.getHakukohdeOid())
          .collect(Collectors.toList());
    }

    public List<String> getValmiit() {
      return this.hakukohteet.stream()
          .filter(hk -> HakukohdeTila.VALMIS.equals(hk.getTila()))
          .map(hk -> hk.getHakukohdeOid())
          .collect(Collectors.toList());
    }

    public List<String> getTekematta() {
      return this.hakukohteet.stream()
          .filter(hk -> HakukohdeTila.TEKEMATTA.equals(hk.getTila()))
          .map(hk -> hk.getHakukohdeOid())
          .collect(Collectors.toList());
    }

    public LaskentaTyyppi getTyyppi() {
      return tyyppi;
    }

    public Boolean getErillishaku() {
      return erillishaku;
    }

    public int getHakukohteitaOhitettu() {
      return (int)
          this.hakukohteet.stream()
              .filter(hk -> HakukohdeTila.KESKEYTETTY.equals(hk.getTila()))
              .count();
    }

    public int getHakukohteitaTekematta() {
      return (int)
          this.hakukohteet.stream()
              .filter(hk -> HakukohdeTila.TEKEMATTA.equals(hk.getTila()))
              .count();
    }

    public int getHakukohteitaValmiina() {
      return (int)
          this.hakukohteet.stream()
              .filter(hk -> HakukohdeTila.VALMIS.equals(hk.getTila()))
              .count();
    }

    public int getHakukohteitaYhteensa() {
      return this.hakukohteet.size();
    }

    public IlmoitusDto getIlmoitus() {
      return ilmoitus;
    }

    public String getHakuOid() {
      return hakuOid;
    }

    public Date getLuotu() {
      return luotu;
    }

    public LaskentaTila getTila() {
      return tila;
    }

    public UUID getUuid() {
      return uuid;
    }

    public Optional<Integer> getValinnanvaihe() {
      return valinnanvaihe;
    }

    public Boolean getValintakoelaskenta() {
      return valintakoelaskenta;
    }

    public LaskentaDto asDto(
        BiFunction<Date, LaskentaTila, Integer> jonosijaProvider, boolean luotiinkoUusiLaskenta) {
      try {
        return new LaskentaDto(
            getUuid().toString(),
            userOID,
            haunnimi,
            nimi,
            getHakuOid(),
            luotu == null ? new Date().getTime() : luotu.getTime(),
            getTila(),
            getTyyppi(),
            Optional.ofNullable(ilmoitus).orElse(null),
            this.hakukohteet.stream().collect(Collectors.toList()),
            erillishaku,
            valinnanvaihe,
            valintakoelaskenta,
            jonosijaProvider.apply(luotu, getTila()),
            luotiinkoUusiLaskenta);
      } catch (Exception e) {
        LOG.error("LaskentaDto:n muodostus Laskentaentiteetista epaonnistui!", e);
        throw e;
      }
    }

    public String getUserOID() {
      return userOID;
    }
  }

  @Override
  public String lueParametri(String nimi) {
    return this.jdbcTemplate.queryForObject("SELECT arvo FROM parametrit WHERE nimi=?", String.class, nimi);
  }
}
