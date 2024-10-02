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

@Component
@Transactional
public class SeurantaDaoImpl implements SeurantaDao {
  private static final Logger LOG = LoggerFactory.getLogger(SeurantaDaoImpl.class);

  private final JdbcTemplate jdbcTemplate;

  @Autowired
  public SeurantaDaoImpl(JdbcTemplate jdbcTemplate) {
    this.jdbcTemplate = jdbcTemplate;
    resetoiMeneillaanOlevatLaskennat();
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
          rs.getObject("valinnanvaihe", Integer.class),
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
  public LaskentaDto haeLaskenta(String uuid) {
    Optional<Laskenta> laskenta =
        this.getLaskennat(Collections.singleton(UUID.fromString(uuid))).stream().findFirst();
    if (!laskenta.isPresent()) {
      LOG.error("Laskentaa ei ole olemassa uuid:lla {}", uuid);
      throw new RuntimeException("Laskentaa ei ole olemassa uuid:lla " + uuid);
    }
    return laskenta.get().asDto(jonosijaProvider(), true);
  }

  @Override
  public Collection<YhteenvetoDto> haeKaynnissaOlevienYhteenvedotHaulle(String hakuOid) {
    Collection<UUID> uuids =
        this.jdbcTemplate.query(
            "SELECT uuid FROM seuranta_laskennat WHERE hakuoid=? AND tila=?",
            uuidRowMapper,
            hakuOid,
            LaskentaTila.MENEILLAAN.toString());
    return this.getLaskennat(uuids).stream()
        .map(laskenta -> laskentaAsYhteenvetoDto(laskenta, jonosijaProvider()))
        .collect(Collectors.toList());
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
  public Collection<YhteenvetoDto> haeYhteenvedotHaulle(String hakuOid) {
    Collection<UUID> uuids =
        this.jdbcTemplate.query(
            "SELECT uuid FROM seuranta_laskennat WHERE hakuoid=?", uuidRowMapper, hakuOid);
    return this.getLaskennat(uuids).stream()
        .map(laskenta -> laskentaAsYhteenvetoDto(laskenta, jonosijaProvider()))
        .collect(Collectors.toList());
  }

  @Override
  public Collection<YhteenvetoDto> haeYhteenvedotHaulle(String hakuOid, LaskentaTyyppi tyyppi) {
    if (tyyppi == null) {
      LOG.error("Laskentatyyppi null kutsussa hakea yhteenvedot tietylle laskentatyypille haussa.");
      throw new RuntimeException(
          "Laskentatyyppi null kutsussa hakea yhteenvedot tietylle laskentatyypille haussa.");
    }
    Collection<UUID> uuids =
        this.jdbcTemplate.query(
            "SELECT uuid FROM seuranta_laskennat WHERE hakuoid=? AND tyyppi=?",
            uuidRowMapper,
            hakuOid,
            tyyppi.toString());
    return this.getLaskennat(uuids).stream()
        .map(laskenta -> laskentaAsYhteenvetoDto(laskenta, jonosijaProvider()))
        .collect(Collectors.toList());
  }

  @Override
  public YhteenvetoDto haeYhteenveto(String uuid) {
    return this.getLaskennat(Collections.singleton(UUID.fromString(uuid))).stream()
        .map(laskenta -> laskentaAsYhteenvetoDto(laskenta, jonosijaProvider()))
        .findFirst()
        .orElse(null);
  }

  public Collection<YhteenvetoDto> haeYhteenvedotAlkamattomille(Collection<String> uuids) {
    SqlParameterSource parameters =
        new MapSqlParameterSource(
            Map.of(
                "uuids",
                    uuids.stream().map(uuid -> UUID.fromString(uuid)).collect(Collectors.toList()),
                "tila", LaskentaTila.ALOITTAMATTA.toString()));
    Collection<UUID> aloittamattaUUIDS =
        new NamedParameterJdbcTemplate(this.jdbcTemplate)
            .query(
                "SELECT uuid FROM seuranta_laskennat WHERE uuid IN (:uuids) AND tila=:tila",
                parameters,
                uuidRowMapper);

    return this.getLaskennat(aloittamattaUUIDS).stream()
        .map(laskenta -> laskentaAsYhteenvetoDto(laskenta, jonosijaProvider()))
        .collect(Collectors.toList());
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
    final int hakukohteitaTekematta = laskenta.getHakukohteitaTekematta();
    final int hakukohteitaValmiina =
        (hakukohteitaYhteensa - hakukohteitaKeskeytetty) - hakukohteitaTekematta;
    final LaskentaTyyppi tyyppi = laskenta.getTyyppi();
    final Integer valinnanvaihe = laskenta.getValinnanvaihe();
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
  public void poistaLaskenta(String uuid) {
    this.jdbcTemplate.update("DELETE FROM seuranta_laskennat WHERE uuid=?", uuid);
  }

  @Override
  public LaskentaDto resetoiEiValmiitHakukohteet(String uuid, boolean nollaaIlmoitukset) {
    Laskenta laskenta =
        this.getLaskennat(Collections.singleton(UUID.fromString(uuid))).stream()
            .findFirst()
            .orElseThrow(() -> new RuntimeException("Laskentaa ei ole olemassa uuid:lla " + uuid));

    Optional<Laskenta> onGoing = orGetOnGoing(laskenta);
    if (onGoing.isPresent()) {
      return onGoing.get().asDto(jonosijaProvider(), false);
    }
    return resetLaskenta(nollaaIlmoitukset, LaskentaTila.ALOITTAMATTA, laskenta);
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
          "DELETE FROM seuranta_hakukohteet_ilmoitukset WHERE laskenta_uuid=?", m.getUuid());
    }

    return this.getLaskennat(Collections.singleton(m.getUuid())).stream()
        .map(laskenta -> laskenta.asDto(jonosijaProvider(), true))
        .findFirst()
        .orElse(null);
  }

  @Override
  public YhteenvetoDto merkkaaTila(
      String uuid, LaskentaTila tila, Optional<IlmoitusDto> ilmoitusDtoOptional) {
    this.jdbcTemplate.update(
        "UPDATE seuranta_laskennat SET tila=? WHERE uuid=?::uuid AND tila=?",
        tila.toString(),
        uuid,
        LaskentaTila.MENEILLAAN.toString());

    // päivitä ilmoitus
    if (ilmoitusDtoOptional.isPresent()) {
      this.paivitaIlmoitus(uuid, ilmoitusDtoOptional.get());
    }

    // päivitetään tieto koska laskenta on lopetettu
    if (tila != LaskentaTila.MENEILLAAN) {
      this.jdbcTemplate.update(
          "UPDATE seuranta_laskennat SET lopetettu=?::timestamptz WHERE uuid=?::uuid",
          Instant.now().toString(),
          uuid);
    }

    Optional<Laskenta> laskenta =
        this.getLaskennat(Collections.singleton(UUID.fromString(uuid))).stream().findFirst();
    return laskenta.map(l -> laskentaAsYhteenvetoDto(l, jonosijaProvider())).orElse(null);
  }

  @Override
  public void resetoiMeneillaanOlevatLaskennat() {
    this.jdbcTemplate.update(
        "UPDATE seuranta_laskennat SET tila=? WHERE tila=?",
        LaskentaTila.PERUUTETTU.toString(),
        LaskentaTila.MENEILLAAN.toString());
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
  public YhteenvetoDto merkkaaTila(
      String uuid,
      LaskentaTila tila,
      HakukohdeTila hakukohdeTila,
      Optional<IlmoitusDto> ilmoitusDtoOptional) {
    Laskenta l =
        this.getLaskennat(Collections.singleton(UUID.fromString(uuid))).stream()
            .findFirst()
            .orElseThrow(() -> new RuntimeException("Laskenta with uuid: " + uuid + " not found"));

    this.jdbcTemplate.update(
        "UPDATE seuranta_laskenta_hakukohteet SET tila=? WHERE laskenta_uuid=?::uuid",
        hakukohdeTila.toString(),
        uuid);

    return this.merkkaaTila(uuid, tila, ilmoitusDtoOptional);
  }

  @Override
  public YhteenvetoDto lisaaIlmoitus(String uuid, String hakukohdeOid, IlmoitusDto ilmoitus) {
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

  @Override
  public YhteenvetoDto merkkaaTila(
      String uuid, String hakukohdeOid, HakukohdeTila tila, IlmoitusDto ilmoitus) {
    if (HakukohdeTila.TEKEMATTA.equals(tila)) {
      throw new RuntimeException("Tekematta tilaa ei saa asettaa manuaalisesti");
    }
    this.jdbcTemplate.update(
        "UPDATE seuranta_laskenta_hakukohteet SET tila=? WHERE laskenta_uuid=?::uuid AND hakukohdeoid=?",
        tila.toString(),
        uuid,
        hakukohdeOid);
    this.lisaaIlmoitus(uuid, hakukohdeOid, ilmoitus);
    return this.getLaskennat(Collections.singleton(UUID.fromString(uuid))).stream()
        .map(laskenta -> laskentaAsYhteenvetoDto(laskenta, jonosijaProvider()))
        .findFirst()
        .orElse(null);
  }

  @Override
  public YhteenvetoDto merkkaaTila(String uuid, String hakukohdeOid, HakukohdeTila tila) {
    if (HakukohdeTila.TEKEMATTA.equals(tila)) {
      throw new RuntimeException("Tekematta tilaa ei saa asettaa manuaalisesti");
    }
    this.jdbcTemplate.update(
        "UPDATE seuranta_laskenta_hakukohteet SET tila=? WHERE laskenta_uuid=?::uuid AND hakukohdeoid=?",
        tila.toString(),
        uuid,
        hakukohdeOid);
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
      Integer valinnanvaihe,
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
        l.getValinnanvaihe(),
        l.getValintakoelaskenta(),
        l.getErillishaku(),
        l.getUserOID(),
        l.getIdentityHash());

    for (HakukohdeDto hakukohdeDto : hakukohdeOids) {
      this.jdbcTemplate.update(
          "INSERT INTO seuranta_laskenta_hakukohteet (laskenta_uuid, hakukohdeoid, organisaatiooid, tila) "
              + "VALUES (?, ?, ?, ?)",
          l.getUuid(),
          hakukohdeDto.getHakukohdeOid(),
          hakukohdeDto.getOrganisaatioOid(),
          HakukohdeTila.TEKEMATTA.toString());
    }

    return new TunnisteDto(l.getUuid().toString(), true);
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
  public String otaSeuraavaLaskentaTyonAlle() {
    Optional<UUID> uuid =
        this.jdbcTemplate
            .query(
                "UPDATE seuranta_laskennat SET tila = ? "
                    + "WHERE uuid = (SELECT uuid FROM seuranta_laskennat WHERE tila=? ORDER BY luotu ASC LIMIT 1 FOR UPDATE) "
                    + "RETURNING uuid",
                uuidRowMapper,
                LaskentaTila.MENEILLAAN.toString(),
                LaskentaTila.ALOITTAMATTA.toString())
            .stream()
            .findFirst();

    if (uuid.isPresent()) {
      // päivitetään tieto koska laskenta on aloitettu
      this.jdbcTemplate.update(
          "UPDATE seuranta_laskennat SET aloitettu=?::timestamptz WHERE uuid=?::uuid",
          Instant.now().toString(),
          uuid.get());
    }

    return uuid.map(id -> id.toString()).orElse(null);
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
    private final Integer valinnanvaihe;
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
        Integer valinnanvaihe,
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
          .putInt(valinnanvaihe != null ? valinnanvaihe : -1)
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

    public Integer getValinnanvaihe() {
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
}
