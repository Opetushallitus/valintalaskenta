package fi.vm.sade.valintalaskenta.laskenta.resource;

import fi.vm.sade.valintalaskenta.domain.dto.seuranta.*;
import fi.vm.sade.valintalaskenta.laskenta.dao.SeurantaDao;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import java.time.Instant;
import java.util.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

@RestController
@PreAuthorize(
    "hasAnyRole('ROLE_APP_VALINTOJENTOTEUTTAMINEN_CRUD', 'ROLE_APP_VALINTOJENTOTEUTTAMINEN_READ_UPDATE')")
@RequestMapping(value = "/resources/seuranta")
@Tag(name = "/resources/seuranta", description = "Resurssi laskennan seurantaan")
public class LaskentaSeurantaResource {

  private static final Logger LOG = LoggerFactory.getLogger(LaskentaSeurantaResource.class);

  private final SeurantaDao seurantaDao;

  public LaskentaSeurantaResource(SeurantaDao seurantaDao) {
    this.seurantaDao = seurantaDao;
  }

  /** Yhteenvedot olemassa olevista laskennoista haulle */
  @GetMapping(value = "/hae/{hakuOid}", produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Hakee yhteenvedot haun laskennoille oid:n perusteella")
  ResponseEntity<Collection<YhteenvetoDto>> hae(@PathVariable("hakuOid") String hakuOid) {
    return ResponseEntity.status(HttpStatus.OK).body(seurantaDao.haeYhteenvedotHaulle(hakuOid));
  }

  /** Yhteenvedot olemassa olevista tietyn tyyppisista laskennoista haulle */
  @GetMapping(value = "/hae/{hakuOid}/tyyppi/{tyyppi}", produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Hakee yhteenvedot haun laskennoille oid:n ja tyypin perusteella")
  ResponseEntity<Collection<YhteenvetoDto>> haeLaskennanYhteenvedotHaulleTyypilla(
      @PathVariable("hakuOid") String hakuOid, @PathVariable("tyyppi") LaskentaTyyppi tyyppi) {
    return ResponseEntity.status(HttpStatus.OK)
        .body(seurantaDao.haeYhteenvedotHaulle(hakuOid, tyyppi));
  }

  @GetMapping(value = "/hae/{hakuOid}/kaynnissa", produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Hakee yhteenvedot käynnissäolevista laskennoista haulle")
  ResponseEntity<Collection<YhteenvetoDto>> haeKaynnissaOlevienLaskentojenYhteenvedot(
      @PathVariable("hakuOid") String hakuOid) {
    return ResponseEntity.status(HttpStatus.OK)
        .body(seurantaDao.haeKaynnissaOlevienYhteenvedotHaulle(hakuOid));
  }

  /** Yhteenvedot olemassa olevista laskennoista */
  @GetMapping(
      value = "/yhteenvetokaikillelaskennoille",
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Hakee yhteenvedot kaikista laskennoista")
  ResponseEntity<Collection<YhteenvetoDto>> haeYhteenvetoKaikilleLaskennoille() {
    return ResponseEntity.status(HttpStatus.OK)
        .body(
            seurantaDao.haeYhteenvetoKaikilleLaskennoille(
                Instant.now().minusSeconds(60 * 60 * 24)));
  }

  @GetMapping(
      value = "/laskenta/otaSeuraavaLaskentaTyonAlle",
      produces = MediaType.TEXT_PLAIN_VALUE)
  @Operation(summary = "Aloittaa seuraavan laskennan")
  ResponseEntity<String> otaSeuraavaLaskentaTyonAlle() {
    Optional<String> uuid = seurantaDao.otaSeuraavaLaskentaTyonAlle();
    LOG.info("Ota seuraava tyon alle: " + (uuid.isPresent() ? uuid.get() : "Ei tyota"));
    if (uuid.isPresent()) {
      final String u = uuid.get();
      return ResponseEntity.status(HttpStatus.OK).body(u);
    } else {
      return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
    }
  }

  @GetMapping(value = "/laskenta/{uuid}", produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Palauttaa laskennan oid:n perusteella")
  ResponseEntity<LaskentaDto> laskenta(@PathVariable("uuid") String uuid) {
    try {
      Optional<LaskentaDto> l = seurantaDao.haeLaskenta(uuid);
      if (l.isEmpty()) {
        throw new RuntimeException("SeurantaDao palautti null olion uuid:lle " + uuid);
      }
      return ResponseEntity.status(HttpStatus.OK).body(l.get());
    } catch (Exception e) {
      throw e;
    }
  }

  @GetMapping(value = "/kuormantasaus/laskenta/{uuid}", produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Palauttaa laskennan oid:n perusteella")
  ResponseEntity<LaskentaDto> kuormantasausLaskenta(@PathVariable("uuid") String uuid) {
    try {
      Optional<LaskentaDto> l = seurantaDao.haeLaskenta(uuid);
      if (l.isEmpty()) {
        throw new RuntimeException("SeurantaDao palautti null olion uuid:lle " + uuid);
      }
      return ResponseEntity.status(HttpStatus.OK).body(l.get());
    } catch (Exception e) {
      throw e;
    }
  }

  @GetMapping(value = "/lataa/{uuid}", produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Palauttaa laskennan tiedostona uuid:n perusteella")
  ResponseEntity<LaskentaDto> lataa(@PathVariable("uuid") String uuid) {
    Optional<LaskentaDto> laskenta = seurantaDao.haeLaskenta(uuid);
    return ResponseEntity.status(HttpStatus.OK)
        .header(
            "Content-Disposition", "attachment; filename=laskenta_" + laskenta.get().getUuid() + ".json")
        .body(laskenta.get());
  }

  @GetMapping(value = "/yhteenveto/{uuid}", produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Palauttaa laskennan yhteenvedon uuid:n perusteella")
  ResponseEntity<YhteenvetoDto> yhteenveto(@PathVariable("uuid") String uuid) {
    return ResponseEntity.status(HttpStatus.OK).body(seurantaDao.haeYhteenveto(uuid));
  }

  @PutMapping(
      value = "/kuormantasaus/laskenta/{uuid}/hakukohde/{hakukohdeOid}/tila/{tila}",
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Paivittaa yksittaisen hakukohteen tilaa laskennassa")
  ResponseEntity<YhteenvetoDto> merkkaaHakukohteenTila(
      @PathVariable("uuid") String uuid,
      @PathVariable("hakukohdeOid") String hakukohdeOid,
      @PathVariable("tila") HakukohdeTila tila) {
    try {
      YhteenvetoDto y = seurantaDao.merkkaaTila(uuid, hakukohdeOid, tila);
      if (y == null) {
        LOG.error(
            "Seurantaan markattiin hakukohteen {} tila {} laskentaan {} mutta ei saatu yhteenvetoa lisayksesta!",
            hakukohdeOid,
            tila,
            uuid);
      }
      return ResponseEntity.status(HttpStatus.OK).body(y);
    } catch (Exception e) {
      LOG.error("Tilan merkkauksessa tapahtui poikkeus. Kayttoliittymaa ei ehka paivitetty", e);
      throw e;
    }
  }

  @PostMapping(
      value = "/kuormantasaus/laskenta/{uuid}/hakukohde/{hakukohdeOid}/tila/{tila}",
      consumes = MediaType.APPLICATION_JSON_VALUE,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Paivittaa yksittaisen hakukohteen tilaa laskennassa ja jattaa ilmoituksen")
  ResponseEntity<YhteenvetoDto> merkkaaHakukohteenTila(
      @PathVariable("uuid") String uuid,
      @PathVariable("hakukohdeOid") String hakukohdeOid,
      @PathVariable("tila") HakukohdeTila tila,
      @RequestBody IlmoitusDto ilmoitus) {
    try {
      YhteenvetoDto y = seurantaDao.merkkaaTila(uuid, hakukohdeOid, tila, ilmoitus);
      if (y == null) {
        LOG.error(
            "Seurantaan markattiin hakukohteen {} tila {} laskentaan {} mutta ei saatu yhteenvetoa lisayksesta!",
            hakukohdeOid,
            tila,
            uuid);
      }
      return ResponseEntity.status(HttpStatus.OK).body(y);
    } catch (Exception e) {
      LOG.error("Tilan merkkauksessa tapahtui poikkeus. Kayttoliittymaa ei ehka paivitetty", e);
      throw e;
    }
  }

  @PostMapping(
      value = "/kuormantasaus/laskenta/{uuid}/hakukohde/{hakukohdeOid}",
      consumes = MediaType.APPLICATION_JSON_VALUE,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Lisää ilmoituksen hakukohteelle")
  ResponseEntity<YhteenvetoDto> lisaaIlmoitusHakukohteelle(
      @PathVariable("uuid") String uuid,
      @PathVariable("hakukohdeOid") String hakukohdeOid,
      @RequestBody IlmoitusDto ilmoitus) {
    YhteenvetoDto y = seurantaDao.lisaaIlmoitus(uuid, hakukohdeOid, ilmoitus);
    if (y == null) {
      LOG.error(
          "Seurantaan lisattiin ilmoitus laskentaan {} hakukohteelle {} mutta ei saatu yhteenvetoa lisayksesta!",
          uuid,
          hakukohdeOid);
    }
    return ResponseEntity.status(HttpStatus.OK).body(y);
  }

  @PutMapping(
      value = "/kuormantasaus/laskenta/{uuid}/resetoi",
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Resetoi keskeneräisten hakukohteiden laskentojen tilat")
  ResponseEntity<LaskentaDto> resetoiTilat(@PathVariable("uuid") String uuid) {
    try {
      LaskentaDto ldto = seurantaDao.resetoiEiValmiitHakukohteet(uuid, true);
      if (ldto == null) {
        LOG.error("Laskennan {} tila resetoitiin mutta ei saatu yhteenvetoa resetoinnista!", uuid);
      }
      return ResponseEntity.status(HttpStatus.OK).body(ldto);
    } catch (Exception e) {
      LOG.error("Seurantapalvelu epaonnistui resetoimaan laskennan uuid=" + uuid, e);
      throw e;
    }
  }

  @PutMapping(
      value = "/kuormantasaus/laskenta/{uuid}/tila/{tila}",
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Paivittaa laskennan tilan")
  ResponseEntity<YhteenvetoDto> merkkaaLaskennanTila(
      @PathVariable("uuid") String uuid, @PathVariable("tila") LaskentaTila tila) {
    YhteenvetoDto y = seurantaDao.merkkaaTila(uuid, tila, Optional.empty());
    if (y == null) {
      LOG.error(
          "Seurantaan paivitettiin laskennan {} tila {} mutta ei saatu yhteenvetoa lisayksesta!",
          uuid,
          tila);
    }
    return ResponseEntity.status(HttpStatus.OK).body(y);
  }

  @PostMapping(
      value = "/kuormantasaus/laskenta/{uuid}/tila/{tila}",
      consumes = MediaType.APPLICATION_JSON_VALUE,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Paivittaa laskennan tilan")
  ResponseEntity<YhteenvetoDto> merkkaaLaskennanTila(
      @PathVariable("uuid") String uuid,
      @PathVariable("tila") LaskentaTila tila,
      @RequestBody IlmoitusDto ilmoitus) {
    YhteenvetoDto y = seurantaDao.merkkaaTila(uuid, tila, Optional.ofNullable(ilmoitus));
    if (y == null) {
      LOG.error(
          "Seurantaan paivitettiin laskennan {} tila {} mutta ei saatu yhteenvetoa lisayksesta!",
          uuid,
          tila);
    }
    return ResponseEntity.status(HttpStatus.OK).body(y);
  }

  @PutMapping(
      value = "/kuormantasaus/laskenta/{uuid}/tila/{tila}/hakukohde/{hakukohteentila}",
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Paivittaa laskennan ja hakukohteiden tilan")
  ResponseEntity<YhteenvetoDto> merkkaaLaskennanJaHakukohteidenTila(
      @PathVariable("uuid") String uuid,
      @PathVariable("tila") LaskentaTila tila,
      @PathVariable("hakukohteentila") HakukohdeTila hakukohteentila) {
    YhteenvetoDto y = seurantaDao.merkkaaTila(uuid, tila, hakukohteentila, Optional.empty());
    if (y == null) {
      LOG.error(
          "Seurantaan paivitettiin laskennan {} tila {} mutta ei saatu yhteenvetoa lisayksesta!",
          uuid,
          tila);
    }
    return ResponseEntity.status(HttpStatus.OK).body(y);
  }

  @PostMapping(
      value = "/kuormantasaus/laskenta/{uuid}/tila/{tila}/hakukohde/{hakukohteentila}",
      consumes = MediaType.APPLICATION_JSON_VALUE,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Paivittaa laskennan ja hakukohteiden tilan ja jättää ilmoituksen")
  ResponseEntity<YhteenvetoDto> merkkaaLaskennanJaHakukohteidenTila(
      @PathVariable("uuid") String uuid,
      @PathVariable("tila") LaskentaTila tila,
      @PathVariable("hakukohteentila") HakukohdeTila hakukohteentila,
      @RequestBody IlmoitusDto ilmoitus) {
    YhteenvetoDto y =
        seurantaDao.merkkaaTila(uuid, tila, hakukohteentila, Optional.ofNullable(ilmoitus));
    if (y == null) {
      LOG.error(
          "Seurantaan paivitettiin laskennan {} tila {} mutta ei saatu yhteenvetoa lisayksesta!",
          uuid,
          tila);
    }
    return ResponseEntity.status(HttpStatus.OK).body(y);
  }

  @PostMapping(
      value = "/kuormantasaus/laskenta/{hakuOid}/tyyppi/{tyyppi}",
      consumes = MediaType.APPLICATION_JSON_VALUE,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Luo uuden laskennan")
  ResponseEntity<TunnisteDto> luoLaskenta(
      @PathVariable("hakuOid") String hakuOid,
      @PathVariable("tyyppi") LaskentaTyyppi tyyppi,
      @RequestParam("userOID") String userOID,
      @RequestParam("haunnimi") String haunnimi,
      @RequestParam(name = "nimi", required = false) String nimi,
      @RequestParam("erillishaku") Boolean erillishaku,
      @RequestParam(name = "valinnanvaihe", required = false) Integer valinnanvaihe,
      @RequestParam(name = "valintakoelaskenta", required = false) Boolean valintakoelaskenta,
      @RequestBody List<HakukohdeDto> hakukohdeOids) {
    if (hakukohdeOids == null) {
      throw new NullPointerException(
          "Laskentaa ei luoda tyhjalle (null) hakukohdedto referenssille!");
    }
    if (hakukohdeOids.isEmpty()) {
      throw new NullPointerException(
          "Laskentaa ei luoda tyhjalle (koko on nolla) hakukohdedto joukolle!");
    }
    hakukohdeOids.forEach(
        hk -> {
          if (hk.getHakukohdeOid() == null || hk.getOrganisaatioOid() == null) {
            throw new NullPointerException(
                "Laskentaa ei luoda hakukohdejoukkoobjektille koska joukossa oli null referensseja sisaltava hakukohde!");
          }
        });
    return ResponseEntity.status(HttpStatus.OK)
        .body(
            seurantaDao.luoLaskenta(
                userOID,
                haunnimi,
                nimi,
                hakuOid,
                tyyppi,
                erillishaku,
                valinnanvaihe,
                valintakoelaskenta,
                hakukohdeOids));
  }

  @DeleteMapping(
      value = "/kuormantasaus/laskenta/{uuid}",
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Poistaa laskennan")
  ResponseEntity<String> poistaLaskenta(@PathVariable("uuid") String uuid) {
    seurantaDao.poistaLaskenta(uuid);
    return ResponseEntity.status(HttpStatus.OK).build();
  }
}
