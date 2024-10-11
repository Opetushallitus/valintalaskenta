package fi.vm.sade.valintalaskenta.laskenta.dao;

import static org.junit.Assert.*;

import fi.vm.sade.valintalaskenta.domain.dto.seuranta.*;
import fi.vm.sade.valintalaskenta.testing.AbstractIntegrationTest;

import java.util.*;
import java.util.stream.Collectors;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * @author Santeri Korri
 */
public class SeurantaDaoTest extends AbstractIntegrationTest {
  private static final Logger LOG = LoggerFactory.getLogger(SeurantaDaoTest.class);
  @Autowired private SeurantaDao seurantaDao;
  private static final Random rnd = new Random(System.currentTimeMillis());

  private static String randomHakukohde() {
    return "HAKUKOHDE_" + Math.abs(rnd.nextInt(100000000));
  }

  @BeforeEach
  public void siivoaSeurannat() {
    seurantaDao.siivoa(new Date());
  }

  @Test
  public void testLuoLaskentaRoundTrip() {
    // luodaan laskenta
    TunnisteDto tunnisteDto = this.seurantaDao.luoLaskenta("userOid", "haku1", "haun laskenta", "hakuOid", LaskentaTyyppi.HAKU,
       false, Optional.empty(), false,
        Collections.singletonList(new HakukohdeDto("hakukohdeOid", "organisaatioOid")));

    // saadaan sama laskenta ulos
    LaskentaDto laskentaDto = this.seurantaDao.haeLaskenta(tunnisteDto.getUuid()).get();
    Assertions.assertEquals(tunnisteDto.getUuid(), laskentaDto.getUuid());
    Assertions.assertEquals("userOid", laskentaDto.getUserOID());
    Assertions.assertEquals("haku1", laskentaDto.getHaunnimi());
    Assertions.assertEquals("haun laskenta", laskentaDto.getNimi());
    Assertions.assertEquals("hakuOid", laskentaDto.getHakuOid());
    Assertions.assertEquals(LaskentaTila.ALOITTAMATTA, laskentaDto.getTila());
    Assertions.assertEquals(LaskentaTyyppi.HAKU, laskentaDto.getTyyppi());
    Assertions.assertEquals(null, laskentaDto.getIlmoitus());
    Assertions.assertEquals(1, laskentaDto.getHakukohteet().size());
    Assertions.assertEquals("hakukohdeOid", laskentaDto.getHakukohteet().iterator().next().getHakukohdeOid());
    Assertions.assertEquals("organisaatioOid", laskentaDto.getHakukohteet().iterator().next().getOrganisaatioOid());
    Assertions.assertEquals(false, laskentaDto.getErillishaku());
    Assertions.assertEquals(null, laskentaDto.getValinnanvaihe());
    Assertions.assertEquals(false, laskentaDto.getValintakoelaskenta());
    Assertions.assertEquals(1, laskentaDto.getJonosija());
    Assertions.assertEquals(true, laskentaDto.getLuotiinkoUusiLaskenta());
  }

  private LaskentaDto luoLaskenta(String nimi, Collection<String> hakukohdeOids) {
    return this.seurantaDao.haeLaskenta(this.seurantaDao.luoLaskenta("userOid", "haku1", nimi, "hakuOid", LaskentaTyyppi.HAKU,
        false, Optional.empty(), false,
        hakukohdeOids.stream().map(oid -> new HakukohdeDto(oid, "organisaatioOid")).toList()).getUuid()).get();
  }

  @Test
  public void testOtaSeuraavaHakukohdeTyonAlleEiLaskentoja() {
    // jos laskentoja ei ole luotu, niitä ei voi ajaa
    Assertions.assertEquals(Optional.empty(), this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10));
  }

  @Test
  public void testOtaSeuraavaHakukohdeTyonAllePerusFlow() {
    // luodaan laskenta
    LaskentaDto laskentaDto = this.luoLaskenta("laskenta1", List.of("123"));

    ImmutablePair<UUID, Collection<String>> hakukohteet = this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10).get();

    // hakukohteet tiedot vastaavat luodun laskennan tietoja
    Assertions.assertEquals(hakukohteet.getLeft().toString(), laskentaDto.getUuid());
    Collection<String> laskennanHakukohteet = laskentaDto.getHakukohteet().stream().map(hk -> hk.getHakukohdeOid()).collect(Collectors.toSet());
    String laskettavaHakukohde = hakukohteet.getRight().iterator().next();
    Assertions.assertTrue(laskennanHakukohteet.contains(laskettavaHakukohde));

    // laskenta on siirretty MENEILLAAN-tilaan
    Assertions.assertEquals(LaskentaTila.MENEILLAAN, this.seurantaDao.haeLaskenta(laskentaDto.getUuid()).get().getTila());
  }

  @Test
  public void testSamanLaskennanHakukohteitaTehdaanOidJarjestyksessa() {
    this.luoLaskenta("laskenta1", List.of("123", "234"));

    // saadaan saman laskennan hakukohteita oid-järjestyksessä
    Assertions.assertEquals("123", this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10).get().getRight().iterator().next());
    Assertions.assertEquals("234", this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10).get().getRight().iterator().next());
  }

  @Test
  public void testEriLaskentojenHakukohteitaTehdaanLuontiJarjestyksessa() {
    // luodaan laskennat
    LaskentaDto laskentaDto1 = this.luoLaskenta("laskenta1", Collections.singleton("234"));
    LaskentaDto laskentaDto2 = this.luoLaskenta("laskenta2", Collections.singleton("123"));

    // saadaan eri laskentojen hakukohteita laskennan luontijärjestyksessä
    Assertions.assertEquals(laskentaDto1.getUuid(), this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10).get().getLeft().toString());
    Assertions.assertEquals(laskentaDto2.getUuid(), this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10).get().getLeft().toString());
  }

  @Test
  public void testMerkkaaHakukohdeValmiiksi() {
    LaskentaDto laskentaDto = this.luoLaskenta("laskenta", List.of("123", "234", "345"));
    YhteenvetoDto yhteenvetoDto = this.seurantaDao.haeYhteenveto(laskentaDto.getUuid());
    Assertions.assertEquals(0, yhteenvetoDto.getHakukohteitaValmiina());

    this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10);
    this.seurantaDao.merkkaaHakukohteetValmiiksi(UUID.fromString(laskentaDto.getUuid()), Collections.singleton("123"));

    yhteenvetoDto = this.seurantaDao.haeYhteenveto(laskentaDto.getUuid());
    Assertions.assertEquals(1, yhteenvetoDto.getHakukohteitaValmiina());
  }

  @Test
  public void testOlemantontaHakukohdettaEiVoiMerkataValmiiksi() {
    LaskentaDto laskentaDto = this.luoLaskenta("laskenta", List.of("123", "234", "345"));
    try {
      this.seurantaDao.merkkaaHakukohteetValmiiksi(UUID.fromString(laskentaDto.getUuid()), Collections.singleton("678"));
      Assertions.fail("Olematon hakukohde merkitty valmiiksi");
    } catch (Exception e) {}
  }

  @Test
  public void testAloittamatontaHakukohdettaEiVoiMerkataValmiiksi() {
    LaskentaDto laskentaDto = this.luoLaskenta("laskenta", List.of("123", "234", "345"));
    try {
      this.seurantaDao.merkkaaHakukohteetValmiiksi(UUID.fromString(laskentaDto.getUuid()), Collections.singleton("123"));
      Assertions.fail("Aloittamaton hakukohde merkitty valmiiksi");
    } catch (Exception e) {}
  }

  @Test
  public void testValmistaHakukohdettaEiVoiMerkataValmiiksi() {
    LaskentaDto laskentaDto = this.luoLaskenta("laskenta", List.of("123", "234", "345"));
    this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10);
    this.seurantaDao.merkkaaHakukohteetValmiiksi(UUID.fromString(laskentaDto.getUuid()), Collections.singleton("123"));
    try {
      // hakukohde on jo valmis => virhe
      this.seurantaDao.merkkaaHakukohteetValmiiksi(UUID.fromString(laskentaDto.getUuid()), Collections.singleton("123"));
      Assertions.fail("Aloittamaton hakukohde merkitty valmiiksi");
    } catch (Exception e) {}
  }

  @Test
  public void testEpaonnistunuttaHakukohdettaEiVoiMerkataValmiiksi() {
    LaskentaDto laskentaDto = this.luoLaskenta("laskenta", List.of("123", "234", "345"));
    this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10);
    this.seurantaDao.merkkaaHakukohteetEpaonnistuneeksi(UUID.fromString(laskentaDto.getUuid()), Collections.singleton("123"), 1, "Kolossaalinen moka");
    try {
      // hakukohde on jo epäonnistunut => virhe
      this.seurantaDao.merkkaaHakukohteetValmiiksi(UUID.fromString(laskentaDto.getUuid()), Collections.singleton("123"));
      Assertions.fail("Aloittamaton hakukohde merkitty valmiiksi");
    } catch (Exception e) {}
  }

  @Test
  public void testMerkkaaHakukohdeEpaonnistuneeksi() {
    LaskentaDto laskentaDto = this.luoLaskenta("laskenta", List.of("123", "234", "345"));
    YhteenvetoDto yhteenvetoDto = this.seurantaDao.haeYhteenveto(laskentaDto.getUuid());
    Assertions.assertEquals(0, yhteenvetoDto.getHakukohteitaValmiina());

    this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10);
    this.seurantaDao.merkkaaHakukohteetEpaonnistuneeksi(UUID.fromString(laskentaDto.getUuid()), Collections.singleton("123"), 1, "viesti");

    yhteenvetoDto = this.seurantaDao.haeYhteenveto(laskentaDto.getUuid());
    Assertions.assertEquals(1, yhteenvetoDto.getHakukohteitaKeskeytetty());
  }

  @Test
  public void testOlematontaHakukohdettaEiVoiMerkataEpaonnistuneeksi() {
    LaskentaDto laskentaDto = this.luoLaskenta("laskenta", List.of("123", "234", "345"));
    try {
      this.seurantaDao.merkkaaHakukohteetEpaonnistuneeksi(UUID.fromString(laskentaDto.getUuid()), Collections.singleton("678"), 1, "Kolossaalinen moka");
      Assertions.fail("Olematon hakukohde merkitty valmiiksi");
    } catch (Exception e) {}
  }

  @Test
  public void testAloittamatontaHakukohdettaEiVoiMerkataEpaonnistuneeksi() {
    LaskentaDto laskentaDto = this.luoLaskenta("laskenta", List.of("123", "234", "345"));
    try {
      this.seurantaDao.merkkaaHakukohteetEpaonnistuneeksi(UUID.fromString(laskentaDto.getUuid()), Collections.singleton("123"), 1, "Kolossaalinen moka");
      Assertions.fail("Aloittamaton hakukohde merkitty valmiiksi");
    } catch (Exception e) {}
  }

  @Test
  public void testValmistaHakukohdettaEiVoiMerkataEpaonnistuneeksi() {
    LaskentaDto laskentaDto = this.luoLaskenta("laskenta", List.of("123", "234", "345"));
    this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10);
    this.seurantaDao.merkkaaHakukohteetValmiiksi(UUID.fromString(laskentaDto.getUuid()), Collections.singleton("123"));
    try {
      // hakukohde on jo valmis => virhe
      this.seurantaDao.merkkaaHakukohteetEpaonnistuneeksi(UUID.fromString(laskentaDto.getUuid()), Collections.singleton("123"), 1, "Kolossaalinen moka");
      Assertions.fail("Aloittamaton hakukohde merkitty valmiiksi");
    } catch (Exception e) {}
  }

  @Test
  public void testEpaonnistunuttaHakukohdettaEiVoiMerkataEpaonnistuneeksi() {
    LaskentaDto laskentaDto = this.luoLaskenta("laskenta", List.of("123", "234", "345"));
    this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10);
    this.seurantaDao.merkkaaHakukohteetEpaonnistuneeksi(UUID.fromString(laskentaDto.getUuid()), Collections.singleton("123"), 1, "Kolossaalinen moka");
    try {
      // hakukohde on jo epäonnistunut => virhe
      this.seurantaDao.merkkaaHakukohteetEpaonnistuneeksi(UUID.fromString(laskentaDto.getUuid()), Collections.singleton("123"), 1, "Kolossaalinen moka");
      Assertions.fail("Aloittamaton hakukohde merkitty valmiiksi");
    } catch (Exception e) {}
  }

  @Test
  public void testKunOsaHakukohteistaKeskenHakuKesken() {
    LaskentaDto laskentaDto = this.luoLaskenta("laskenta", List.of("123", "234", "345"));
    this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10);
    this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10);
    this.seurantaDao.merkkaaHakukohteetValmiiksi(UUID.fromString(laskentaDto.getUuid()), List.of("123", "234"));

    Assertions.assertEquals(LaskentaTila.MENEILLAAN, this.seurantaDao.haeYhteenveto(laskentaDto.getUuid()).getTila());
  }

  @Test
  public void testKunKaikkiHakukohteetLaskettuHakuValmis() {
    LaskentaDto laskentaDto = this.luoLaskenta("laskenta", List.of("123", "234", "345"));
    this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10);
    this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10);
    this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10);
    this.seurantaDao.merkkaaHakukohteetValmiiksi(UUID.fromString(laskentaDto.getUuid()), List.of("123", "234"));
    this.seurantaDao.merkkaaHakukohteetEpaonnistuneeksi(UUID.fromString(laskentaDto.getUuid()), List.of("345"), 1, "Kolossaalinen epäonnistuminen");

    Assertions.assertEquals(LaskentaTila.VALMIS, this.seurantaDao.haeYhteenveto(laskentaDto.getUuid()).getTila());
  }

  @Test
  public void testEpaonnistunutHakukohdeLasketaanUudestaanKunnesYrityksetTaynna() {
    LaskentaDto laskentaDto = this.luoLaskenta("laskenta", List.of("123"));
    this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10);
    this.seurantaDao.merkkaaHakukohteetEpaonnistuneeksi(UUID.fromString(laskentaDto.getUuid()), List.of("123"), 3, "Kolossaalinen epäonnistuminen");
    this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10);
    this.seurantaDao.merkkaaHakukohteetEpaonnistuneeksi(UUID.fromString(laskentaDto.getUuid()), List.of("123"), 3, "Kolossaalinen epäonnistuminen");
    this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10);
    this.seurantaDao.merkkaaHakukohteetEpaonnistuneeksi(UUID.fromString(laskentaDto.getUuid()), List.of("123"), 3, "Kolossaalinen epäonnistuminen");

    Assertions.assertTrue(this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10).isEmpty());
  }

  @Test
  public void testSamanNoodinHakukohteidenMaaraRajoitettu() {
    this.luoLaskenta("laskenta", List.of("123", "234", "345"));

    // noodi1:llä voi olla vain yksi laskenta käynnissä samaan aikaan
    Assertions.assertTrue(this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 1).isPresent());
    Assertions.assertTrue(this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 1).isEmpty());
  }

  @Test
  public void testEriNoodienHakukohteidenMaaraItsenainen() {
    this.luoLaskenta("laskenta", List.of("123", "234", "345"));

    // noodi1 ja noodi2 voivat kummatkin ottaa yhden hakukohteet laskettavaksi
    Assertions.assertTrue(this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 1).isPresent());
    Assertions.assertTrue(this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi2", 1).isPresent());
  }

  @Test
  public void testPeruutaAloittamatonLaskenta() {
    LaskentaDto laskentaDto = this.luoLaskenta("laskenta", List.of("123", "234", "345"));

    this.seurantaDao.peruutaLaskenta(laskentaDto.getUuid(), Optional.empty());

    Assertions.assertEquals(LaskentaTila.PERUUTETTU, this.seurantaDao.haeLaskenta(laskentaDto.getUuid()).get().getTila());
    Assertions.assertTrue(this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10).isEmpty());
  }

  @Test
  public void testPeruutaMeneillaanOlevaLaskenta() {
    LaskentaDto laskentaDto = this.luoLaskenta("laskenta", List.of("123", "234", "345"));
    this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10);

    this.seurantaDao.peruutaLaskenta(laskentaDto.getUuid(), Optional.empty());

    Assertions.assertEquals(LaskentaTila.PERUUTETTU, this.seurantaDao.haeLaskenta(laskentaDto.getUuid()).get().getTila());
    Assertions.assertTrue(this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10).isEmpty());
  }

  @Test
  public void testValmistaLaskentaaEiVoiPeruuttaa() {
    LaskentaDto laskentaDto = this.luoLaskenta("laskenta", List.of("123"));
    this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10);
    this.seurantaDao.merkkaaHakukohteetValmiiksi(UUID.fromString(laskentaDto.getUuid()), List.of("123"));

    try {
      this.seurantaDao.peruutaLaskenta(laskentaDto.getUuid(), Optional.empty());
      Assertions.fail("Valmista laskentaa ei pidä voida peruuttaa");
    } catch (Exception e) {}
  }

  @Test
  public void testPeruutettuaLaskentaaEiVoiPeruuttaa() {
    LaskentaDto laskentaDto = this.luoLaskenta("laskenta", List.of("123"));
    this.seurantaDao.peruutaLaskenta(laskentaDto.getUuid(), Optional.empty());

    try {
      this.seurantaDao.peruutaLaskenta(laskentaDto.getUuid(), Optional.empty());
      Assertions.fail("Peruutettua laskentaa ei pidä voida peruuttaa");
    } catch (Exception e) {}
  }
}
