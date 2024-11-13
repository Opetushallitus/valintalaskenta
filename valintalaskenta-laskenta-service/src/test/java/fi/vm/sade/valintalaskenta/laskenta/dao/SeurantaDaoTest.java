package fi.vm.sade.valintalaskenta.laskenta.dao;

import fi.vm.sade.valintalaskenta.domain.dto.seuranta.*;
import fi.vm.sade.valintalaskenta.testing.AbstractIntegrationTest;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
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
    Assertions.assertEquals(Optional.empty(), laskentaDto.getValinnanvaihe());
    Assertions.assertEquals(false, laskentaDto.getValintakoelaskenta());
    Assertions.assertEquals(Optional.of(1), laskentaDto.getJonosija());
    Assertions.assertEquals(true, laskentaDto.getLuotiinkoUusiLaskenta());
  }

  private LaskentaDto luoLaskenta(String nimi, Collection<String> hakukohdeOids, LaskentaTyyppi laskentaTyyppi) {
    return this.seurantaDao.haeLaskenta(this.seurantaDao.luoLaskenta("userOid", "haku1", nimi, "hakuOid", laskentaTyyppi,
        false, Optional.empty(), false,
        hakukohdeOids.stream().map(oid -> new HakukohdeDto(oid, "organisaatioOid")).toList()).getUuid()).get();
  }

  private LaskentaDto luoLaskenta(String nimi, Collection<String> hakukohdeOids) {
    return this.luoLaskenta(nimi, hakukohdeOids, LaskentaTyyppi.HAKU);
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
    // luodaan laskenta kahdella hakukohteella
    this.luoLaskenta("laskenta1", List.of("123", "234"));

    // saadaan saman laskennan hakukohteita oid-järjestyksessä
    Assertions.assertEquals("123", this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10).get().getRight().iterator().next());
    Assertions.assertEquals("234", this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10).get().getRight().iterator().next());
  }

  @Test
  public void testEriLaskentojenHakukohteitaTehdaanLuontiJarjestyksessa() {
    // luodaan kaksi erillistä laskentaa yhdellä hakukohteella
    LaskentaDto laskentaDto1 = this.luoLaskenta("laskenta1", Collections.singleton("234"));
    LaskentaDto laskentaDto2 = this.luoLaskenta("laskenta2", Collections.singleton("123"));

    // saadaan eri laskentojen hakukohteet laskentojen luontijärjestyksessä
    Assertions.assertEquals(laskentaDto1.getUuid(), this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10).get().getLeft().toString());
    Assertions.assertEquals(laskentaDto2.getUuid(), this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10).get().getLeft().toString());
  }

  @Test
  public void testMerkkaaHakukohdeValmiiksi() {
    // luodaan laskenta useammalla hakukohteella
    LaskentaDto laskentaDto = this.luoLaskenta("laskenta", List.of("123", "234", "345"));
    Optional<YhteenvetoDto> yhteenvetoDto = this.seurantaDao.haeYhteenveto(laskentaDto.getUuid());
    Assertions.assertEquals(0, yhteenvetoDto.get().getHakukohteitaValmiina());

    // otetaan hakukohde työn alle ja merkataan se valmiiksi
    this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10);
    this.seurantaDao.merkkaaHakukohteetValmiiksi(UUID.fromString(laskentaDto.getUuid()), Collections.singleton("123"));

    // yhteenvedossa näkyy että yksi hakukohde valmis
    yhteenvetoDto = this.seurantaDao.haeYhteenveto(laskentaDto.getUuid());
    Assertions.assertEquals(1, yhteenvetoDto.get().getHakukohteitaValmiina());
  }

  @Test
  public void testMerkkaaOsittainlaskettuJaKeskeytettyHakukohdeValmiiksi() {
    // luodaan laskenta useammalla hakukohteella
    LaskentaDto laskentaDto = this.luoLaskenta("laskenta", List.of("123", "234", "345"));
    Optional<YhteenvetoDto> yhteenvetoDto = this.seurantaDao.haeYhteenveto(laskentaDto.getUuid());
    Assertions.assertEquals(0, yhteenvetoDto.get().getHakukohteitaValmiina());

    // otetaan hakukohde työn alle ja merkataan se valmiiksi
    this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10);
    this.seurantaDao.merkkaaHakukohteetValmiiksi(UUID.fromString(laskentaDto.getUuid()), Collections.singleton("123"));

    // peruutetaan laskenta
    this.seurantaDao.peruutaLaskenta(laskentaDto.getUuid(), Optional.empty());

    // merkataan hakukohteet valmiiksi, tämä ei vaikuta mitään koska laskenta keskeytetty
    this.seurantaDao.merkkaaHakukohteetValmiiksi(UUID.fromString(laskentaDto.getUuid()), List.of("123", "234", "345"));

    // yhteenvedossa näkyy että hakukohteet keskeytetty
    yhteenvetoDto = this.seurantaDao.haeYhteenveto(laskentaDto.getUuid());
    Assertions.assertEquals(3, yhteenvetoDto.get().getHakukohteitaKeskeytetty());
  }

  @Test
  public void testMerkkaaKeskeytettyHakukohdeValmiiksi() {
    // luodaan laskenta useammalla hakukohteella
    LaskentaDto laskentaDto = this.luoLaskenta("laskenta", List.of("123", "234", "345"));
    Optional<YhteenvetoDto> yhteenvetoDto = this.seurantaDao.haeYhteenveto(laskentaDto.getUuid());
    Assertions.assertEquals(0, yhteenvetoDto.get().getHakukohteitaValmiina());

    // peruutetaan laskenta
    this.seurantaDao.peruutaLaskenta(laskentaDto.getUuid(), Optional.empty());

    // merkataan hakukohde valmiiksi, tämä ei vaikuta mitään koska laskenta keskeytetty
    this.seurantaDao.merkkaaHakukohteetValmiiksi(UUID.fromString(laskentaDto.getUuid()), Collections.singleton("123"));

    // yhteenvedossa näkyy että yksi hakukohde valmis
    yhteenvetoDto = this.seurantaDao.haeYhteenveto(laskentaDto.getUuid());
    Assertions.assertEquals(3, yhteenvetoDto.get().getHakukohteitaKeskeytetty());
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
      Assertions.fail("Valmis hakukohde merkitty valmiiksi");
    } catch (Exception e) {}
  }

  @Test
  public void testMerkkaaHakukohdeEpaonnistuneeksi() {
    // luodaan laskenta useammalla hakukohteella
    LaskentaDto laskentaDto = this.luoLaskenta("laskenta", List.of("123", "234", "345"));
    Optional<YhteenvetoDto> yhteenvetoDto = this.seurantaDao.haeYhteenveto(laskentaDto.getUuid());
    Assertions.assertEquals(0, yhteenvetoDto.get().getHakukohteitaValmiina());

    // otetaan hakukohde työn alle ja merkataan epäonnistuneeksi
    this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10);
    this.seurantaDao.merkkaaHakukohteetEpaonnistuneeksi(UUID.fromString(laskentaDto.getUuid()), Collections.singleton("123"), 1, "viesti");

    // yhteenvedossa näkyy että yksi hakukohde epäonnistunut
    yhteenvetoDto = this.seurantaDao.haeYhteenveto(laskentaDto.getUuid());
    Assertions.assertEquals(1, yhteenvetoDto.get().getHakukohteitaKeskeytetty());
  }

  @Test
  public void testOlematontaHakukohdettaEiVoiMerkataEpaonnistuneeksi() {
    LaskentaDto laskentaDto = this.luoLaskenta("laskenta", List.of("123", "234", "345"));
    try {
      this.seurantaDao.merkkaaHakukohteetEpaonnistuneeksi(UUID.fromString(laskentaDto.getUuid()), Collections.singleton("678"), 1, "Kolossaalinen moka");
      Assertions.fail("Olematon hakukohde merkitty epäonnistuneeksi");
    } catch (Exception e) {}
  }

  @Test
  public void testAloittamatontaHakukohdettaEiVoiMerkataEpaonnistuneeksi() {
    LaskentaDto laskentaDto = this.luoLaskenta("laskenta", List.of("123", "234", "345"));
    try {
      this.seurantaDao.merkkaaHakukohteetEpaonnistuneeksi(UUID.fromString(laskentaDto.getUuid()), Collections.singleton("123"), 1, "Kolossaalinen moka");
      Assertions.fail("Aloittamaton hakukohde merkitty epäonnistuneeksi");
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
      Assertions.fail("Valmis hakukohde merkitty epäonnistuneeksi");
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
      Assertions.fail("Epäonnistunut hakukohde merkitty epäonnistuneeksi");
    } catch (Exception e) {}
  }

  @Test
  public void testKunOsaHakukohteistaKeskenHakuKesken() {
    // luodaan laskenta usemalla hakukohteella
    LaskentaDto laskentaDto = this.luoLaskenta("laskenta", List.of("123", "234", "345"));

    // merkataan kaikki paitsi yksi hakukohde valmiiksi
    this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10);
    this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10);
    this.seurantaDao.merkkaaHakukohteetValmiiksi(UUID.fromString(laskentaDto.getUuid()), List.of("123", "234"));

    // laskenta edelleen kesken
    Assertions.assertEquals(LaskentaTila.MENEILLAAN, this.seurantaDao.haeYhteenveto(laskentaDto.getUuid()).get().getTila());
  }

  @Test
  public void testKunKaikkiHakukohteetLaskettuHakuValmis() {
    // luodaan laskenta useammalla hakukohteella
    LaskentaDto laskentaDto = this.luoLaskenta("laskenta", List.of("123", "234", "345"));

    // merkataan kaikki hakukohteet valmiiksi
    this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10);
    this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10);
    this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10);
    this.seurantaDao.merkkaaHakukohteetValmiiksi(UUID.fromString(laskentaDto.getUuid()), List.of("123", "234"));
    this.seurantaDao.merkkaaHakukohteetEpaonnistuneeksi(UUID.fromString(laskentaDto.getUuid()), List.of("345"), 1, "Kolossaalinen epäonnistuminen");

    // laskent on valmis
    Assertions.assertEquals(LaskentaTila.VALMIS, this.seurantaDao.haeYhteenveto(laskentaDto.getUuid()).get().getTila());
  }

  @Test
  public void testEpaonnistunutHakukohdeLasketaanUudestaanKunnesYrityksetTaynna() {
    // luodaan laskenta yhdellä hakukohteella
    LaskentaDto laskentaDto = this.luoLaskenta("laskenta", List.of("123"));

    // merkataan hakukohde epäonnistuneeksi maksimiyritysten verran
    this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10);
    this.seurantaDao.merkkaaHakukohteetEpaonnistuneeksi(UUID.fromString(laskentaDto.getUuid()), List.of("123"), 3, "Kolossaalinen epäonnistuminen");
    this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10);
    this.seurantaDao.merkkaaHakukohteetEpaonnistuneeksi(UUID.fromString(laskentaDto.getUuid()), List.of("123"), 3, "Kolossaalinen epäonnistuminen");
    this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10);
    this.seurantaDao.merkkaaHakukohteetEpaonnistuneeksi(UUID.fromString(laskentaDto.getUuid()), List.of("123"), 3, "Kolossaalinen epäonnistuminen");

    // hakukohdetta ei enää saada lasketaavaksi
    Assertions.assertTrue(this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10).isEmpty());
  }

  @Test
  public void testSamanNoodinHakukohteidenMaaraRajoitettu() {
    // luodaan laskenta useammalla hakukohteella
    this.luoLaskenta("laskenta", List.of("123", "234", "345"));

    // otetaan yksi hakukohde työn alle
    Assertions.assertTrue(this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 1).isPresent());

    // koska yhtäaikaisten hakukohteiden määrä saavutettu, uutta ei saada työn alle
    Assertions.assertTrue(this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 1).isEmpty());
  }

  @Test
  public void testEriNoodienHakukohteidenMaaraItsenainen() {
    // luodaan laskenta useammalla hakukohteella
    this.luoLaskenta("laskenta", List.of("123", "234", "345"));

    // noodi1 ja noodi2 voivat kummatkin ottaa yhden hakukohteet laskettavaksi
    Assertions.assertTrue(this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 1).isPresent());
    Assertions.assertTrue(this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi2", 1).isPresent());
  }

  @Test
  public void testPeruutaAloittamatonLaskenta() {
    // luodaan laskenta useammalla hakukohteella
    LaskentaDto laskentaDto = this.luoLaskenta("laskenta", List.of("123", "234", "345"));

    // peruutetaan laskenta
    this.seurantaDao.peruutaLaskenta(laskentaDto.getUuid(), Optional.empty());

    // laskenta on peruutettu-tilassa
    Assertions.assertEquals(LaskentaTila.PERUUTETTU, this.seurantaDao.haeLaskenta(laskentaDto.getUuid()).get().getTila());
    // työn alle ei saada hakukohteita
    Assertions.assertTrue(this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10).isEmpty());
  }

  @Test
  public void testPeruutaMeneillaanOlevaLaskenta() {
    // luodaan laskenta useammalla hakukohteella ja otetaan yksi hakukohde työn alle
    LaskentaDto laskentaDto = this.luoLaskenta("laskenta", List.of("123", "234", "345"));
    this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10);

    // peruutetaan laskenta
    this.seurantaDao.peruutaLaskenta(laskentaDto.getUuid(), Optional.empty());

    // laskenta on perutettu-tilassa
    Assertions.assertEquals(LaskentaTila.PERUUTETTU, this.seurantaDao.haeLaskenta(laskentaDto.getUuid()).get().getTila());
    // työn alle ei saada hakukohteita
    Assertions.assertTrue(this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 10).isEmpty());
  }

  @Test
  public void testValmistaLaskentaaEiVoiPeruuttaa() {
    // luodaan laskenta yhdella hakukohteella ja merkataan hakukohde valmiiksi jolloin myös laskenta on valmis
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
    // luodaan laskenta ja peruutetaan se samoin tein
    LaskentaDto laskentaDto = this.luoLaskenta("laskenta", List.of("123"));
    this.seurantaDao.peruutaLaskenta(laskentaDto.getUuid(), Optional.empty());

    try {
      this.seurantaDao.peruutaLaskenta(laskentaDto.getUuid(), Optional.empty());
      Assertions.fail("Peruutettua laskentaa ei pidä voida peruuttaa");
    } catch (Exception e) {}
  }

  @Test
  public void testLaskeSamaHakukohdeMonesti() {
    LaskentaDto laskentaDto;
    Optional<ImmutablePair<UUID, Collection<String>>> tyonAlla;
    Collection<String> hakukohteet;

    // lasketaan hakukohde kertaalleen
    laskentaDto = this.luoLaskenta("laskenta", List.of("123"));
    tyonAlla = this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 1);
    hakukohteet = tyonAlla.get().right;
    this.seurantaDao.merkkaaHakukohteetValmiiksi(UUID.fromString(laskentaDto.getUuid()), hakukohteet);

    // luodaan uudestaan
    laskentaDto = this.luoLaskenta("laskenta", List.of("123"));

    // laskenta käynnistyy
    tyonAlla = this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 1);
    hakukohteet = tyonAlla.get().right;
    Assertions.assertEquals(LaskentaTila.MENEILLAAN, this.seurantaDao.haeYhteenveto(laskentaDto.getUuid()).get().getTila());

    // ja valmistuu
    this.seurantaDao.merkkaaHakukohteetValmiiksi(UUID.fromString(laskentaDto.getUuid()), hakukohteet);
    Assertions.assertEquals(LaskentaTila.VALMIS, this.seurantaDao.haeYhteenveto(laskentaDto.getUuid()).get().getTila());
  }

  @Test
  public void testValintaRyhmalaskentaPerusFlow() {
    LaskentaDto laskentaDto = this.luoLaskenta("laskenta", List.of("123", "234", "345"), LaskentaTyyppi.VALINTARYHMA);

    Optional<ImmutablePair<UUID, Collection<String>>> tyonAlla = this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi1", 1);
    Collection<String> hakukohteet = tyonAlla.get().right;

    // kaikki hakukohteet otetaan työn alle yhtäaikaa, maxYhtaaikaisetHakukohteet-parametrista ei välitettä
    Assertions.assertEquals(3, hakukohteet.size());

    // laskenta on työn alla
    Assertions.assertEquals(LaskentaTila.MENEILLAAN, this.seurantaDao.haeYhteenveto(laskentaDto.getUuid()).get().getTila());

    // merkataan hakukohteet valmiiksi
    this.seurantaDao.merkkaaHakukohteetValmiiksi(UUID.fromString(laskentaDto.getUuid()), hakukohteet);

    // laskenta on valmis
    Assertions.assertEquals(LaskentaTila.VALMIS, this.seurantaDao.haeYhteenveto(laskentaDto.getUuid()).get().getTila());
  }

  @Test
  public void testOtaSeuraavaLaskentaTyonalleThreadSafety() {
    // luodaan laskenta 500 hakukohteella
    Set<String> expectedHakukohteet = IntStream.range(0, 500).mapToObj(i -> "hakukohde" + i).collect(Collectors.toSet());
    LaskentaDto laskenta = this.luoLaskenta("laskenta", expectedHakukohteet);

    // otetaan rinnakkaisesti työn alle 500 laskentaa
    Executor executor = Executors.newWorkStealingPool(100);
    CompletableFuture<String>[] hakukohteet = IntStream.range(0, 500)
        .mapToObj(i -> CompletableFuture.supplyAsync(
            () -> {
              Optional<ImmutablePair<UUID, Collection<String>>> result = this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi" + i, 10);
              String hakukohdeOid = result.get().getRight().iterator().next();
              return hakukohdeOid;
            }, executor)).toArray(s -> new CompletableFuture[s]);
    CompletableFuture.allOf(hakukohteet);
    Set<String> actualHakukohteet = Arrays.stream(hakukohteet).map(f -> f.join()).collect(Collectors.toSet());

    // työn alle otetut vastaavat luotuja
    Assertions.assertEquals(expectedHakukohteet, actualHakukohteet);
  }


  @Test
  public void testMerkitseHakukohdeLasketuksiThreadSafety() {
    // luodaan 500 laskentaa kahdella hakukohteella
    Set<ImmutablePair<UUID, String>> expectedHakukohteet = IntStream.range(0, 500).mapToObj(i -> {
      Collection<String> hakukohteet = List.of(i + "_1", i + "_2");
      UUID uuid = UUID.fromString(this.luoLaskenta("laskenta" + i, hakukohteet).getUuid());
      Set<ImmutablePair<UUID, String>> hks = hakukohteet.stream().map(hk -> new ImmutablePair<>(uuid, hk)).collect(Collectors.toSet());
      return hks;
    }).flatMap(Collection::stream).collect(Collectors.toSet());

    // otetaan kaikki työn alle
    expectedHakukohteet.stream().forEach(hk -> this.seurantaDao.otaSeuraavatHakukohteetTyonAlle("noodi", 1000));

    // merkitään rinnakkaisesti valmiiksi tai epäonnistuneeksi
    Executor executor = Executors.newFixedThreadPool(100);
    CompletableFuture<?>[] futures = expectedHakukohteet.stream().map(hk -> CompletableFuture.supplyAsync(
        () -> {
          UUID uuid = hk.getLeft();
          String hakukohdeOid = hk.getRight();
          if(Math.random()>0.5d) {
            this.seurantaDao.merkkaaHakukohteetValmiiksi(uuid, List.of(hakukohdeOid));
          } else {
            this.seurantaDao.merkkaaHakukohteetEpaonnistuneeksi(uuid, List.of(hakukohdeOid), 1, "");
          }
          return hakukohdeOid;
        }, executor)).toArray(i -> new CompletableFuture[i]);
    CompletableFuture.allOf(futures).join();

    // kaikki laskennat valmiita
    expectedHakukohteet.parallelStream().forEach(hk -> {
      Assertions.assertEquals(LaskentaTila.VALMIS, this.seurantaDao.haeYhteenveto(hk.getLeft().toString()).get().getTila());
    });
  }
}
