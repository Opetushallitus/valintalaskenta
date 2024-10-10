package fi.vm.sade.valintalaskenta.laskenta.dao;

import static org.junit.Assert.*;

import fi.vm.sade.valintalaskenta.domain.dto.seuranta.*;
import fi.vm.sade.valintalaskenta.testing.AbstractIntegrationTest;

import java.util.*;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * @author Jussi Jartamo
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
  public void testaaMerkkaaLaskennanTilaJaHakukohteidenTilaKerrallaValmistuneelleLaskennalle() {
    String uuid = aloitaUusiLaskenta(Optional.of(randomHakukohde()));
    YhteenvetoDto y =
        seurantaDao.peruutaLaskenta(
            uuid,
            Optional.of(IlmoitusDto.ilmoitus("Toimiiko!")));
    assertOikeaLaskentaEiOleNull(uuid, y);
  }

/*
  @Test
  public void testaaLisaaIlmoitusHakukohteelle() {
    final String hakukohde = randomHakukohde();
    String uuid = aloitaUusiLaskenta(Optional.of(hakukohde));
    IlmoitusDto ilmoitusDto = new IlmoitusDto(IlmoitusTyyppi.ILMOITUS, "Jee");
    YhteenvetoDto y = seurantaDao.lisaaIlmoitus(uuid, hakukohde, ilmoitusDto);
    assertOikeaLaskentaEiOleNull(uuid, y);
    Assertions.assertEquals(
        ilmoitusDto.toString(),
        seurantaDao.haeLaskenta(uuid).get().getHakukohteet().stream()
            .filter(hk -> hakukohde.equals(hk.getHakukohdeOid()))
            .findFirst()
            .map(hk -> hk.getIlmoitukset())
            .get()
            .get(0)
            .toString());
  }
*/

/*
  @Test
  public void testaaLisaaIlmoitusLaskennalle() {
    final String hakukohde = randomHakukohde();
    String uuid = aloitaUusiLaskenta(Optional.of(hakukohde));
    IlmoitusDto ilmoitusDto = new IlmoitusDto(IlmoitusTyyppi.ILMOITUS, "Jee");
    YhteenvetoDto y = seurantaDao.merkkaaTila(uuid, LaskentaTila.VALMIS, Optional.of(ilmoitusDto));
    assertOikeaLaskentaEiOleNull(uuid, y);
    Assertions.assertEquals(
        ilmoitusDto.toString(), seurantaDao.haeLaskenta(uuid).get().getIlmoitus().toString());
  }

*/
  @Test
  public void testaaMerkkaaLaskennanTilaJaHakukohteidenTilaKerralla() {
    final String hakukohde = randomHakukohde();
    String uuid = aloitaUusiLaskenta(Optional.of(hakukohde));
    YhteenvetoDto y =
        seurantaDao.peruutaLaskenta(
            uuid,
            Optional.of(IlmoitusDto.ilmoitus("Toimiiko!")));
    assertOikeaLaskentaEiOleNull(uuid, y);
  }

/*
  @Test
  public void testaaMerkkaaLaskennanTila() {
    final String hakukohde = randomHakukohde();
    String uuid = aloitaUusiLaskenta(Optional.of(hakukohde));
    YhteenvetoDto y =
        seurantaDao.merkkaaTila(
            uuid, LaskentaTila.VALMIS, Optional.of(IlmoitusDto.ilmoitus("Toimiiko!")));
    assertOikeaLaskentaEiOleNull(uuid, y);
  }

  @Test
  public void testaaMerkkaaLaskennanTilaMerkatulleLaskennalle() {
    final String hakukohde = randomHakukohde();
    String uuid = aloitaUusiLaskenta(Optional.of(hakukohde));
    YhteenvetoDto y =
        seurantaDao.merkkaaTila(
            uuid, LaskentaTila.VALMIS, Optional.of(IlmoitusDto.ilmoitus("Toimiiko!")));
    assertOikeaLaskentaEiOleNull(uuid, y);
  }
*/

/*
  @Test
  public void testaaLaskennanAloittaminenIlmanLaskentaaPalauttaaNull() {
    assertTrue(seurantaDao.otaSeuraavaLaskentaTyonAlle().isEmpty());
  }

  @Test
  public void testaaKolmasLaskennanAloitusPalauttaaNullKunVainKaksiLaskentaaOlemassa() {
    Collection<HakukohdeDto> hakukohdeOids =
        Arrays.asList(new HakukohdeDto("h1", "o1"), new HakukohdeDto("h2", "o2"));
    seurantaDao.luoLaskenta(
        "U0", "", "", "hakuOid1", LaskentaTyyppi.HAKU, true, null, null, hakukohdeOids);
    seurantaDao.luoLaskenta(
        "U0", "", "", "hakuOid2", LaskentaTyyppi.HAKU, true, null, null, hakukohdeOids);
    assertTrue(seurantaDao.otaSeuraavaLaskentaTyonAlle().isPresent());
    assertTrue(seurantaDao.otaSeuraavaLaskentaTyonAlle().isPresent());
    assertTrue(seurantaDao.otaSeuraavaLaskentaTyonAlle().isEmpty());
  }

  @Test
  public void testaaSeuranta() throws InterruptedException {
    String hakuOid = "hakuOid";
    Collection<HakukohdeDto> hakukohdeOids =
        Arrays.asList(
            new HakukohdeDto("hk1", "oo1"),
            new HakukohdeDto("hk2", "oo2"),
            new HakukohdeDto("hk3", "oo3"));
    seurantaDao.luoLaskenta(
        "U0", "", "", hakuOid, LaskentaTyyppi.HAKU, true, null, null, hakukohdeOids);
    String uuid = seurantaDao.otaSeuraavaLaskentaTyonAlle().map(l -> l.getUuid()).get();
    seurantaDao.merkkaaTila(uuid, "hk3", HakukohdeTila.KESKEYTETTY);
    seurantaDao.merkkaaTila(uuid, "hk2", HakukohdeTila.VALMIS);
    seurantaDao.merkkaaTila(uuid, "hk1", HakukohdeTila.KESKEYTETTY);
    seurantaDao.merkkaaTila(uuid, "hk1", HakukohdeTila.KESKEYTETTY);
    seurantaDao.merkkaaTila(uuid, "hk2", HakukohdeTila.VALMIS);
    seurantaDao.merkkaaTila(uuid, "hk3", HakukohdeTila.VALMIS);
    seurantaDao.luoLaskenta(
        "U0", "", "", hakuOid, LaskentaTyyppi.HAKU, true, null, null, hakukohdeOids);
    seurantaDao.lisaaIlmoitus(
        uuid, "hk1", new IlmoitusDto(IlmoitusTyyppi.ILMOITUS, "Ei toimi", null));
    seurantaDao.haeYhteenvedotHaulle(hakuOid);
    Collection<YhteenvetoDto> yhteenvedot =
        seurantaDao.haeYhteenvedotHaulle(hakuOid, LaskentaTyyppi.HAKU);
    assertEquals(1, yhteenvedot.size());
    LaskentaDto l = seurantaDao.haeLaskenta(uuid).get();
    l.getHakukohteet()
        .forEach(
            hk -> {
              LOG.info(
                  "Hakukohde {} ja organisaatio {}", hk.getHakukohdeOid(), hk.getOrganisaatioOid());
              assertTrue("Organisaatio Oid puuttui", hk.getOrganisaatioOid() != null);
              assertTrue("HakukohdeOid puuttui", hk.getHakukohdeOid() != null);
            });
    assertEquals(LaskentaTila.MENEILLAAN, seurantaDao.haeLaskenta(uuid).get().getTila());
    seurantaDao.merkkaaTila(
        uuid, LaskentaTila.VALMIS, Optional.of(IlmoitusDto.ilmoitus("Toimiiko!")));
    assertEquals(LaskentaTila.VALMIS, seurantaDao.haeLaskenta(uuid).get().getTila());
    seurantaDao.merkkaaTila(
        uuid, LaskentaTila.PERUUTETTU, Optional.of(IlmoitusDto.ilmoitus("Toimiiko!")));
    assertEquals(LaskentaTila.VALMIS, seurantaDao.haeLaskenta(uuid).get().getTila());
    seurantaDao.merkkaaTila(
        uuid, "hk3", HakukohdeTila.VALMIS, new IlmoitusDto(IlmoitusTyyppi.VAROITUS, "Hehei2"));
    seurantaDao.lisaaIlmoitus(uuid, "hk3", new IlmoitusDto(IlmoitusTyyppi.VAROITUS, "Hehei3"));
    String uuid2 =
        seurantaDao
            .luoLaskenta(
                "U0", "", "", hakuOid, LaskentaTyyppi.HAKU, true, null, null, hakukohdeOids)
            .getUuid();
    assertNotSame(uuid, uuid2);
    assertEquals(seurantaDao.resetoiEiValmiitHakukohteet(uuid, false).getUuid(), uuid2);
    assertEquals(2, seurantaDao.haeYhteenvedotHaulle(hakuOid).size());
    seurantaDao.siivoa(Date.from(Instant.now().minus(Period.ofDays(1))));
    assertEquals(2, seurantaDao.haeYhteenvedotHaulle(hakuOid).size());
    seurantaDao.siivoa(Date.from(Instant.now().plusSeconds(3600)));
    assertEquals(0, seurantaDao.haeYhteenvedotHaulle(hakuOid).size());
  }

  @Test
  public void testaaHakukohteetKerrallaValmiiksi() {
    String hakuOid = "hakuOidKerrallaValmiiksi";
    Collection<HakukohdeDto> hakukohdeOids =
        Arrays.asList(
            new HakukohdeDto("hk1", "oo1"),
            new HakukohdeDto("hk2", "oo2"),
            new HakukohdeDto("hk3", "oo3"));
    seurantaDao.luoLaskenta(
        "U0", "", "", hakuOid, LaskentaTyyppi.VALINTARYHMA, true, null, null, hakukohdeOids);
    String uuid = seurantaDao.otaSeuraavaLaskentaTyonAlle().map(l -> l.getUuid()).get();
    YhteenvetoDto y =
        seurantaDao.merkkaaTila(
            uuid,
            LaskentaTila.VALMIS,
            HakukohdeTila.VALMIS,
            Optional.of(IlmoitusDto.ilmoitus("Toimiiko!")));
    LOG.error("### {}", new GsonBuilder().setPrettyPrinting().create().toJson(y));
    assertEquals(LaskentaTila.VALMIS, y.getTila());
    assertEquals(3, y.getHakukohteitaValmiina());
    assertEquals(3, y.getHakukohteitaYhteensa());
    assertEquals(0, y.getHakukohteitaKeskeytetty());
  }

  @Test
  public void testaaHakukohteetKerrallaOhitettu() {
    String hakuOid = "hakuOidKerrallaOhitettu";
    Collection<HakukohdeDto> hakukohdeOids =
        Arrays.asList(
            new HakukohdeDto("hk1", "oo1"),
            new HakukohdeDto("hk2", "oo2"),
            new HakukohdeDto("hk3", "oo3"));

    seurantaDao.luoLaskenta(
        "U0", "", "", hakuOid, LaskentaTyyppi.VALINTARYHMA, true, null, null, hakukohdeOids);
    String uuid = seurantaDao.otaSeuraavaLaskentaTyonAlle().map(l -> l.getUuid()).get();
    YhteenvetoDto y =
        seurantaDao.merkkaaTila(
            uuid,
            LaskentaTila.VALMIS,
            HakukohdeTila.KESKEYTETTY,
            Optional.of(IlmoitusDto.ilmoitus("Toimiiko!")));
    LOG.error("### {}", new GsonBuilder().setPrettyPrinting().create().toJson(y));
    assertEquals(LaskentaTila.VALMIS, y.getTila());
    assertEquals(0, y.getHakukohteitaValmiina());
    assertEquals(3, y.getHakukohteitaYhteensa());
    assertEquals(3, y.getHakukohteitaKeskeytetty());
  }
*/

  private String luoUusiLaskenta(Optional<String> hakukohdeOid) {
    Collection<HakukohdeDto> hakukohdeOids =
        Arrays.asList(
            new HakukohdeDto(hakukohdeOid.orElse("h1"), "o1"), new HakukohdeDto("h2", "o2"));
    String uuid =
        seurantaDao
            .luoLaskenta("U0", "", "", "hk1", LaskentaTyyppi.HAKU, true, null, null, hakukohdeOids)
            .getUuid();
    return uuid;
  }

  private String aloitaUusiLaskenta(Optional<String> hakukohdeOid) {
    throw new UnsupportedOperationException();
    /*
    String uuid = luoUusiLaskenta(hakukohdeOid);
    seurantaDao.otaSeuraavatHakukohteetTyonAlle("", 10);


    return seurantaDao.otaSeuraavaLaskentaTyonAlle().map(l -> l.getUuid()).get();
*/
  }

  private void assertOikeaLaskentaEiOleNull(String uuid, YhteenvetoDto y) {
    assertNotNull(y);
    assertEquals(uuid, y.getUuid());
  }
}
