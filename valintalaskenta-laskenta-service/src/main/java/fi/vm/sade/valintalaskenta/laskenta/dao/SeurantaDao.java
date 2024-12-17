package fi.vm.sade.valintalaskenta.laskenta.dao;

import fi.vm.sade.valintalaskenta.domain.dto.seuranta.*;
import java.time.Instant;
import java.util.Collection;
import java.util.Date;
import java.util.Optional;
import java.util.UUID;
import org.apache.commons.lang3.tuple.ImmutablePair;

/**
 * Laskentojen koordinointiin liittyvät tietokantatoiminnot.
 *
 * <p>Useat metodit sisältävät useampia tietokantakutsuja. Jos transaktio on jo käynnissä, ne
 * suoritetaan osana olemassaolevaa transaktiota, muuten käynnistetään uusi transaktio. Huomioi tämä
 * suunnittelulähtökohta luokkaa muuttaessasi.
 */
public interface SeurantaDao {

  /**
   * Luo uuden laskennan
   *
   * @param userOID laskennan käynnistäneen käyttäjän tunniste
   * @param haunnimi haun nimi (raportointinäkymää varten)
   * @param nimi laskennan nimi (raportointinäkymää varten)
   * @param hakuOid haun tunniste
   * @param tyyppi haun tyyppi
   * @param erillishaku onko kyseessä erillishaku
   * @param valinnanvaihe laskettava valinnanvaihe (empty tarkoittaa että lasketaan kaikki vaiheet)
   * @param valintakoelaskenta onko kyseessä valintakoelaskenta
   * @param hakukohdeOids laskettavat hakukohteet
   * @return
   */
  TunnisteDto luoLaskenta(
      String userOID,
      String haunnimi,
      String nimi,
      String hakuOid,
      LaskentaTyyppi tyyppi,
      boolean erillishaku,
      Optional<Integer> valinnanvaihe,
      boolean valintakoelaskenta,
      Collection<HakukohdeDto> hakukohdeOids);

  /**
   * Hakee yksittäiseen laskentaan liittyvät tiedot
   *
   * @param uuid laskennan tunniste
   */
  Optional<LaskentaDto> haeLaskenta(String uuid);

  /**
   * Ottaa seuraavan hakukohteen työn alle. Jos kyseessä on valintaryhmälaskenta, kaikki hakukohteet
   * otetaan työn alle samalla kertaa.
   *
   * @param noodiId noodi joka on käynnistämässä hakukohteen laskentaa
   * @param maxYhtaaikaisetHakukohteet maksimimäärä hakukohteita joka voi olla käynnissä yhdellä
   *     noodilla yhtäaikaa
   * @return käynnistetty hakukohde (hakukohteet)
   */
  Optional<ImmutablePair<UUID, Collection<String>>> otaSeuraavatHakukohteetTyonAlle(
      String noodiId, int maxYhtaaikaisetHakukohteet);

  /**
   * Peruuttaa luodun laskennan, ts. merkitsee laskennan tilan peruutetuksi ja hakukohteiden
   * laskennan tilan keskeytetyksi
   *
   * @param uuid keskeytettävän laskennan tunniste
   * @param ilmoitus laskennan keskeyttämisen syy
   */
  void peruutaLaskenta(String uuid, Optional<IlmoitusDto> ilmoitus);

  /**
   * Resetoi laskennan, ts. siirtää laskennan tilaan ALOITTAMATTA, ja keskeytyneet hakukohteet
   * tilaan TEKEMATTA
   *
   * @param uuid laskennan tunniste
   * @param nollaaIlmoitukset poistetaanko laskentaa liitetyt ilmoitukset
   * @return resetoitu laskenta
   */
  LaskentaDto resetoiLaskenta(String uuid, boolean nollaaIlmoitukset);

  /**
   * Merkitsee lasketut hakukohteet valmiiksi. Mikäli laskennan kaikki hakukohteet on laskettu
   * onnistuneesti merkitään laskenta valmiiksi. Mikäli osan hakukohteista on epäonnistunut
   * siirretään laskenta virhetilaan.
   *
   * @param uuid laskennan tunniste
   * @param hakukohdeOids valmiiksi merkittävät hakukohteet
   */
  void merkkaaHakukohteetValmiiksi(UUID uuid, Collection<String> hakukohdeOids);

  /**
   * Merkitsee epäonnistuneet hakukohteet epäonnistuneiksi jos maksimimäärä yrityksiä on käytetty,
   * muuten siirtää ne TEKEMÄTTÄ-tilaan.
   *
   * @param uuid laskennan tunniste
   * @param hakukohdeOids epäonnistuneet hakukohteet
   * @param maxYritykset maksimäärä yrityksiä jonka jälkeen hakukohteen laskenta todetaan
   *     epäonnistuneeksi
   * @param message epäonnistumisen syy
   */
  void merkkaaHakukohteetEpaonnistuneeksi(
      UUID uuid, Collection<String> hakukohdeOids, int maxYritykset, String message);

  /**
   * Merkkaa laskentaa suorittavan noodin elossaolevaksi
   *
   * @param noodiId noodin tunniste
   */
  void merkkaaNoodiLiveksi(String noodiId);

  /**
   * Resetoi niillä noodeilla käynnissä olevat hakukohteiden laskennat jotka eivät ole ilmoittaneet
   * olevansa elossa tietyn ajan sisällä.
   *
   * @param viive kuinka uusi viimeisimmän elonmerkin pitää olla
   */
  void resetoiKuolleidenNoodienLaskennat(int viive);

  int liveNoodienMaara(int viive);

  /**
   * Hakee yhteenvedon laskennan kulusta
   *
   * @param uuid laskennan tunniste
   */
  Optional<YhteenvetoDto> haeYhteenveto(String uuid);

  /**
   * Yhteenvedot kaikista laskennoista. Käytetään valintalaskennan hallinnta -näkymässä.
   *
   * @param luotuAlkaen palautetaan tätä uudemmat laskennat
   */
  Collection<YhteenvetoDto> haeYhteenvetoKaikilleLaskennoille(Instant luotuAlkaen);

  /**
   * Siivoaa ylimaaraiset laskennat pois
   *
   * @param viimeinenSailottavaPaivamaara tätä vanhemmat laskennat siivotaan pois
   */
  void siivoa(Date viimeinenSailottavaPaivamaara);

  /**
   * Lukee parametrin tietokannasta
   *
   * @param nimi parametrin nimi
   * @return parametrin arvo
   */
  String lueParametri(String nimi);
}
