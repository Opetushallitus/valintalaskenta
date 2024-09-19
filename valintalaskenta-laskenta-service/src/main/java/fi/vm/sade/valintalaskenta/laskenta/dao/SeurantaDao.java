package fi.vm.sade.valintalaskenta.laskenta.dao;

import fi.vm.sade.valintalaskenta.domain.dto.seuranta.*;
import java.time.Instant;
import java.util.Collection;
import java.util.Date;
import java.util.Optional;

public interface SeurantaDao {

  /** Siivoaa ylimaaraiset laskennat pois */
  void siivoa(Date viimeinenSailottavaPaivamaara);

  /** Kaikki laskentaan liittyva tieto */
  Optional<LaskentaDto> haeLaskenta(String uuid);

  /** Yhteenveto laskennan kulusta */
  YhteenvetoDto haeYhteenveto(String uuid);

  /** Yhteenveto laskennan kulusta */
  Collection<YhteenvetoDto> haeYhteenvedotAlkamattomille(Collection<String> uuids);

  /** Yhteenvedot laskennan kulusta */
  Collection<YhteenvetoDto> haeYhteenvedotHaulle(String hakuOid);

  /** Yhteenvedot laskennan kulusta */
  Collection<YhteenvetoDto> haeYhteenvedotHaulle(String hakuOid, LaskentaTyyppi tyyppi);

  /** Yhteenvedot kaynnissa olevien laskentojen kulusta */
  Collection<YhteenvetoDto> haeKaynnissaOlevienYhteenvedotHaulle(String hakuOid);

  /** Yhteenvedot kaynnissa olevien laskentojen kulusta */
  Collection<YhteenvetoDto> haeYhteenvetoKaikilleLaskennoille(Instant luotuAlkaen);

  TunnisteDto luoLaskenta(
      String userOID,
      String haunnimi,
      String nimi,
      String hakuOid,
      LaskentaTyyppi tyyppi,
      Boolean erillishaku,
      Integer valinnanvaihe,
      Boolean valintakoelaskenta,
      Collection<HakukohdeDto> hakukohdeOids);

  void poistaLaskenta(String uuid);

  YhteenvetoDto merkkaaTila(String uuid, String hakukohdeOid, HakukohdeTila tila);

  YhteenvetoDto merkkaaTila(
      String uuid, String hakukohdeOid, HakukohdeTila tila, IlmoitusDto ilmoitus);

  YhteenvetoDto lisaaIlmoitus(String uuid, String hakukohdeOid, IlmoitusDto ilmoitus);

  YhteenvetoDto merkkaaTila(String uuid, LaskentaTila tila, Optional<IlmoitusDto> ilmoitus);

  YhteenvetoDto merkkaaTila(
      String uuid, LaskentaTila tila, HakukohdeTila hakukohdeTila, Optional<IlmoitusDto> ilmoitus);

  LaskentaDto resetoiEiValmiitHakukohteet(String uuid, boolean nollaaIlmoitukset);

  void resetoiMeneillaanOlevatLaskennat();

  Optional<String> otaSeuraavaLaskentaTyonAlle();
}
