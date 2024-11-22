package fi.vm.sade.valintalaskenta.runner.service;

import fi.vm.sade.service.valintaperusteet.dto.HakukohdeViiteDTO;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.*;
import fi.vm.sade.valintalaskenta.runner.dto.Maski;
import java.util.*;

public interface LuoLaskentaService {

  /**
   * Käynnistää uuden laskennan
   *
   * @param userOID haun käynnistäneen käyttäjän tunniste, ei vaikuta laskentaan mutta näytetään
   *     valintalaskennan hallinnassa
   * @param haunNimi haun nimi, ei vaikuta laskentaan mutta näytetään valintalaskennan hallinnassa
   * @param nimi laskennan nimi, ei vaikuta laskentaan mutta näytetään valintalaskennan hallinnassa
   * @param laskentatyyppi määrittää laskennan tyypin (haku, hakukohde, valintaryhmä)
   * @param isValintakoelaskenta valintakoelaskentaa käytetään selvittämään ketkä hakijat tulevat
   *     kutsutuksi valintakokeeseen
   * @param valinnanvaihe määrittää että laskennassa lasketaan vain tietty valinnan vaihe
   * @param hakuOid laskettavan haun tunniste
   * @param maski maskitoiminnin avulla laskenta voidaan rajoittaa tiettyihin hakukohteisiin joko
   *     white- tai blacklistillä
   * @param isErillishaku erillishakutoiminnolla haun päätteeksi suoritetaan sijoittelu kunkin
   *     hakukohteen sisällä, ts. ei kaikkien haun hakukohteiden yli
   * @param hakukohdeViitteet laskettavat hakukohteet
   */
  TunnisteDto kaynnistaLaskentaHaulle(
      String userOID,
      String haunNimi,
      String nimi,
      LaskentaTyyppi laskentatyyppi,
      boolean isValintakoelaskenta,
      Optional<Integer> valinnanvaihe,
      String hakuOid,
      Optional<Maski> maski,
      boolean isErillishaku,
      List<HakukohdeViiteDTO> hakukohdeViitteet);

  TunnisteDto kaynnistaLaskentaUudelleen(final String uuid);

  void peruutaLaskenta(String uuid, Boolean lopetaVainJonossaOlevaLaskenta);

  Optional<LaskentaDto> haeLaskenta(String uuid);
}
