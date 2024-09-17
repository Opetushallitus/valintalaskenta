package fi.vm.sade.valinta.kooste.valintalaskenta.resource;

import fi.vm.sade.valinta.kooste.valintalaskenta.dto.Maski;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaTyyppi;
import java.util.Optional;
import org.apache.commons.lang.StringUtils;

public class LaskentaParams {
  private final String userOID;
  private final String haunNimi;
  private final String nimi;
  private final LaskentaTyyppi laskentatyyppi;
  private final Boolean isValintakoelaskenta;
  private final Integer valinnanvaihe;
  private final String hakuOid;
  private final Optional<Maski> maski;
  private final boolean isErillishaku;

  /**
   *
   * @param userOID               haun käynnistäneen käyttäjän tunniste, ei vaikuta laskentaan mutta näytetään
   *                              valintalaskennan hallinnassa
   * @param haunNimi              haun nimi, ei vaikuta laskentaan mutta näytetään valintalaskennan hallinnassa
   * @param nimi                  laskennan nimi, ei vaikuta laskentaan mutta näytetään valintalaskennan hallinnassa
   * @param laskentatyyppi        määrittää laskennan tyypin (haku, hakukohde, valintaryhmä)
   * @param isValintakoelaskenta  valintakoelaskentaa käytetään selvittämään ketkä hakijat tulevat kutsutuksi
   *                              valintakokeeseen
   * @param valinnanvaihe         määrittää että laskennassa lasketaan vain tietty valinnan vaihe
   * @param hakuOid               laskettavan haun tunniste
   * @param maski                 maskitoiminnin avulla laskenta voidaan rajoittaa tiettyihin hakukohteisiin joko
   *                              white- tai blacklistillä
   * @param isErillishaku         erillishakutoiminnolla haun päätteeksi suoritetaan sijoittelu kunkin hakukohteen
   *                              sisällä, ts. ei kaikkien haun hakukohteiden yli
   */
  public LaskentaParams(
      String userOID,
      String haunNimi,
      String nimi,
      LaskentaTyyppi laskentatyyppi,
      Boolean isValintakoelaskenta,
      Integer valinnanvaihe,
      String hakuOid,
      Optional<Maski> maski,
      boolean isErillishaku) {
    this.userOID = userOID;
    this.haunNimi = haunNimi;
    this.nimi = nimi;
    this.laskentatyyppi = laskentatyyppi;
    this.isValintakoelaskenta = isValintakoelaskenta;
    this.valinnanvaihe = valinnanvaihe;

    if (StringUtils.isBlank(hakuOid)) {
      throw new RuntimeException("HakuOid on pakollinen");
    }
    this.hakuOid = hakuOid;

    if (maski == null) {
      throw new RuntimeException("maski on pakollinen");
    }
    this.maski = maski;
    this.isErillishaku = isErillishaku;
  }

  public String getHaunNimi() {
    return haunNimi;
  }

  public String getNimi() {
    return nimi;
  }

  public String getUserOID() {
    return userOID;
  }

  public LaskentaTyyppi getLaskentatyyppi() {
    return laskentatyyppi;
  }

  public Boolean getIsValintakoelaskenta() {
    return isValintakoelaskenta;
  }

  public Integer getValinnanvaihe() {
    return valinnanvaihe;
  }

  public String getHakuOid() {
    return hakuOid;
  }

  public Optional<Maski> getMaski() {
    return maski;
  }

  public boolean isErillishaku() {
    return isErillishaku;
  }
}
