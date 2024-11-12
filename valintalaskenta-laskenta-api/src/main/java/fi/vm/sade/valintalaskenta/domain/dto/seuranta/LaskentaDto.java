package fi.vm.sade.valintalaskenta.domain.dto.seuranta;

import java.util.List;
import java.util.Optional;

public class LaskentaDto {
  private final String uuid;
  private final String hakuOid;
  private final String userOID;
  private final String haunnimi;
  private final String nimi;
  private final long luotu;
  private final boolean erillishaku;
  private final LaskentaTila tila;
  private final LaskentaTyyppi tyyppi;
  private final List<HakukohdeDto> hakukohteet;
  private final IlmoitusDto ilmoitus;
  private final Optional<Integer> valinnanvaihe;
  private final boolean valintakoelaskenta;
  private final Optional<Integer> jonosija;
  private final boolean luotiinkoUusiLaskenta;

  public LaskentaDto(
      String uuid,
      String userOID,
      String haunnimi,
      String nimi,
      String hakuOid,
      long luotu,
      LaskentaTila tila,
      LaskentaTyyppi tyyppi,
      IlmoitusDto ilmoitus,
      List<HakukohdeDto> hakukohteet,
      boolean erillishaku,
      Optional<Integer> valinnanvaihe,
      boolean valintakoelaskenta,
      Optional<Integer> jonosija,
      boolean luotiinkoUusiLaskenta) {
    this.haunnimi = haunnimi;
    this.nimi = nimi;
    this.jonosija = jonosija;
    this.userOID = userOID;
    this.uuid = uuid;
    this.hakuOid = hakuOid;
    this.luotu = luotu;
    this.tyyppi = tyyppi;
    this.ilmoitus = ilmoitus;
    this.tila = tila;
    this.hakukohteet = hakukohteet;
    this.erillishaku = erillishaku;
    this.valinnanvaihe = valinnanvaihe;
    this.valintakoelaskenta = valintakoelaskenta;
    this.luotiinkoUusiLaskenta = luotiinkoUusiLaskenta;
  }

  public boolean getLuotiinkoUusiLaskenta() {
    return luotiinkoUusiLaskenta;
  }

  public boolean isLuotiinkoUusiLaskenta() {
    return luotiinkoUusiLaskenta;
  }

  public Optional<Integer> getJonosija() {
    return jonosija;
  }

  public boolean getErillishaku() {
    return erillishaku;
  }

  public String getHaunnimi() {
    return haunnimi;
  }

  public String getNimi() {
    return nimi;
  }

  public IlmoitusDto getIlmoitus() {
    return ilmoitus;
  }

  public String getUserOID() {
    return userOID;
  }

  public Optional<Integer> getValinnanvaihe() {
    return valinnanvaihe;
  }

  public boolean getValintakoelaskenta() {
    return valintakoelaskenta;
  }

  public LaskentaTila getTila() {
    return tila;
  }

  public List<HakukohdeDto> getHakukohteet() {
    return hakukohteet;
  }

  public String getHakuOid() {
    return hakuOid;
  }

  public Long getLuotu() {
    return luotu;
  }

  public String getUuid() {
    return uuid;
  }

  public LaskentaTyyppi getTyyppi() {
    return tyyppi;
  }

  public boolean isErillishaku() {
    return erillishaku;
  }
}
