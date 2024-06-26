package fi.vm.sade.valintalaskenta.domain.dto.seuranta;

import java.util.List;

public class LaskentaDto {
  private final String uuid;
  private final String hakuOid;
  private final String userOID;
  private final String haunnimi;
  private final String nimi;
  private final long luotu;
  private final Boolean erillishaku;
  private final LaskentaTila tila;
  private final LaskentaTyyppi tyyppi;
  private final List<HakukohdeDto> hakukohteet;
  private final IlmoitusDto ilmoitus;
  private final Integer valinnanvaihe;
  private final Boolean valintakoelaskenta;
  private final Integer jonosija;
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
      Boolean erillishaku,
      Integer valinnanvaihe,
      Boolean valintakoelaskenta,
      Integer jonosija,
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

  public Integer getJonosija() {
    return jonosija;
  }

  public Boolean getErillishaku() {
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

  public YhteenvetoDto asYhteenveto() {
    int valmiit = 0;
    int keskeytetty = 0;
    if (hakukohteet == null) {
      return new YhteenvetoDto(
          uuid,
          userOID,
          haunnimi,
          nimi,
          hakuOid,
          luotu,
          tila,
          0,
          valmiit,
          keskeytetty,
          null,
          tyyppi,
          valinnanvaihe,
          valintakoelaskenta);
    }
    for (HakukohdeDto h : hakukohteet) {
      if (HakukohdeTila.KESKEYTETTY.equals(h.getTila())) {
        ++keskeytetty;
      } else if (HakukohdeTila.VALMIS.equals(h.getTila())) {
        ++valmiit;
      }
    }
    return new YhteenvetoDto(
        uuid,
        userOID,
        haunnimi,
        nimi,
        hakuOid,
        luotu,
        tila,
        hakukohteet.size(),
        valmiit,
        keskeytetty,
        jonosija,
        tyyppi,
        valinnanvaihe,
        valintakoelaskenta);
  }

  public String getUserOID() {
    return userOID;
  }

  public Integer getValinnanvaihe() {
    return valinnanvaihe;
  }

  public Boolean getValintakoelaskenta() {
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

  public Boolean isErillishaku() {
    return erillishaku;
  }
}
