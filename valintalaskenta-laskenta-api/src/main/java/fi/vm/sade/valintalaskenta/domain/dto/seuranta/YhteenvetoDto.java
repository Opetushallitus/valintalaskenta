package fi.vm.sade.valintalaskenta.domain.dto.seuranta;

public class YhteenvetoDto {
  private final String uuid;
  private final String hakuOid;
  private final String userOID;
  private final String haunnimi;
  private final String nimi;
  private final long luotu;
  private final LaskentaTila tila;
  private final int hakukohteitaYhteensa;
  private final int hakukohteitaValmiina;
  private final int hakukohteitaKeskeytetty;
  private final Integer jonosija; // sija laskennan jonossa tai null jos ei jonossa
  private final LaskentaTyyppi tyyppi;
  private final Integer valinnanvaihe;
  private final Boolean valintakoelaskenta;

  public YhteenvetoDto(
      String uuid,
      String userOID,
      String haunnimi,
      String nimi,
      String hakuOid,
      Long luotu,
      LaskentaTila tila,
      int hakukohteitaYhteensa,
      int hakukohteitaValmiina,
      int hakukohteitaKeskeytetty,
      Integer jonosija,
      LaskentaTyyppi tyyppi,
      Integer valinnanvaihe,
      Boolean valintakoelaskenta) {
    this.userOID = userOID;
    this.haunnimi = haunnimi;
    this.nimi = nimi;
    this.uuid = uuid;
    this.hakuOid = hakuOid;
    this.luotu = luotu;
    this.tila = tila;
    this.hakukohteitaYhteensa = hakukohteitaYhteensa;
    this.hakukohteitaValmiina = hakukohteitaValmiina;
    this.hakukohteitaKeskeytetty = hakukohteitaKeskeytetty;
    this.jonosija = jonosija;
    this.tyyppi = tyyppi;
    this.valinnanvaihe = valinnanvaihe;
    this.valintakoelaskenta = valintakoelaskenta;
  }

  public String getHaunnimi() {
    return haunnimi;
  }

  public String getNimi() {
    return nimi;
  }

  public String getUserOID() {
    return userOID;
  }

  public Integer getJonosija() {
    return jonosija;
  }

  public String getHakuOid() {
    return hakuOid;
  }

  public int getHakukohteitaKeskeytetty() {
    return hakukohteitaKeskeytetty;
  }

  public int getHakukohteitaValmiina() {
    return hakukohteitaValmiina;
  }

  public int getHakukohteitaYhteensa() {
    return hakukohteitaYhteensa;
  }

  public long getLuotu() {
    return luotu;
  }

  public String getUuid() {
    return uuid;
  }

  public LaskentaTila getTila() {
    return tila;
  }

  public LaskentaTyyppi getTyyppi() {
    return tyyppi;
  }

  public Integer getValinnanvaihe() {
    return valinnanvaihe;
  }

  public Boolean getValintakoelaskenta() {
    return valintakoelaskenta;
  }
}
