package fi.vm.sade.valintalaskenta.laskenta.resource.domain;

import com.google.common.collect.ComparisonChain;
import com.google.common.hash.HashCode;
import com.google.common.hash.Hashing;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.*;
import java.nio.charset.Charset;
import java.util.*;
import java.util.function.BiFunction;
import java.util.stream.Collectors;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Laskenta {
  private static final Logger LOG = LoggerFactory.getLogger(Laskenta.class);

  private UUID uuid;
  private final String haunnimi;
  private final String nimi;
  private final String hakuOid;
  private final Date luotu;
  private final LaskentaTila tila;
  private final LaskentaTyyppi tyyppi;
  private final Collection<HakukohdeDto> hakukohteet;
  private final IlmoitusDto ilmoitus;
  private final Integer valinnanvaihe;
  private final Boolean valintakoelaskenta;
  private final Boolean erillishaku;
  private final String userOID;
  private final String identityHash;

  /*
    public Laskenta() {
      this.userOID = null;
      this.haunnimi = null;
      this.nimi = null;
      this.hakukohteitaYhteensa = 0;
      this.hakukohteitaTekematta = 0;
      this.hakukohteitaOhitettu = 0;
      this.uuid = null;
      this.hakuOid = null;
      this.tila = null;
      this.ilmoitukset = null;
      this.luotu = null;
      this.valmiit = null;
      this.ohitettu = null;
      this.tekematta = null;
      this.tyyppi = null;
      this.valinnanvaihe = null;
      this.ilmoitus = null;
      this.valintakoelaskenta = null;
      this.hakukohdeOidJaOrganisaatioOids = null;
      this.hakukohteet = null;
      this.erillishaku = null;
      this.identityHash = null;
    }
  */

  public Laskenta(
      UUID uuid,
      String userOID,
      String haunnimi,
      String nimi,
      String hakuOid,
      Date luotu,
      LaskentaTyyppi tyyppi,
      Boolean erillishaku,
      Integer valinnanvaihe,
      Boolean valintakoelaskenta,
      LaskentaTila laskentaTila,
      Collection<HakukohdeDto> hakukohdeOids,
      IlmoitusDto ilmoitus) {
    this.haunnimi = haunnimi;
    this.nimi = nimi;
    this.uuid = uuid;
    this.userOID = userOID;
    this.hakuOid = hakuOid;
    this.luotu = luotu;
    this.tila = laskentaTila;
    this.ilmoitus = ilmoitus;
    this.hakukohteet = hakukohdeOids;
    this.tyyppi = tyyppi;
    this.erillishaku = erillishaku;
    this.valinnanvaihe = valinnanvaihe;
    this.valintakoelaskenta = valintakoelaskenta;
    this.identityHash = createIdentityHash().toString();
  }

  public String getIdentityHash() {
    return identityHash;
  }

  public String getHaunnimi() {
    return haunnimi;
  }

  public String getNimi() {
    return nimi;
  }

  private HashCode createIdentityHash() {
    /*
    private final String hakuOid;
    private final LaskentaTyyppi tyyppi;
    private final int hakukohteitaYhteensa;
    private final List<HakukohdeJaOrganisaatio> hakukohdeOidJaOrganisaatioOids;
    private final Integer valinnanvaihe;
    private final Boolean valintakoelaskenta;
    private final Boolean erillishaku;
    */
    final long DELIMETER = 1000000000L;
    return Hashing.md5()
        .newHasher()
        .putString(hakuOid, Charset.forName("UTF-8"))
        .putLong(DELIMETER + 1L)
        .putInt(tyyppi != null ? tyyppi.ordinal() : -1)
        .putLong(DELIMETER + 2L)
        .putInt(this.hakukohteet.size())
        .putLong(DELIMETER + 3L)
        .putInt(valinnanvaihe != null ? valinnanvaihe : -1)
        .putLong(DELIMETER + 4L)
        .putBoolean(Boolean.TRUE.equals(valintakoelaskenta))
        .putLong(DELIMETER + 5L)
        .putBoolean(Boolean.TRUE.equals(erillishaku))
        .putLong(DELIMETER + 6L)
        .putObject(
            this.hakukohteet,
            (oids, sink) -> {
              Optional.ofNullable(oids).orElse(Collections.emptyList()).stream()
                  .sorted(
                      (h1, h2) ->
                          ComparisonChain.start()
                              .compare(h1.getHakukohdeOid(), h2.getHakukohdeOid())
                              .compare(h1.getOrganisaatioOid(), h2.getOrganisaatioOid())
                              .result())
                  .forEach(
                      h -> {
                        sink.putString(h.getHakukohdeOid(), Charset.forName("UTF-8"))
                            .putLong(DELIMETER + 7L)
                            .putString(h.getOrganisaatioOid(), Charset.forName("UTF-8"))
                            .putLong(DELIMETER + 8L);
                      });
            })
        .hash();
  }

  public List<String> getOhitettu() {
    return this.hakukohteet.stream()
        .filter(hk -> HakukohdeTila.KESKEYTETTY.equals(hk.getTila()))
        .map(hk -> hk.getHakukohdeOid())
        .collect(Collectors.toList());
  }

  public List<String> getValmiit() {
    return this.hakukohteet.stream()
        .filter(hk -> HakukohdeTila.VALMIS.equals(hk.getTila()))
        .map(hk -> hk.getHakukohdeOid())
        .collect(Collectors.toList());
  }

  public List<String> getTekematta() {
    return this.hakukohteet.stream()
        .filter(hk -> HakukohdeTila.TEKEMATTA.equals(hk.getTila()))
        .map(hk -> hk.getHakukohdeOid())
        .collect(Collectors.toList());
  }

  public LaskentaTyyppi getTyyppi() {
    return tyyppi;
  }

  public Boolean getErillishaku() {
    return erillishaku;
  }

  public int getHakukohteitaOhitettu() {
    return (int)
        this.hakukohteet.stream()
            .filter(hk -> HakukohdeTila.KESKEYTETTY.equals(hk.getTila()))
            .count();
  }

  public int getHakukohteitaTekematta() {
    return (int)
        this.hakukohteet.stream()
            .filter(hk -> HakukohdeTila.TEKEMATTA.equals(hk.getTila()))
            .count();
  }

  public int getHakukohteitaYhteensa() {
    return this.hakukohteet.size();
  }

  public IlmoitusDto getIlmoitus() {
    return ilmoitus;
  }

  public String getHakuOid() {
    return hakuOid;
  }

  public Date getLuotu() {
    return luotu;
  }

  public LaskentaTila getTila() {
    return tila;
  }

  public UUID getUuid() {
    return uuid;
  }

  public Integer getValinnanvaihe() {
    return valinnanvaihe;
  }

  public Boolean getValintakoelaskenta() {
    return valintakoelaskenta;
  }

  public LaskentaDto asDto(
      BiFunction<Date, LaskentaTila, Integer> jonosijaProvider, boolean luotiinkoUusiLaskenta) {
    try {
      return new LaskentaDto(
          getUuid().toString(),
          userOID,
          haunnimi,
          nimi,
          getHakuOid(),
          luotu == null ? new Date().getTime() : luotu.getTime(),
          getTila(),
          getTyyppi(),
          Optional.ofNullable(ilmoitus).orElse(null),
          this.hakukohteet.stream().collect(Collectors.toList()),
          erillishaku,
          valinnanvaihe,
          valintakoelaskenta,
          jonosijaProvider.apply(luotu, getTila()),
          luotiinkoUusiLaskenta);
    } catch (Exception e) {
      LOG.error("LaskentaDto:n muodostus Laskentaentiteetista epaonnistui!", e);
      throw e;
    }
  }

  public String getUserOID() {
    return userOID;
  }
}
