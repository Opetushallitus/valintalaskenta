package fi.vm.sade.valinta.kooste.valintalaskenta.dto;

import fi.vm.sade.valinta.kooste.AuditSession;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaTyyppi;
import java.util.Collection;

public class LaskentaStartParams {
  private static final Integer HAE_KAIKKI_VALINNANVAIHEET = -1;

  private static final String NIMI_FORMAT = "Laskenta hakuOid(%s) uuid(%s) hakukohteita(%d)";
  private final String uuid;
  private final String hakuOid;
  private final boolean osittainenLaskenta;
  private final Integer valinnanvaihe;
  private final boolean erillishaku;
  private final Boolean valintakoelaskenta;
  private final boolean valintaryhmalaskenta;
  private final LaskentaTyyppi tyyppi;
  private final AuditSession auditSession;

  public LaskentaStartParams(
      AuditSession auditSession,
      String uuid,
      String hakuOid,
      boolean erillishaku,
      boolean osittainenLaskenta,
      boolean valintaryhmalaskenta,
      Integer valinnanvaihe,
      Boolean valintakoelaskenta,
      LaskentaTyyppi tyyppi) {
    this.auditSession = auditSession;
    this.uuid = uuid;
    this.hakuOid = hakuOid;
    this.osittainenLaskenta = osittainenLaskenta;
    this.valintaryhmalaskenta = valintaryhmalaskenta;
    this.valinnanvaihe = valinnanvaihe;
    this.valintakoelaskenta = valintakoelaskenta;
    this.tyyppi = tyyppi;
    this.erillishaku = erillishaku;
  }

  public AuditSession getAuditSession() {
    return auditSession;
  }

  public LaskentaTyyppi getTyyppi() {
    return tyyppi;
  }

  /** Tilapainen workaround resurssin valinnanvaiheen normalisointiin. */
  public Integer getValinnanvaihe() {
    return HAE_KAIKKI_VALINNANVAIHEET.equals(this.valinnanvaihe)
        ? null
        : this.valinnanvaihe;
  }

  public Boolean getValintakoelaskenta() {
    return valintakoelaskenta;
  }

  public boolean isOsittainenLaskenta() {
    return osittainenLaskenta;
  }

  public boolean isValintaryhmaLaskenta() {
    return valintaryhmalaskenta;
  }

  public boolean isErillishaku() {
    return erillishaku;
  }

  public boolean isValintaryhmalaskenta() {
    return valintaryhmalaskenta;
  }

  public String getHakuOid() {
    return hakuOid;
  }

  public String getUuid() {
    return uuid;
  }
}
