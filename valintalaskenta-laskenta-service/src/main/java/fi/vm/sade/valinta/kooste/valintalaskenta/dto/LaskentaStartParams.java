package fi.vm.sade.valinta.kooste.valintalaskenta.dto;

import fi.vm.sade.valinta.kooste.external.resource.valintatulosservice.dto.AuditSession;
import fi.vm.sade.valinta.kooste.valintalaskenta.actor.dto.HakukohdeJaOrganisaatio;
import fi.vm.sade.valinta.seuranta.dto.LaskentaTyyppi;
import java.util.Collection;

public class LaskentaStartParams implements LaskentaInfo {
  private static final String NIMI_FORMAT = "Laskenta hakuOid(%s) uuid(%s) hakukohteita(%d)";
  private final String uuid;
  private final String hakuOid;
  private final boolean osittainenLaskenta;
  private final Integer valinnanvaihe;
  private final boolean erillishaku;
  private final Boolean valintakoelaskenta;
  private final boolean valintaryhmalaskenta;
  private final Collection<HakukohdeJaOrganisaatio> hakukohdeDtos;
  private final LaskentaTyyppi tyyppi;
  private final AuditSession auditSession;

  public LaskentaStartParams(
      AuditSession auditSession,
      String uuid,
      String hakuOid,
      boolean erillishaku,
      Integer valinnanvaihe,
      Boolean valintakoelaskenta,
      Collection<HakukohdeJaOrganisaatio> hakukohdeDtos,
      LaskentaTyyppi tyyppi) {
    this.auditSession = auditSession;
    this.uuid = uuid;
    this.hakuOid = hakuOid;
    this.osittainenLaskenta = false;
    this.valinnanvaihe = valinnanvaihe;
    this.valintakoelaskenta = valintakoelaskenta;
    this.hakukohdeDtos = hakukohdeDtos;
    this.valintaryhmalaskenta = false;
    this.tyyppi = tyyppi;
    this.erillishaku = erillishaku;
  }

  public LaskentaStartParams(
      AuditSession auditSession,
      String uuid,
      String hakuOid,
      boolean erillishaku,
      boolean osittainenLaskenta,
      boolean valintaryhmalaskenta,
      Integer valinnanvaihe,
      Boolean valintakoelaskenta,
      Collection<HakukohdeJaOrganisaatio> hakukohdeDtos,
      LaskentaTyyppi tyyppi) {
    this.auditSession = auditSession;
    this.uuid = uuid;
    this.hakuOid = hakuOid;
    this.osittainenLaskenta = osittainenLaskenta;
    this.valintaryhmalaskenta = valintaryhmalaskenta;
    this.valinnanvaihe = valinnanvaihe;
    this.valintakoelaskenta = valintakoelaskenta;
    this.hakukohdeDtos = hakukohdeDtos;
    this.tyyppi = tyyppi;
    this.erillishaku = erillishaku;
  }

  public AuditSession getAuditSession() {
    return auditSession;
  }

  public LaskentaTyyppi getTyyppi() {
    return tyyppi;
  }

  public Collection<HakukohdeJaOrganisaatio> getHakukohdeDtos() {
    return hakukohdeDtos;
  }

  public Integer getValinnanvaihe() {
    return valinnanvaihe;
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

  public String toString() {
    return String.format(NIMI_FORMAT, hakuOid, uuid, hakukohdeDtos.size());
  }
}
