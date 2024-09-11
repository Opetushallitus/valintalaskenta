package fi.vm.sade.valinta.kooste.valintalaskenta.actor;

import fi.vm.sade.valinta.kooste.valintalaskenta.dto.Laskenta;
import fi.vm.sade.valinta.kooste.valintalaskenta.dto.LaskentaStartParams;

public class LaskentaActorWrapper implements Laskenta {

  private final String uuid;
  private final String hakuOid;
  private final boolean osittainen;
  private final LaskentaActor laskentaActor;

  public LaskentaActorWrapper(LaskentaStartParams params, LaskentaActor laskentaActor) {
    this.uuid = params.getUuid();
    this.hakuOid = params.getHakuOid();
    this.osittainen = params.isOsittainenLaskenta();
    this.laskentaActor = laskentaActor;
  }

  public LaskentaActor laskentaActor() {
    return laskentaActor;
  }

  public String getHakuOid() {
    return hakuOid;
  }

  public String getUuid() {
    return uuid;
  }

  public boolean isOsittainenLaskenta() {
    return osittainen;
  }

  public boolean isValmis() {
    return false;
  }

  public void lopeta() {
    laskentaActor.lopeta();
  }
}
