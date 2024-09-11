package fi.vm.sade.valinta.kooste.proxy.resource.valintatulosservice;

import fi.vm.sade.valinta.kooste.external.resource.sijoittelu.ValintatulosUpdateStatus;
import java.util.List;

public class VastaanottoUpdateFailuresException extends RuntimeException {
  public final List<ValintatulosUpdateStatus> failedUpdateStatuses;

  public VastaanottoUpdateFailuresException(List<ValintatulosUpdateStatus> failedUpdateStatuses) {
    this.failedUpdateStatuses = failedUpdateStatuses;
  }
}
