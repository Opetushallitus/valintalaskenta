package fi.vm.sade.valintalaskenta.laskenta.resource.external;

import java.util.List;

public interface HakuAppResource {

  public List<HakuAppHakemus> getApplicationsByOids(String hakuOid, String hakukohdeOid);
}
