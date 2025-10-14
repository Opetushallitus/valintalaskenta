package fi.vm.sade.valintalaskenta.laskenta.resource.external;

import java.util.List;

public interface AtaruResource {
  List<AtaruHakemus> getAtaruHakemukset(String hakuOid, String hakukohdeOids);
}
