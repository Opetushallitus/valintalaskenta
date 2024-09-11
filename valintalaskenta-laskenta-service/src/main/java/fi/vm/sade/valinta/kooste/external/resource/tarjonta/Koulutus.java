package fi.vm.sade.valinta.kooste.external.resource.tarjonta;

import fi.vm.sade.tarjonta.service.resources.v1.dto.koulutus.KomoV1RDTO;
import fi.vm.sade.valinta.kooste.external.resource.tarjonta.dto.KoutaKoulutus;
import java.util.List;

public class Koulutus {
  public final String oid;
  public final List<String> koulutusUrit;

  public Koulutus(String oid, String koulutusUri) {
    this.oid = oid;
    this.koulutusUrit = List.of(koulutusUri);
  }

  public Koulutus(KomoV1RDTO dto) {
    this.oid = dto.getOid();
    this.koulutusUrit = null;
  }

  public Koulutus(KoutaKoulutus dto) {
    this.oid = dto.oid;
    this.koulutusUrit = dto.koulutusKoodiUrit;
  }
}
