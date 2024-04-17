package fi.vm.sade.valintalaskenta.laskenta.dao;

import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.valinta.ValinnanvaiheLite;
import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;
import java.util.List;
import java.util.Optional;

public interface ValinnanvaiheDAO {

  Valinnanvaihe haeEdeltavaValinnanvaihe(String hakuOid, String hakukohdeOid, int jarjestysnumero);

  Valinnanvaihe haeViimeisinValinnanvaihe(String hakuOid, String hakukohdeOid, int jarjestysnumero);

  Valinnanvaihe haeValinnanvaihe(String valinnanvaiheOid);

  Optional<ValinnanvaiheLite> haeValinnanvaiheLite(String valinnanvaiheOid);

  List<Valinnanvaihe> haeValinnanvaiheetJarjestysnumerolla(
      String hakuOid, String hakukohdeOid, int jarjestysnumero);

  void saveOrUpdate(Valinnanvaihe valinnanvaihe);

  void poistaValinnanvaihe(Valinnanvaihe valinnanvaihe);

  void poistaJono(Valintatapajono jono);
}
