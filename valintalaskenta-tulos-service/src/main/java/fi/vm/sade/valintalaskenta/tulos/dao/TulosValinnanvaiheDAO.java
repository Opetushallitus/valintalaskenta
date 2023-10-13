package fi.vm.sade.valintalaskenta.tulos.dao;

import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;

import java.util.List;
import java.util.UUID;
import java.util.stream.Stream;

public interface TulosValinnanvaiheDAO {
  List<Valinnanvaihe> readByHakukohdeOid(String hakukohdeoid);

  List<Valinnanvaihe> readByHakuOid(String hakuoid);

  Stream<Valinnanvaihe> readByHakuOidStreaming(String hakuoid);

  List<Valinnanvaihe> readByHakuOidAndHakemusOid(String hakuOid, String hakemusOid);

  Valinnanvaihe findByValintatapajonoOid(String valintatapajonoOid);

  Valinnanvaihe haeValinnanvaihe(String valinnanvaiheOid);

  void saveOrUpdate(Valinnanvaihe vaihe);

  UUID saveVaihe(Valinnanvaihe vaihe);

  List<Valintatapajono> valintatapajonotJotkaEivatKaytaLaskentaa();
}
