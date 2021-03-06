package fi.vm.sade.valintalaskenta.tulos.dao;

import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.valinta.ValintatapajonoMigrationDTO;
import java.util.List;
import java.util.stream.Stream;
import org.apache.commons.lang3.tuple.Pair;
import org.mongodb.morphia.Key;

public interface ValinnanvaiheDAO {
  List<Valinnanvaihe> readByHakukohdeOid(String hakukohdeoid);

  List<Valinnanvaihe> readByHakuOid(String hakuoid);

  Stream<Valinnanvaihe> readByHakuOidStreaming(String hakuoid);

  List<ValintatapajonoMigrationDTO> valintatapajonotJotkaEivatKaytaLaskentaa();

  List<Pair<String, String>> hakuOidHakukohdeOidPairsForJonos(
      List<ValintatapajonoMigrationDTO> validValintatapajonos);

  List<Valinnanvaihe> readByHakuOidAndHakemusOid(String hakuOid, String hakemusOid);

  Valinnanvaihe findByValintatapajonoOid(String valintatapajonoOid);

  Valinnanvaihe haeValinnanvaihe(String valinnanvaiheOid);

  void saveOrUpdate(Valinnanvaihe vaihe);

  Key<Valinnanvaihe> saveVaihe(Valinnanvaihe vaihe);
}
