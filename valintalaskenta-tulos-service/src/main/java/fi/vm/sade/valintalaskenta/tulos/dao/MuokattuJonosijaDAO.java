package fi.vm.sade.valintalaskenta.tulos.dao;

import fi.vm.sade.valintalaskenta.domain.valinta.MuokattuJonosija;
import java.util.List;

public interface MuokattuJonosijaDAO {
  MuokattuJonosija readByValintatapajonoOid(String valintatapajonoOid, String hakemusOid);

  void saveOrUpdate(MuokattuJonosija muokattuJonosija);

  List<MuokattuJonosija> readByHakuOid(String hakuOid);

  List<MuokattuJonosija> readByhakukohdeOid(String hakukohdeOid);

  List<MuokattuJonosija> readByHakuOidAndHakemusOid(String hakuOid, String hakemusOid);
}
