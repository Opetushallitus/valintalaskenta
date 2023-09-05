package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import fi.vm.sade.valintalaskenta.domain.valinta.MuokattuJonosija;
import fi.vm.sade.valintalaskenta.tulos.dao.MuokattuJonosijaDAO;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Component
public class MuokattuJonosijaDAOImpl implements MuokattuJonosijaDAO {
  private static final Logger LOGGER = LoggerFactory.getLogger(MuokattuJonosijaDAOImpl.class);

  @Override
  public List<MuokattuJonosija> readByHakuOid(String hakuOid) {
    return null;//datastore.find(MuokattuJonosija.class).filter("hakuOid", hakuOid).asList();
  }

  @Override
  public List<MuokattuJonosija> readByhakukohdeOid(String hakukohdeOid) {
    return null;//return datastore.find(MuokattuJonosija.class).filter("hakukohdeOid", hakukohdeOid).asList();
  }

  @Override
  public List<MuokattuJonosija> readByHakuOidAndHakemusOid(String hakuOid, String hakemusOid) {
    return null;
/*    return datastore
        .find(MuokattuJonosija.class)
        .filter("hakuOid", hakuOid)
        .filter("hakemusOid", hakemusOid)
        .asList();*/
  }

  @Override
  public MuokattuJonosija readByValintatapajonoOid(String valintatapajonoOid, String hakemusOid) {
    return null;
/*    return datastore
        .find(MuokattuJonosija.class)
        .filter("valintatapajonoOid", valintatapajonoOid)
        .filter("hakemusOid", hakemusOid)
        .get();*/
  }

  @Override
  public void saveOrUpdate(MuokattuJonosija muokattuJonosija) {
    //datastore.save(muokattuJonosija);
  }
}
