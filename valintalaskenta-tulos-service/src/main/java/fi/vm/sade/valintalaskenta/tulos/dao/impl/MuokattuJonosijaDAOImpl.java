package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import static dev.morphia.query.filters.Filters.and;
import static dev.morphia.query.filters.Filters.eq;

import dev.morphia.Datastore;
import fi.vm.sade.valintalaskenta.domain.valinta.MuokattuJonosija;
import fi.vm.sade.valintalaskenta.tulos.dao.MuokattuJonosijaDAO;
import java.util.List;
import java.util.stream.Collectors;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

@Component
public class MuokattuJonosijaDAOImpl implements MuokattuJonosijaDAO {
  private static final Logger LOGGER = LoggerFactory.getLogger(MuokattuJonosijaDAOImpl.class);

  @Qualifier("datastore2")
  @Autowired
  private Datastore datastore;

  @Override
  public List<MuokattuJonosija> readByHakuOid(String hakuOid) {
    return datastore.find(MuokattuJonosija.class).filter(eq("hakuOid", hakuOid)).stream()
        .collect(Collectors.toList());
  }

  @Override
  public List<MuokattuJonosija> readByhakukohdeOid(String hakukohdeOid) {
    return datastore.find(MuokattuJonosija.class).filter(eq("hakukohdeOid", hakukohdeOid)).stream()
        .collect(Collectors.toList());
  }

  @Override
  public List<MuokattuJonosija> readByHakuOidAndHakemusOid(String hakuOid, String hakemusOid) {
    return datastore
        .find(MuokattuJonosija.class)
        .filter(and(eq("hakuOid", hakuOid), eq("hakemusOid", hakemusOid)))
        .stream()
        .collect(Collectors.toList());
  }

  @Override
  public MuokattuJonosija readByValintatapajonoOid(String valintatapajonoOid, String hakemusOid) {
    return datastore
        .find(MuokattuJonosija.class)
        .filter(and(eq("valintatapajonoOid", valintatapajonoOid), eq("hakemusOid", hakemusOid)))
        .first();
  }

  @Override
  public void saveOrUpdate(MuokattuJonosija muokattuJonosija) {
    datastore.save(muokattuJonosija);
  }
}
