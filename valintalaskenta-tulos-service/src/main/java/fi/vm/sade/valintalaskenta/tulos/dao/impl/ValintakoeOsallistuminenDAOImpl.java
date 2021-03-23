package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import com.mongodb.BasicDBObject;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.tulos.dao.ValintakoeOsallistuminenDAO;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import org.mongodb.morphia.Datastore;
import org.mongodb.morphia.query.Query;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Repository;

@Repository
public class ValintakoeOsallistuminenDAOImpl implements ValintakoeOsallistuminenDAO {
  @Qualifier("datastore2")
  @Autowired
  private Datastore datastore;

  @Override
  public ValintakoeOsallistuminen findByHakemusOid(String hakemusOid) {
    return datastore.find(ValintakoeOsallistuminen.class, "hakemusOid", hakemusOid).get();
  }

  @Override
  public List<ValintakoeOsallistuminen> findByHakutoive(String hakukohdeOid) {
    return datastore
        .find(ValintakoeOsallistuminen.class)
        .field("hakutoiveet.hakukohdeOid")
        .equal(hakukohdeOid)
        .asList();
  }

  @Override
  public List<ValintakoeOsallistuminen> findByHakutoiveet(List<String> hakukohdeOids) {
    return datastore
        .find(ValintakoeOsallistuminen.class)
        .field("hakutoiveet.hakukohdeOid")
        .in(hakukohdeOids)
        .asList();
  }

  @Override
  public List<ValintakoeOsallistuminen> findByHakijaOids(List<String> hakijaOids) {
    return datastore
      .find(ValintakoeOsallistuminen.class)
      .field("hakijaOid")
      .in(hakijaOids)
      .asList();
  }

  @Override
  public List<ValintakoeOsallistuminen> findByHakuAndOsallistuminen(
      String hakuOid, Osallistuminen osallistuminen) {
    return datastore
        .find(ValintakoeOsallistuminen.class)
        .field("hakuOid")
        .equal(hakuOid)
        .field("hakutoiveet.valinnanVaiheet.valintakokeet.osallistuminenTulos.osallistuminen")
        .equal(osallistuminen)
        .asList();
  }

  @Override
  public List<ValintakoeOsallistuminen> findAmmatillisenKielikoeOsallistumiset(Date since) {
    Query<ValintakoeOsallistuminen> findQuery =
        datastore
            .find(ValintakoeOsallistuminen.class)
            .disableValidation()
            .filter("createdAt >=", since)
            .filter(
                "hakutoiveet.valinnanVaiheet.valintakokeet",
                new BasicDBObject(
                    "$elemMatch",
                    new BasicDBObject(
                        "$and",
                        new BasicDBObject[] {
                          new BasicDBObject(
                              "valintakoeTunniste",
                              new BasicDBObject(
                                  "$in", Arrays.asList("kielikoe_fi", "kielikoe_sv"))),
                          new BasicDBObject(
                              "osallistuminenTulos.osallistuminen",
                              Osallistuminen.OSALLISTUU.name())
                        })));
    return findQuery.asList();
  }
}
