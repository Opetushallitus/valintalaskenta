package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.tulos.dao.TulosValintakoeOsallistuminenDAO;
import java.util.Date;
import java.util.List;

import fi.vm.sade.valintalaskenta.tulos.dao.repository.TulosValintakoeOsallistuminenRepository;
import org.springframework.stereotype.Repository;

@Repository
public class TulosValintakoeOsallistuminenDAOImpl implements TulosValintakoeOsallistuminenDAO {

  private final TulosValintakoeOsallistuminenRepository repo;

  public TulosValintakoeOsallistuminenDAOImpl(TulosValintakoeOsallistuminenRepository repo) {
    this.repo = repo;
  }


  @Override
  public ValintakoeOsallistuminen findByHakemusOid(String hakemusOid) {
    return repo.findValintakoeOsallistuminenByHakemusOid(hakemusOid).orElse(null);
  }

  @Override
  public List<ValintakoeOsallistuminen> findByHakutoive(String hakukohdeOid) {
    return repo.findValintakoeOsallistuminensByHakutoive(hakukohdeOid);
  }

  @Override
  public List<ValintakoeOsallistuminen> findByHakutoiveet(List<String> hakukohdeOids) {
    return repo.findValintakoeOsallistuminensByHakutoiveet(hakukohdeOids);
  }

  @Override
  public List<ValintakoeOsallistuminen> findByHakijaOids(List<String> hakijaOids) {
    return repo.findValintakoeOsallistuminensByHakijaOidIn(hakijaOids);
  }

  @Override
  public List<ValintakoeOsallistuminen> findByHakuAndOsallistuminen(
      String hakuOid, Osallistuminen osallistuminen) {
    return repo.findByHakuAndOsallistuminen(hakuOid, osallistuminen);
/*    return datastore
        .find(ValintakoeOsallistuminen.class)
        .field("hakuOid")
        .equal(hakuOid)
        .field("hakutoiveet.valinnanVaiheet.valintakokeet.osallistuminenTulos.osallistuminen")
        .equal(osallistuminen)
        .asList();*/
  }

  @Override
  public List<ValintakoeOsallistuminen> findAmmatillisenKielikoeOsallistumiset(Date since) {
    //tODO Is this used anywhere?? Remove if not
    return null;
/*    Query<ValintakoeOsallistuminen> findQuery =
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
    return findQuery.asList();*/
  }
}
