package fi.vm.sade.valintalaskenta.laskenta.dao.impl;

import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValintakoeOsallistuminenDAO;
import java.util.Iterator;
import java.util.List;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Repository;

@Repository("ValintakoeOsallistuminenDAO")
public class ValintakoeOsallistuminenDAOImpl implements ValintakoeOsallistuminenDAO {
  @Value("${valintalaskenta-laskenta-service.mongodb.useIndexQueries:false}")
  private boolean useIndexQueries;

  @Override
  public List<ValintakoeOsallistuminen> readAll() {
    return null; // return morphiaDS.find(ValintakoeOsallistuminen.class).asList();
  }

  @Override
  public ValintakoeOsallistuminen readByHakuOidAndHakemusOid(String hakuOid, String hakemusOid) {
    return null;
    /*    return morphiaDS
    .find(ValintakoeOsallistuminen.class, "hakuOid", hakuOid)
    .filter("hakemusOid", hakemusOid)
    .get();*/
  }

  @Override
  public void createOrUpdate(ValintakoeOsallistuminen v) {
    /*morphiaDS.save(v);*/
  }

  @Override
  public ValintakoeOsallistuminen haeEdeltavaValinnanvaihe(
      String hakuOid, String hakukohdeOid, int jarjestysnumero) {
    if (jarjestysnumero == 0) return null;
    ValintakoeOsallistuminen edellinen = null;
    final Iterator<ValintakoeOsallistuminen> lasketutEdellisenVaiheenOsallistumiset =
        lasketutValintakoeOsallistumiset(hakuOid, hakukohdeOid, jarjestysnumero);
    if (lasketutEdellisenVaiheenOsallistumiset.hasNext()) {
      edellinen = lasketutEdellisenVaiheenOsallistumiset.next();
    } else {
      final Iterator<ValintakoeOsallistuminen> hakijanValintaEdellisenVaiheenOsallistumiset =
          hakijanValintaValintakoeOsallistumiset(hakuOid, hakukohdeOid, jarjestysnumero);
      if (hakijanValintaEdellisenVaiheenOsallistumiset.hasNext()) {
        edellinen = hakijanValintaEdellisenVaiheenOsallistumiset.next();
      }
    }
    return edellinen;
  }

  // Olemassaolevat laskennat (kevat 2015) vaatii tämän, uudet laskennat eivät.
  private Iterator<ValintakoeOsallistuminen> lasketutValintakoeOsallistumiset(
      String hakuOid, String hakukohdeOid, int jarjestysnumero) {
    return null;
    /*    final Query<ValintakoeOsallistuminen> query =
        morphiaDS.createQuery(ValintakoeOsallistuminen.class);
    query
        .field("hakuOid")
        .equal(hakuOid)
        .field("hakutoiveet.hakukohdeOid")
        .equal(hakukohdeOid)
        .field("hakutoiveet.valinnanVaiheet.valinnanVaiheJarjestysluku")
        .equal(jarjestysnumero - 1);
    return morphiaDS
        .<ValintakoeOsallistuminen, ValintakoeOsallistuminen>createAggregation(
            ValintakoeOsallistuminen.class)
        .match(query)
        .project(
            Projection.projection("_id").suppress(),
            Projection.projection("hakutoiveet"),
            Projection.projection("hakuOid"))
        .unwind("hakutoiveet")
        .match(query)
        .unwind("hakutoiveet.valinnanVaiheet")
        .match(query)
        .limit(1)
        .aggregate(ValintakoeOsallistuminen.class);*/
  }

  private Iterator<ValintakoeOsallistuminen> hakijanValintaValintakoeOsallistumiset(
      String hakuOid, String hakukohdeOid, int jarjestysnumero) {
    return null;
    /*    final Query<ValintakoeOsallistuminen> query =
        morphiaDS.createQuery(ValintakoeOsallistuminen.class);
    query
        .field("hakuOid")
        .equal(hakuOid)
        .field("hakutoiveet.laskettavaHakukohdeOid")
        .equal(hakukohdeOid)
        .field("hakutoiveet.valinnanVaiheet.laskettavaJarjestysluku")
        .equal(jarjestysnumero - 1);
    return morphiaDS
        .<ValintakoeOsallistuminen, ValintakoeOsallistuminen>createAggregation(
            ValintakoeOsallistuminen.class)
        .match(query)
        .project(
            Projection.projection("_id").suppress(),
            Projection.projection("hakutoiveet"),
            Projection.projection("hakuOid"))
        .unwind("hakutoiveet")
        .match(query)
        .unwind("hakutoiveet.valinnanVaiheet")
        .match(query)
        .limit(1)
        .aggregate(ValintakoeOsallistuminen.class);*/
  }
}
