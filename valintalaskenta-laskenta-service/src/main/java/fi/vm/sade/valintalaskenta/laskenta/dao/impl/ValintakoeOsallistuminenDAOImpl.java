package fi.vm.sade.valintalaskenta.laskenta.dao.impl;

import static dev.morphia.query.filters.Filters.and;
import static dev.morphia.query.filters.Filters.eq;

import dev.morphia.Datastore;
import dev.morphia.aggregation.stages.Projection;
import dev.morphia.aggregation.stages.Unwind;
import dev.morphia.query.filters.Filter;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValintakoeOsallistuminenDAO;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Repository;

@Repository("ValintakoeOsallistuminenDAO")
public class ValintakoeOsallistuminenDAOImpl implements ValintakoeOsallistuminenDAO {
  @Value("${valintalaskenta-laskenta-service.mongodb.useIndexQueries:false}")
  private boolean useIndexQueries;

  @Autowired private Datastore morphiaDS;

  @Override
  public List<ValintakoeOsallistuminen> readAll() {
    return morphiaDS.find(ValintakoeOsallistuminen.class).stream().collect(Collectors.toList());
  }

  @Override
  public ValintakoeOsallistuminen readByHakuOidAndHakemusOid(String hakuOid, String hakemusOid) {
    return morphiaDS
        .find(ValintakoeOsallistuminen.class)
        .filter(and(eq("hakuOid", hakuOid), eq("hakemusOid", hakemusOid)))
        .first();
  }

  @Override
  public void createOrUpdate(ValintakoeOsallistuminen v) {
    morphiaDS.save(v);
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
    final Filter filter =
        and(
            eq("hakuOid", hakuOid),
            eq("hakutoiveet.hakukohdeOid", hakukohdeOid),
            eq("hakutoiveet.valinnanVaiheet.valinnanVaiheJarjestysluku", jarjestysnumero - 1));
    return morphiaDS
        .aggregate(ValintakoeOsallistuminen.class)
        .match(filter)
        .project(Projection.project().exclude("_id").include("hakutoiveet").include("hakuOid"))
        .unwind(Unwind.unwind("hakutoiveet"))
        .match(filter)
        .unwind(Unwind.unwind("hakutoiveet.valinnanVaiheet"))
        .match(filter)
        .limit(1)
        .execute(ValintakoeOsallistuminen.class);
  }

  private Iterator<ValintakoeOsallistuminen> hakijanValintaValintakoeOsallistumiset(
      String hakuOid, String hakukohdeOid, int jarjestysnumero) {
    final Filter filter =
        and(
            eq("hakuOid", hakuOid),
            eq("hakutoiveet.laskettavaHakukohdeOid", hakukohdeOid),
            eq("hakutoiveet.valinnanVaiheet.laskettavaJarjestysluku", jarjestysnumero - 1));
    return morphiaDS
        .aggregate(ValintakoeOsallistuminen.class)
        .match(filter)
        .project(Projection.project().exclude("_id").include("hakutoiveet").include("hakuOid"))
        .unwind(Unwind.unwind("hakutoiveet"))
        .match(filter)
        .unwind(Unwind.unwind("hakutoiveet.valinnanVaiheet"))
        .match(filter)
        .limit(1)
        .execute(ValintakoeOsallistuminen.class);
  }
}
