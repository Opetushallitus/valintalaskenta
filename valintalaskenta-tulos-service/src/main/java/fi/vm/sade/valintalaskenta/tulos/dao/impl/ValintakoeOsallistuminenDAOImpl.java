package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import static dev.morphia.query.filters.Filters.*;

import dev.morphia.Datastore;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.tulos.dao.ValintakoeOsallistuminenDAO;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;
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
    return datastore
        .find(ValintakoeOsallistuminen.class)
        .filter(eq("hakemusOid", hakemusOid))
        .first();
  }

  @Override
  public List<ValintakoeOsallistuminen> findByHakutoive(String hakukohdeOid) {
    return datastore
        .find(ValintakoeOsallistuminen.class)
        .filter(eq("hakutoiveet.hakukohdeOid", hakukohdeOid))
        .stream()
        .collect(Collectors.toList());
  }

  @Override
  public List<ValintakoeOsallistuminen> findByHakutoiveet(List<String> hakukohdeOids) {
    return datastore
        .find(ValintakoeOsallistuminen.class)
        .filter(in("hakutoiveet.hakukohdeOid", hakukohdeOids))
        .stream()
        .collect(Collectors.toList());
  }

  @Override
  public List<ValintakoeOsallistuminen> findByHakijaOids(List<String> hakijaOids) {
    return datastore
        .find(ValintakoeOsallistuminen.class)
        .filter(in("hakijaOid", hakijaOids))
        .stream()
        .collect(Collectors.toList());
  }

  @Override
  public List<ValintakoeOsallistuminen> findByHakuAndOsallistuminen(
      String hakuOid, Osallistuminen osallistuminen) {
    return datastore
        .find(ValintakoeOsallistuminen.class)
        .filter(
            and(
                eq("hakuOid", hakuOid),
                eq(
                    "hakutoiveet.valinnanVaiheet.valintakokeet.osallistuminenTulos.osallistuminen",
                    osallistuminen)))
        .stream()
        .collect(Collectors.toList());
  }

  @Override
  public List<ValintakoeOsallistuminen> findAmmatillisenKielikoeOsallistumiset(Date since) {
    return datastore
        .find(ValintakoeOsallistuminen.class)
        .disableValidation()
        .filter(
            and(
                gte("createdAt", since),
                elemMatch(
                    "hakutoiveet.valinnanVaiheet.valintakokeet",
                    and(
                        in("valintakoeTunniste", Arrays.asList("kielikoe_fi", "kielikoe_sv")),
                        eq(
                            "osallistuminenTulos.osallistuminen",
                            Osallistuminen.OSALLISTUU.name())))))
        .stream()
        .collect(Collectors.toList());
  }
}
