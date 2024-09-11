package fi.vm.sade.valinta.kooste.external.resource.hakuapp;

import fi.vm.sade.valinta.kooste.external.resource.hakuapp.dto.HakemusPrototyyppi;
import fi.vm.sade.valinta.kooste.util.HakemusWrapper;
import io.reactivex.Observable;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CompletableFuture;

public interface ApplicationAsyncResource {

  List<String> DEFAULT_KEYS =
      Arrays.asList(
          "applicationSystemId",
          "oid",
          "personOid",
          "answers.henkilotiedot",
          "answers.lisatiedot",
          "answers.hakutoiveet",
          "hakutapa",
          "maxApplicationOptions");
  List<String> DEFAULT_STATES = Arrays.asList("ACTIVE", "INCOMPLETE");
  int DEFAULT_ROW_LIMIT = 100000;

  CompletableFuture<List<HakemusWrapper>> getApplicationsByOid(String hakuOid, String hakukohdeOid);

  Observable<Set<String>> getApplicationOids(String hakuOid, String hakukohdeOid);

  CompletableFuture<List<HakemusWrapper>> getApplicationsByOids(
      String hakuOid, Collection<String> hakukohdeOids);

  CompletableFuture<List<HakemusWrapper>> getApplicationsByOidsWithPOST(
      String hakuOid, List<String> hakukohdeOids);

  Observable<List<HakemusWrapper>> getApplicationsByHakemusOids(List<String> hakemusOids);

  CompletableFuture<List<HakemusWrapper>> getApplicationsByhakemusOidsInParts(
      String hakuOid, List<String> hakemusOids, List<String> keys);

  Observable<List<HakemusWrapper>> putApplicationPrototypes(
      String hakuOid,
      String hakukohdeOid,
      String tarjoajaOid,
      Collection<HakemusPrototyyppi> hakemusPrototyypit);

  Observable<HakemusWrapper> getApplication(String hakemusOid);

  Observable<List<HakemusWrapper>> getApplicationsByOids(Collection<String> hakemusOids);

  Observable<String> changeStateOfApplicationsToPassive(List<String> hakemusOid, String reason);
}
