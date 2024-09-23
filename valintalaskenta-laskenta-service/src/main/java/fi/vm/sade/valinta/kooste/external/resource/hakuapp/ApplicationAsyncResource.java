package fi.vm.sade.valinta.kooste.external.resource.hakuapp;

import fi.vm.sade.valinta.kooste.util.HakemusWrapper;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface ApplicationAsyncResource {

  List<String> DEFAULT_STATES = Arrays.asList("ACTIVE", "INCOMPLETE");
  int DEFAULT_ROW_LIMIT = 100000;

  CompletableFuture<List<HakemusWrapper>> getApplicationsByOids(
      String hakuOid, Collection<String> hakukohdeOids);
}
