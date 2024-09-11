package fi.vm.sade.valinta.kooste.external.resource.ataru;

import fi.vm.sade.valinta.kooste.external.resource.ataru.dto.AtaruHakemusPrototyyppi;
import fi.vm.sade.valinta.kooste.external.resource.ataru.dto.AtaruSyntheticApplicationResponse;
import fi.vm.sade.valinta.kooste.util.HakemusWrapper;
import io.reactivex.Observable;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface AtaruAsyncResource {
  CompletableFuture<List<HakemusWrapper>> getApplicationsByHakukohde(String hakukohdeOid);

  CompletableFuture<List<HakemusWrapper>> getApplicationsByHakukohde(
      String hakukohdeOid, boolean withHarkinnanvaraisuustieto);

  CompletableFuture<List<HakemusWrapper>> getApplicationsByOids(List<String> oids);

  CompletableFuture<List<HakemusWrapper>> getApplicationsByOidsWithHarkinnanvaraisuustieto(
      List<String> oids);

  Observable<List<AtaruSyntheticApplicationResponse>> putApplicationPrototypes(
      Collection<AtaruHakemusPrototyyppi> hakemusPrototyypit);
}
