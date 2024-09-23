package fi.vm.sade.valinta.kooste.external.resource.koodisto;

import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import fi.vm.sade.valinta.kooste.external.resource.koodisto.dto.Koodi;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class KoodistoCachedAsyncResource {
  private final KoodistoAsyncResource koodistoAsyncResource;
  private final Cache<String, CompletableFuture<Koodi>> koodiCache =
      CacheBuilder.newBuilder().expireAfterAccess(7, TimeUnit.HOURS).build();

  @Autowired
  public KoodistoCachedAsyncResource(KoodistoAsyncResource koodistoAsyncResource) {
    this.koodistoAsyncResource = koodistoAsyncResource;
  }

  public CompletableFuture<Koodi> maatjavaltiot2ToMaatjavaltiot1(String koodiUri) {
    try {
      CompletableFuture<Koodi> f =
          this.koodiCache.get(
              koodiUri, () -> this.koodistoAsyncResource.maatjavaltiot2ToMaatjavaltiot1(koodiUri));
      if (f.isCompletedExceptionally()) {
        this.koodiCache.invalidate(koodiUri);
        return this.koodiCache.get(
            koodiUri, () -> this.koodistoAsyncResource.maatjavaltiot2ToMaatjavaltiot1(koodiUri));
      }
      return f;
    } catch (ExecutionException e) {
      return CompletableFuture.failedFuture(e);
    }
  }
}
