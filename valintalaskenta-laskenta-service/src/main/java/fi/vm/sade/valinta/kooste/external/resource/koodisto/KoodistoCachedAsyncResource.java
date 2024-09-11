package fi.vm.sade.valinta.kooste.external.resource.koodisto;

import static java.util.concurrent.TimeUnit.MINUTES;

import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import fi.vm.sade.valinta.kooste.external.resource.koodisto.dto.Koodi;
import fi.vm.sade.valinta.kooste.external.resource.koodisto.dto.Metadata;
import fi.vm.sade.valinta.kooste.util.KieliUtil;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class KoodistoCachedAsyncResource {
  public static final String MAAT_JA_VALTIOT_1 = "maatjavaltiot1";
  public static final String MAAT_JA_VALTIOT_2 = "maatjavaltiot2";
  public static final String POSTI = "posti";
  public static final String KIELI = "kieli";
  public static final String KUNTA = "kunta";
  public static final String HYVAKSYNNAN_EHDOT = "hyvaksynnanehdot";
  public static final String PAINOTETTAVAT_OPPIAINEET_LUKIOSSA = "painotettavatoppiaineetlukiossa";
  public static final String VALINTAKOKEEN_TYYPPI = "valintakokeentyyppi";

  private final KoodistoAsyncResource koodistoAsyncResource;
  private final Cache<String, CompletableFuture<Map<String, Koodi>>> koodistoCache =
      CacheBuilder.newBuilder().expireAfterAccess(7, TimeUnit.HOURS).build();
  private final Cache<String, CompletableFuture<Koodi>> koodiCache =
      CacheBuilder.newBuilder().expireAfterAccess(7, TimeUnit.HOURS).build();
  private final Cache<String, CompletableFuture<List<Koodi>>> ylakooditCache =
      CacheBuilder.newBuilder().expireAfterAccess(7, TimeUnit.HOURS).build();
  private final Cache<String, CompletableFuture<List<Koodi>>> alakooditCache =
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

  public Map<String, Koodi> haeKoodisto(String koodistoUri) {
    try {
      return this.haeKoodistoAsync(koodistoUri).get(1, MINUTES);
    } catch (InterruptedException | ExecutionException | TimeoutException e) {
      throw new RuntimeException(e);
    }
  }

  public CompletableFuture<Map<String, Koodi>> haeKoodistoAsync(String koodistoUri) {
    try {
      CompletableFuture<Map<String, Koodi>> f =
          this.koodistoCache.get(koodistoUri, () -> fetchKoodisto(koodistoUri));
      if (f.isCompletedExceptionally()) {
        this.koodistoCache.invalidate(koodistoUri);
        return this.koodistoCache.get(koodistoUri, () -> fetchKoodisto(koodistoUri));
      }
      return f;
    } catch (ExecutionException e) {
      return CompletableFuture.failedFuture(e);
    }
  }

  private CompletableFuture<Map<String, Koodi>> fetchKoodisto(String koodistoUri) {
    return this.koodistoAsyncResource
        .haeKoodisto(koodistoUri)
        .thenApplyAsync(KoodistoCachedAsyncResource::konversio);
  }

  private static Map<String, Koodi> konversio(List<Koodi> koodit) {
    return koodit.stream()
        .collect(
            Collectors.toMap(
                Koodi::getKoodiArvo, a -> a, (a, b) -> a.getVersio() > b.getVersio() ? a : b));
  }

  public static String haeKoodistaArvo(
      Koodi koodi, final String preferoituKieli, String defaultArvo) {
    if (koodi == null || koodi.getMetadata() == null) {
      return defaultArvo;
    } else {
      return Stream.of(
              nameInPreferredLanguage(koodi, preferoituKieli),
              nameInFinnish(koodi),
              nameInAnyLanguage(koodi))
          .flatMap(a -> a)
          .findFirst()
          .map(m -> m.getNimi())
          .orElse(defaultArvo);
    }
  }

  public CompletableFuture<List<Koodi>> ylakoodit(String koodiUri) {
    try {
      CompletableFuture<List<Koodi>> f =
          this.ylakooditCache.get(koodiUri, () -> this.koodistoAsyncResource.ylakoodit(koodiUri));
      if (f.isCompletedExceptionally()) {
        this.ylakooditCache.invalidate(koodiUri);
        return this.ylakooditCache.get(
            koodiUri, () -> this.koodistoAsyncResource.ylakoodit(koodiUri));
      }
      return f;
    } catch (ExecutionException e) {
      return CompletableFuture.failedFuture(e);
    }
  }

  public CompletableFuture<List<Koodi>> alakoodit(String koodiUri) {
    try {
      CompletableFuture<List<Koodi>> f =
          this.alakooditCache.get(koodiUri, () -> this.koodistoAsyncResource.alakoodit(koodiUri));
      if (f.isCompletedExceptionally()) {
        this.alakooditCache.invalidate(koodiUri);
        return this.alakooditCache.get(
            koodiUri, () -> this.koodistoAsyncResource.alakoodit(koodiUri));
      }
      return f;
    } catch (ExecutionException e) {
      return CompletableFuture.failedFuture(e);
    }
  }

  private static Stream<Metadata> nameInFinnish(Koodi koodi) {
    return nameInPreferredLanguage(koodi, KieliUtil.SUOMI);
  }

  private static Stream<Metadata> nameInPreferredLanguage(Koodi koodi, String preferoituKieli) {
    return nameInAnyLanguage(koodi).filter(m -> preferoituKieli.equals(m.getKieli()));
  }

  private static Stream<Metadata> nameInAnyLanguage(Koodi koodi) {
    return koodi.getMetadata().stream();
  }
}
