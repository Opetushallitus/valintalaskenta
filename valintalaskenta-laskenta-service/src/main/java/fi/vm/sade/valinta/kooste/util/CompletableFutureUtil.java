package fi.vm.sade.valinta.kooste.util;

import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

public class CompletableFutureUtil {
  public static <T> CompletableFuture<List<T>> sequence(List<CompletableFuture<T>> fs) {
    return CompletableFuture.allOf(fs.toArray(CompletableFuture[]::new))
        .thenApplyAsync(v -> fs.stream().map(CompletableFuture::join).collect(Collectors.toList()));
  }

  public static CompletableFuture<List<?>> sequenceWildcard(List<CompletableFuture<?>> fs) {
    return CompletableFuture.allOf(fs.toArray(CompletableFuture[]::new))
        .thenApplyAsync(v -> fs.stream().map(CompletableFuture::join).collect(Collectors.toList()));
  }

  public static <K, V> CompletableFuture<Map<K, V>> sequence(Map<K, CompletableFuture<V>> fs) {
    return CompletableFuture.allOf(fs.values().toArray(new CompletableFuture[0]))
        .thenApplyAsync(
            v ->
                fs.entrySet().stream()
                    .collect(Collectors.toMap(Map.Entry::getKey, e -> e.getValue().join())));
  }
}
