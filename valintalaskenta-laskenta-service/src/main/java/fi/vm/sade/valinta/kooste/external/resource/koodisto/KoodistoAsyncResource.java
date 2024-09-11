package fi.vm.sade.valinta.kooste.external.resource.koodisto;

import fi.vm.sade.valinta.kooste.external.resource.koodisto.dto.Koodi;
import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface KoodistoAsyncResource {
  CompletableFuture<List<Koodi>> haeKoodisto(String koodistoUri);

  CompletableFuture<List<Koodi>> ylakoodit(String koodiUri);

  CompletableFuture<List<Koodi>> alakoodit(String koodiUri);

  CompletableFuture<Koodi> maatjavaltiot2ToMaatjavaltiot1(String koodiUri);

  public CompletableFuture<List<Koodi>> haeKoodienUusinVersio(String... koodiUris);
}
