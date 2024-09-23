package fi.vm.sade.valinta.kooste.external.resource.koodisto;

import fi.vm.sade.valinta.kooste.external.resource.koodisto.dto.Koodi;
import java.util.concurrent.CompletableFuture;

public interface KoodistoAsyncResource {

  CompletableFuture<Koodi> maatjavaltiot2ToMaatjavaltiot1(String koodiUri);
}
