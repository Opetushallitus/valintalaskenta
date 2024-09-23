package fi.vm.sade.valinta.kooste.external.resource.oppijanumerorekisteri;

import fi.vm.sade.valinta.kooste.external.resource.oppijanumerorekisteri.dto.HenkiloPerustietoDto;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

public interface OppijanumerorekisteriAsyncResource {

  CompletableFuture<Map<String, HenkiloPerustietoDto>> haeHenkilot(List<String> personOids);
}
