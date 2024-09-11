package fi.vm.sade.valinta.kooste.external.resource.oppijanumerorekisteri;

import fi.vm.sade.valinta.kooste.external.resource.oppijanumerorekisteri.dto.HenkiloCreateDTO;
import fi.vm.sade.valinta.kooste.external.resource.oppijanumerorekisteri.dto.HenkiloPerustietoDto;
import fi.vm.sade.valinta.kooste.external.resource.oppijanumerorekisteri.dto.HenkiloViiteDto;
import io.reactivex.Observable;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CompletableFuture;

public interface OppijanumerorekisteriAsyncResource {
  Observable<List<HenkiloPerustietoDto>> haeTaiLuoHenkilot(
      List<HenkiloCreateDTO> henkiloPrototyypit);

  CompletableFuture<Map<String, HenkiloPerustietoDto>> haeHenkilot(List<String> personOids);

  CompletableFuture<List<HenkiloViiteDto>> haeHenkiloOidDuplikaatit(Set<String> personOids);
}
