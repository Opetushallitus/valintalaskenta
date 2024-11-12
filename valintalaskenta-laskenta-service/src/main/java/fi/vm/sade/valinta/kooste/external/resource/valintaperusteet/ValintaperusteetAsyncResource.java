package fi.vm.sade.valinta.kooste.external.resource.valintaperusteet;

import fi.vm.sade.service.valintaperusteet.dto.*;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

public interface ValintaperusteetAsyncResource {

  CompletableFuture<List<HakukohdeViiteDTO>> haunHakukohteet(String hakuOid);

  CompletableFuture<String> haeValintaryhmaVastuuorganisaatio(String valintaryhmaOid);

  CompletableFuture<List<ValintaperusteetDTO>> haeValintaperusteet(
      String hakukohdeOid, Optional<Integer> valinnanVaiheJarjestysluku);
}
