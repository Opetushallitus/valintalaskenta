package fi.vm.sade.valinta.kooste.external.resource.valintaperusteet;

import fi.vm.sade.service.valintaperusteet.dto.*;
import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface ValintaperusteetAsyncResource {

  CompletableFuture<List<ValintaperusteetHakijaryhmaDTO>> haeHakijaryhmat(String hakukohdeOid);

  // @GET /valintaperusteet-service/resources/hakukohde/haku/{}
  CompletableFuture<List<HakukohdeViiteDTO>> haunHakukohteet(String hakuOid);

  CompletableFuture<List<ValintaperusteetDTO>> haeValintaperusteet(
      String hakukohdeOid, Integer valinnanVaiheJarjestysluku);

  CompletableFuture<String> haeValintaryhmaVastuuorganisaatio(String valintaryhmaOid);
}
