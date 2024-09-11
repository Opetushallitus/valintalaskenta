package fi.vm.sade.valinta.kooste.external.resource.suoritusrekisteri;

import fi.vm.sade.valinta.kooste.external.resource.suoritusrekisteri.dto.Arvosana;
import fi.vm.sade.valinta.kooste.external.resource.suoritusrekisteri.dto.Oppija;
import fi.vm.sade.valinta.kooste.external.resource.suoritusrekisteri.dto.Suoritus;
import io.reactivex.Observable;
import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface SuoritusrekisteriAsyncResource {

  Observable<List<Oppija>> getOppijatByHakukohde(String hakukohdeOid, String hakuOid);

  CompletableFuture<List<Oppija>> getOppijatByHakukohdeWithoutEnsikertalaisuus(
      String hakukohdeOid, String hakuOid);

  Observable<Oppija> getSuorituksetByOppija(String opiskelijaOid, String hakuOid);

  CompletableFuture<List<Oppija>> getSuorituksetByOppijas(
      List<String> opiskelijaOids, String hakuOid);

  Observable<Oppija> getSuorituksetWithoutEnsikertalaisuus(String opiskelijaOid);

  CompletableFuture<List<Oppija>> getSuorituksetForOppijasWithoutEnsikertalaisuus(
      List<String> opiskelijaOids);

  Observable<List<Oppija>> getSuorituksetWithoutEnsikertalaisuus(List<String> opiskelijaOids);

  CompletableFuture<Suoritus> postSuoritus(Suoritus suoritus);

  CompletableFuture<Arvosana> postArvosana(Arvosana arvosana);

  CompletableFuture<Arvosana> updateExistingArvosana(
      String arvosanaId, Arvosana arvosanaWithUpdatedValues);

  CompletableFuture<String> deleteSuoritus(String suoritusId);

  CompletableFuture<String> deleteArvosana(String arvosanaId);
}
