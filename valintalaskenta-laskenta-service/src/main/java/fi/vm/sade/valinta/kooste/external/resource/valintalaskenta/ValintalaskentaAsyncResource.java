package fi.vm.sade.valinta.kooste.external.resource.valintalaskenta;

import fi.vm.sade.valintalaskenta.domain.dto.JonoDto;
import fi.vm.sade.valintalaskenta.domain.dto.LaskeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.SuoritustiedotDTO;
import fi.vm.sade.valintalaskenta.domain.dto.ValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.ValintatietoValinnanvaiheDTO;
import io.reactivex.Observable;
import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface ValintalaskentaAsyncResource {
  Observable<List<JonoDto>> jonotSijoitteluun(String hakuOid);

  CompletableFuture<ValinnanvaiheDTO> lisaaTuloksia(
      String hakuOid, String hakukohdeOid, String tarjoajaOid, ValinnanvaiheDTO vaihe);

  Observable<String> laskeJaSijoittele(
      String uuid, List<LaskeDTO> lista, SuoritustiedotDTO suoritustiedot);

  CompletableFuture<List<ValintatietoValinnanvaiheDTO>> laskennantulokset(String hakukohdeOid);

  Observable<String> valintakokeet(LaskeDTO laskeDTO, SuoritustiedotDTO suoritukset);

  Observable<String> laske(LaskeDTO laskeDTO, SuoritustiedotDTO suoritukset);

  Observable<String> laskeKaikki(LaskeDTO laskeDTO, SuoritustiedotDTO suoritukset);

  Observable<List<ValintatietoValinnanvaiheDTO>> hakukohde(String hakukohdeoid);
}
