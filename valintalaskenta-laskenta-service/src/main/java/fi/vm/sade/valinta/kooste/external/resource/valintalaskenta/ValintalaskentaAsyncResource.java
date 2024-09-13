package fi.vm.sade.valinta.kooste.external.resource.valintalaskenta;

import fi.vm.sade.valintalaskenta.domain.dto.LaskeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.SuoritustiedotDTO;
import io.reactivex.Observable;
import java.util.List;

public interface ValintalaskentaAsyncResource {
  Observable<String> laskeJaSijoittele(
      String uuid, List<LaskeDTO> lista, SuoritustiedotDTO suoritustiedot);

  Observable<String> valintakokeet(LaskeDTO laskeDTO, SuoritustiedotDTO suoritukset);

  Observable<String> laske(LaskeDTO laskeDTO, SuoritustiedotDTO suoritukset);

  Observable<String> laskeKaikki(LaskeDTO laskeDTO, SuoritustiedotDTO suoritukset);
}
