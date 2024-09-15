package fi.vm.sade.valinta.kooste.seuranta;

import fi.vm.sade.valinta.kooste.valintalaskenta.resource.LaskentaParams;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.*;
import io.reactivex.Observable;
import java.util.List;
import java.util.Optional;
import org.springframework.http.ResponseEntity;

public interface LaskentaSeurantaService {

  Observable<Optional<String>> otaSeuraavaLaskentaTyonAlle();

  Observable<LaskentaDto> laskenta(String uuid);

  Observable<LaskentaDto> resetoiTilat(String uuid);

  Observable<TunnisteDto> luoLaskenta(
      LaskentaParams laskentaParams, List<HakukohdeDto> hakukohdeOids);

  Observable<ResponseEntity> merkkaaHakukohteenTila(
      String uuid,
      String hakukohdeOid,
      HakukohdeTila tila,
      Optional<IlmoitusDto> ilmoitusDtoOptional);

  Observable<ResponseEntity> merkkaaLaskennanTila(
      String uuid,
      LaskentaTila tila,
      HakukohdeTila hakukohdetila,
      Optional<IlmoitusDto> ilmoitusDtoOptional);

  Observable<ResponseEntity> merkkaaLaskennanTila(
      String uuid, LaskentaTila tila, Optional<IlmoitusDto> ilmoitusDtoOptional);
}
