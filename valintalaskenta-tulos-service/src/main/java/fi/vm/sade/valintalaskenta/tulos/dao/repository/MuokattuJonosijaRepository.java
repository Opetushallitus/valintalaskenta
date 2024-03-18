package fi.vm.sade.valintalaskenta.tulos.dao.repository;

import fi.vm.sade.valintalaskenta.domain.valinta.MuokattuJonosija;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import org.springframework.data.repository.CrudRepository;

public interface MuokattuJonosijaRepository extends CrudRepository<MuokattuJonosija, UUID> {

  List<MuokattuJonosija> findMuokattuJonosijasByHakuOid(String hakuOid);

  List<MuokattuJonosija> findMuokattuJonosijasByHakukohdeOid(String hakukohdeOid);

  List<MuokattuJonosija> findMuokattuJonosijasByHakuOidAndHakemusOid(
      String hakuOid, String hakemusOid);

  Optional<MuokattuJonosija> findMuokattuJonosijaByValintatapajonoOidAndHakemusOid(
      String valintatapajonoOid, String hakemusOid);
}
