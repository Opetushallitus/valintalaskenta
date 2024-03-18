package fi.vm.sade.valintalaskenta.tulos.dao.repository;

import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;
import java.util.Optional;
import java.util.UUID;
import org.springframework.data.repository.CrudRepository;

public interface ValintatapajonoRepository extends CrudRepository<Valintatapajono, UUID> {

  Optional<Valintatapajono> findValintatapajonoByValintatapajonoOid(String valintatapajonoOid);
}
