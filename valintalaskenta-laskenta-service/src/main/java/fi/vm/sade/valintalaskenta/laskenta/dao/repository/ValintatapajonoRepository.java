package fi.vm.sade.valintalaskenta.laskenta.dao.repository;

import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;
import org.springframework.data.repository.CrudRepository;

import java.util.UUID;

public interface ValintatapajonoRepository extends CrudRepository<Valintatapajono, UUID> {
}
