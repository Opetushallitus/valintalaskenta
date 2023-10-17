package fi.vm.sade.valintalaskenta.laskenta.dao.repository;

import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;
import java.util.UUID;
import org.springframework.data.repository.CrudRepository;

public interface ValintatapajonoRepository extends CrudRepository<Valintatapajono, UUID> {}
