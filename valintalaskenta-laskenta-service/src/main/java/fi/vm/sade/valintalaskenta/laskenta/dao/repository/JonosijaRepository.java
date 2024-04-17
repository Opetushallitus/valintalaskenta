package fi.vm.sade.valintalaskenta.laskenta.dao.repository;

import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import java.util.UUID;
import org.springframework.data.repository.CrudRepository;

public interface JonosijaRepository extends CrudRepository<Jonosija, UUID> {}
