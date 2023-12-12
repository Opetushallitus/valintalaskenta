package fi.vm.sade.valintalaskenta.tulos.dao.repository;

import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;
import java.util.UUID;
import org.springframework.data.repository.CrudRepository;

public interface JarjestyskriteerihistoriaRepository
    extends CrudRepository<Jarjestyskriteerihistoria, UUID> {}
