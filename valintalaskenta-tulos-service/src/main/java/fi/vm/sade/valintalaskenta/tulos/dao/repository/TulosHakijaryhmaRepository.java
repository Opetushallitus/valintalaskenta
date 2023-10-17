package fi.vm.sade.valintalaskenta.tulos.dao.repository;

import fi.vm.sade.valintalaskenta.domain.valinta.Hakijaryhma;
import org.springframework.data.repository.CrudRepository;

import java.util.List;
import java.util.UUID;

public interface TulosHakijaryhmaRepository extends CrudRepository<Hakijaryhma, UUID> {

  List<Hakijaryhma> findHakijaryhmasByHakukohdeOid(String hakukohdeOid);
}
