package fi.vm.sade.valintalaskenta.laskenta.dao.repository;

import fi.vm.sade.valintalaskenta.domain.valinta.Hakijaryhma;
import org.springframework.data.jdbc.repository.query.Query;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.query.Param;

import java.util.Optional;
import java.util.UUID;

public interface HakijaryhmaRepository extends CrudRepository<Hakijaryhma, UUID> {

    @Query("select * from Hakijaryhma h where h.hakijaryhma_oid = :hakijaryhmaOid")
    Optional<Hakijaryhma> findByHakijaryhmaOid(@Param("hakijaryhmaOid") String hakijaryhmaOId);

}
