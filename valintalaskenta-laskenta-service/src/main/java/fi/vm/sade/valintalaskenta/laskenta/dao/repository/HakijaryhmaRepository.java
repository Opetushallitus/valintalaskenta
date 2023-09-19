package fi.vm.sade.valintalaskenta.laskenta.dao.repository;

import fi.vm.sade.valintalaskenta.domain.valinta.Hakijaryhma;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.Optional;
import java.util.UUID;

@Repository
public interface HakijaryhmaRepository extends CrudRepository<Hakijaryhma, UUID> {

    @Query("select h from Hakijaryhma h left join Jonosija j on j.hakijaryhma = h.id where h.hakijaryhmaOid = :hakijaryhmaOid")
    Optional<Hakijaryhma> findByHakijaryhmaOid(@Param("hakijaryhmaOid") String hakijaryhmaOId);

}
