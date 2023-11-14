package fi.vm.sade.valintalaskenta.tulos.dao.repository;

import fi.vm.sade.valintalaskenta.domain.valinta.Hakijaryhma;
import java.util.List;
import java.util.UUID;
import org.springframework.data.jdbc.repository.query.Query;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.query.Param;

public interface TulosHakijaryhmaRepository extends CrudRepository<Hakijaryhma, UUID> {

  @Query(
      "select distinct ryhma.* from Hakijaryhma ryhma join Jonosija js on js.hakijaryhma = ryhma.id where ryhma.hakukohde_oid = :hakukohdeOid")
  List<Hakijaryhma> findHakijaryhmasByHakukohdeOid(@Param("hakukohdeOid") String hakukohdeOid);
}
