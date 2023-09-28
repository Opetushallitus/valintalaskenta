package fi.vm.sade.valintalaskenta.laskenta.dao.repository;

import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import org.springframework.data.jdbc.repository.query.Query;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.query.Param;

import java.util.Optional;
import java.util.UUID;

public interface ValinnanvaiheRepository extends CrudRepository<Valinnanvaihe, UUID> {

  @Query("select * from Valinnanvaihe vv where vv.haku_oid = :hakuOid and vv.hakukohde_oid = :hakukohdeOid and vv.jarjestysnumero = :jarjestysnro")
  Optional<Valinnanvaihe> findPreviousValinnanvaihe(@Param("hakuOid") String hakuOid,
                                                    @Param("hakukohdeOid") String hakukohdeOid,
                                                    @Param("jarjestysnro") int jarjestysnumero);

  Optional<Valinnanvaihe> findValinnanvaiheByValinnanvaiheOid(String valinnanvaiheOid);
}
