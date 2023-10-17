package fi.vm.sade.valintalaskenta.laskenta.dao.repository;

import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.valinta.ValinnanvaiheLite;
import org.springframework.data.jdbc.repository.query.Query;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface ValinnanvaiheRepository extends CrudRepository<Valinnanvaihe, UUID> {

  @Query("select * from Valinnanvaihe vv where vv.haku_oid = :hakuOid and vv.hakukohde_oid = :hakukohdeOid and vv.jarjestysnumero = :jarjestysnro")
  Optional<Valinnanvaihe> findPreviousValinnanvaihe(@Param("hakuOid") String hakuOid,
                                                    @Param("hakukohdeOid") String hakukohdeOid,
                                                    @Param("jarjestysnro") int jarjestysnumero);

  Optional<Valinnanvaihe> findValinnanvaiheByValinnanvaiheOid(String valinnanvaiheOid);

  Optional<ValinnanvaiheLite> findValinnanvaiheLiteByValinnanvaiheOid(String valinnanvaiheOid);

  @Query("select * from Valinnanvaihe vv where vv.haku_oid = :hakuOid and vv.hakukohde_oid = :hakukohdeOid and vv.jarjestysnumero < :jarjestysnro order by vv.jarjestysnumero desc limit 1")
  Optional<Valinnanvaihe> findViimeisinEdeltavaValinnanvaihe(@Param("hakuOid") String hakuOid,
                                                             @Param("hakukohdeOid") String hakukohdeOid,
                                                             @Param("jarjestysnro") int jarjestysnumero);

  @Query("select distinct vv.* from Valinnanvaihe vv where vv.haku_oid = :hakuOid and vv.hakukohde_oid = :hakukohdeOid and vv.jarjestysnumero = :jarjestysnro")
  List<Valinnanvaihe> findDistinctValinnanvaiheetJarjestysnumerolla(@Param("hakuOid") String hakuOid,
                                                                    @Param("hakukohdeOid") String hakukohdeOid,
                                                                    @Param("jarjestysnro") int jarjestysnumero);

}

