package fi.vm.sade.valintalaskenta.tulos.dao.repository;

import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import org.springframework.data.jdbc.repository.query.Query;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Stream;

public interface TulosValinnanvaiheRepository extends CrudRepository<Valinnanvaihe, UUID> {

  List<Valinnanvaihe> findValinnanvaihesByHakukohdeOid(String hakukohdeOid);

  Stream<Valinnanvaihe> findValinnanvaihesByHakuOid(String hakuOid);

  @Query("select * from Valinnanvaihe vv join Valintatapajono vtpjono on vtpjono.valinnanvaihe = vv.id join Jonosija js on js.valintatapajono = vtpjono.id where js.hakemus_oid = :hakemusOid and vv.haku_oid = :hakuOid")
  List<Valinnanvaihe> findValinnanvaihesByHakuOidAndHakemusOid(@Param("hakuOid") String hakuOid, @Param("hakemusOid") String hakemusOid);

  @Query("select * from Valinnanvaihe vv join Valintatapajono vtpjono on vtpjono.valinnanvaihe = vv.id where vtpjono.valintatapajono_oid = :valintatapajonoOid")
  Optional<Valinnanvaihe> findValinnanvaiheByValintatapajono(@Param("valintatapajonoOid") String valintatapajonoOid);

  Optional<Valinnanvaihe> findValinnanvaiheByValinnanvaiheOid(String valinnanvaiheOid);
}
