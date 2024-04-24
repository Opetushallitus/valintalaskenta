package fi.vm.sade.valintalaskenta.tulos.dao.repository;

import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;
import java.util.Optional;
import java.util.UUID;
import org.springframework.data.jdbc.repository.query.Query;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.query.Param;

public interface TulosValintatapajonoRepository extends CrudRepository<Valintatapajono, UUID> {

  Optional<Valintatapajono> findValintatapajonoByValintatapajonoOid(String valintatapajonoOid);

  @Query(
      "update Valintatapajono vtj set valmis_sijoiteltavaksi = :valmisSijoiteltavaksi where vtj.valintatapajono_oid = :valintatapajonoOid returning *;")
  Optional<Valintatapajono> paivitaValmisSijoiteltavaksi(
      @Param("valintatapajonoOid") String valintatapajonoOid,
      @Param("valmisSijoiteltavaksi") boolean valmisSijoiteltavaksi);
}
