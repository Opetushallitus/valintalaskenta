package fi.vm.sade.valintalaskenta.laskenta.dao.repository;

import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import java.util.Optional;
import java.util.UUID;
import org.springframework.data.jdbc.repository.query.Query;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.query.Param;

public interface ValintakoeOsallistuminenRepository
    extends CrudRepository<ValintakoeOsallistuminen, UUID> {

  @Query(
      "SELECT * FROM valintakoe_osallistuminen vo WHERE vo.hakemus_oid = :hakemusOid AND vo.haku_oid = :hakuOid")
  Optional<ValintakoeOsallistuminen> findByHakemusOidAndHakuOid(
      @Param("hakemusOid") String hakemusOid, @Param("hakuOid") String hakuOid);

  @Query(
      "SELECT distinct vo.id FROM valintakoe_osallistuminen vo "
          + "JOIN hakutoive h on h.valintakoe_osallistuminen = vo.id "
          + "JOIN valintakoe_valinnanvaihe vvv on vvv.hakutoive = h.id "
          + "WHERE vo.haku_oid = :hakuOid AND h.hakukohde_oid = :hakukohdeOid AND vvv.valinnan_vaihe_jarjestysluku = :jarjestysluku limit 1")
  Optional<UUID> findByHakuHakukohdeAndValinnanvaiheJarjestysLuku(
      @Param("hakuOid") String hakuOid,
      @Param("hakukohdeOid") String hakukohdeOid,
      @Param("jarjestysluku") int jarjestysluku);
}
