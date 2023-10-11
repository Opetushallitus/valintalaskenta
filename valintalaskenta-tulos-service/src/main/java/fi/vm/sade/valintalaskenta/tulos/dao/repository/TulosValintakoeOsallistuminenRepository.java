package fi.vm.sade.valintalaskenta.tulos.dao.repository;

import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import org.springframework.data.jdbc.repository.query.Query;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface TulosValintakoeOsallistuminenRepository extends CrudRepository<ValintakoeOsallistuminen, UUID> {

  Optional<ValintakoeOsallistuminen> findValintakoeOsallistuminenByHakemusOid(String hakemusOid);

  @Query("select * from ValintakoeOsallistuminen vo join Hakutoive h on h.valintakoe_osallistuminen = vo.id where h.hakukohde_oid = :hakutoive")
  List<ValintakoeOsallistuminen> findValintakoeOsallistuminensByHakutoive(@Param("hakutoive") String hakukohdeOid);

  @Query("select * from ValintakoeOsallistuminen vo join Hakutoive h on h.valintakoe_osallistuminen = vo.id where h.hakukohde_oid in (:hakutoiveet)")
  List<ValintakoeOsallistuminen> findValintakoeOsallistuminensByHakutoiveet(@Param("hakutoiveet") List<String> hakukohdeOid);

  List<ValintakoeOsallistuminen> findValintakoeOsallistuminensByHakijaOidIn(List<String> hakijaOids);

  @Query("select * from ValintakoeOsallistuminen vo join Hakutoive h on h.valintakoe_osallistuminen = vo.id join ValintakoeValinnanvaihe vvv on vvv.hakutoive = h.id join Valintakoe vk on vk.valintakoe_valinnanvaihe = vvv.id where vo.hakuOid = :hakuOid and vk.osallistuminen = :osallistuminen")
  List<ValintakoeOsallistuminen> findByHakuAndOsallistuminen(@Param("hakuOid") String hakuOid, @Param("osallistuminen") Osallistuminen osallistuminen);

}
