package fi.vm.sade.valintalaskenta.tulos.dao.repository;

import fi.vm.sade.valintalaskenta.domain.valinta.HarkinnanvarainenHyvaksyminen;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import org.springframework.data.repository.CrudRepository;

public interface HarkinnanvarainenHyvaksyminenRepository
    extends CrudRepository<HarkinnanvarainenHyvaksyminen, UUID> {

  Optional<HarkinnanvarainenHyvaksyminen>
      findHarkinnanvarainenHyvaksyminenByHakukohdeOidAndHakemusOid(
          String hakukohdeOid, String hakemusOid);

  List<HarkinnanvarainenHyvaksyminen> findHarkinnanvarainenHyvaksyminensByHakukohdeOid(
      String hakukohdeOid);

  List<HarkinnanvarainenHyvaksyminen> findHarkinnanvarainenHyvaksyminensByHakuOid(String hakuOid);

  List<HarkinnanvarainenHyvaksyminen> findHarkinnanvarainenHyvaksyminensByHakuOidAndHakemusOid(
      String hakuOid, String hakemusOid);
}
