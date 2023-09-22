package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import fi.vm.sade.valintalaskenta.domain.valinta.HakijaryhmaEntity;
import fi.vm.sade.valintalaskenta.tulos.dao.HakijaryhmaDAO;
import java.util.ArrayList;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;

@Repository
public class HakijaryhmaDAOImpl implements HakijaryhmaDAO {
  private static final Logger LOGGER = LoggerFactory.getLogger(HakijaryhmaDAOImpl.class);

  @Override
  public List<HakijaryhmaEntity> readByHakukohdeOid(String hakukohdeoid) {
/*    List<HakijaryhmaMigrationDTO> ryhmat =
        datastore
            .createQuery(HakijaryhmaMigrationDTO.class)
            .field("hakukohdeOid")
            .equal(hakukohdeoid)
            .asList();
    List<Hakijaryhma> migratedRyhmat =
        ryhmat.stream()
            .map(ryhma -> migrate(ryhma))
            .sorted(comparing(Hakijaryhma::getPrioriteetti))
            .collect(Collectors.toList());
    return migratedRyhmat;*/
    return new ArrayList<>();
  }
}
