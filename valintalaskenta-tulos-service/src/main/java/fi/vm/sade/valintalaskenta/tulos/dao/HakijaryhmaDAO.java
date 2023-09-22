package fi.vm.sade.valintalaskenta.tulos.dao;

import fi.vm.sade.valintalaskenta.domain.valinta.HakijaryhmaEntity;
import java.util.List;

public interface HakijaryhmaDAO {
  List<HakijaryhmaEntity> readByHakukohdeOid(String hakukohdeoid);
}
