package fi.vm.sade.valintalaskenta.tulos.dao;

import fi.vm.sade.valintalaskenta.domain.valinta.Hakijaryhma;
import java.util.List;

public interface TulosHakijaryhmaDAO {
  List<Hakijaryhma> readByHakukohdeOid(String hakukohdeoid);
}
