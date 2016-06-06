package fi.vm.sade.valintalaskenta.tulos.dao;

import fi.vm.sade.valintalaskenta.domain.valinta.Hakijaryhma;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;

import java.util.List;

public interface HakijaryhmaDAO {
    List<Hakijaryhma> readByHakukohdeOid(String hakukohdeoid);
}
