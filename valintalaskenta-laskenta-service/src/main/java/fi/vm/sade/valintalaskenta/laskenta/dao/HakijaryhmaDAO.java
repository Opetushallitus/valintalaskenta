package fi.vm.sade.valintalaskenta.laskenta.dao;

import fi.vm.sade.valintalaskenta.domain.valinta.Hakijaryhma;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;

import java.util.List;
import java.util.Optional;

public interface HakijaryhmaDAO {

    Optional<Hakijaryhma> haeHakijaryhma(String hakijaryhmaOid);

    List<Hakijaryhma> haeHakijaryhmatPrioriteetilla(String hakukohdeOid, int prioriteetti);

    List<Hakijaryhma> haeHakijaryhmat(String hakukohdeOid);

    void create(Hakijaryhma hakijaryhma);

    void poistaHakijaryhma(Hakijaryhma hakijaryhma);
}