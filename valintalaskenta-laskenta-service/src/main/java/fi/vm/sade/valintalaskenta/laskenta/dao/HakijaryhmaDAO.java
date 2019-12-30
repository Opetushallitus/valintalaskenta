package fi.vm.sade.valintalaskenta.laskenta.dao;

import fi.vm.sade.auditlog.User;
import fi.vm.sade.valintalaskenta.domain.valinta.Hakijaryhma;

import java.util.List;
import java.util.Optional;

public interface HakijaryhmaDAO {

    Optional<Hakijaryhma> haeHakijaryhma(String hakijaryhmaOid, User auditUser);

    List<Hakijaryhma> haeHakijaryhmatPrioriteetilla(String hakukohdeOid, int prioriteetti, User auditUser);

    List<Hakijaryhma> haeHakijaryhmat(String hakukohdeOid, User auditUser);

    void create(Hakijaryhma hakijaryhma, User auditUser);

    void poistaHakijaryhma(Hakijaryhma hakijaryhma);
}
