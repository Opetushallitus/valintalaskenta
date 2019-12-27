package fi.vm.sade.valintalaskenta.tulos.dao;

import fi.vm.sade.auditlog.User;
import fi.vm.sade.valintalaskenta.domain.valinta.Hakijaryhma;

import java.util.List;

public interface HakijaryhmaDAO {
    List<Hakijaryhma> readByHakukohdeOid(String hakukohdeoid, User auditUser);
}
