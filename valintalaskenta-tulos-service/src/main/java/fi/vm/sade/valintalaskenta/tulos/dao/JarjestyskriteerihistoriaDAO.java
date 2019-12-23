package fi.vm.sade.valintalaskenta.tulos.dao;

import fi.vm.sade.auditlog.User;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;

import java.util.List;

public interface JarjestyskriteerihistoriaDAO {
    List<Jarjestyskriteerihistoria> findByValintatapajonoAndHakemusOid(String valintatapajonoOid, String hakemusOid, User auditUser);
}
