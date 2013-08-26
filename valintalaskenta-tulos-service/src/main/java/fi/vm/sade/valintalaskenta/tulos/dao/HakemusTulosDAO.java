package fi.vm.sade.valintalaskenta.tulos.dao;

import java.util.Collection;
import java.util.List;

import fi.vm.sade.valintalaskenta.domain.Valintatapajono;
import fi.vm.sade.valintalaskenta.domain.VersiohallintaHakukohde;

/**
 * 
 * @author Jussi Jartamo
 * 
 *         Hakemuslähtöistä tulosten hakua
 */
public interface HakemusTulosDAO {

    List<Valintatapajono> findByHakemusOid(String hakemusOid);

    Collection<String> findValintatapajonoOidsByHakemusOid(String hakemusOid);

    List<VersiohallintaHakukohde> findPartialByValinnanvaiheOid(String hakuOid, Collection<String> valinnanvaiheOid);
}
