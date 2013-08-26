package fi.vm.sade.valintalaskenta.tulos.dao;

import java.util.List;
import java.util.Set;

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

    Set<String> findValintatapajonoOidsByHakemusOid(String hakemusOid);

    List<VersiohallintaHakukohde> findByValinnanvaiheOid(String hakuOid);
}
