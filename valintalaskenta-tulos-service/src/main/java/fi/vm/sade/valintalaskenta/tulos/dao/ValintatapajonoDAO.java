package fi.vm.sade.valintalaskenta.tulos.dao;

import fi.vm.sade.valintalaskenta.domain.Valintatapajono;

import java.util.List;

/**
 * @author Jussi Jartamo
 */
public interface ValintatapajonoDAO {

    List<Valintatapajono> readByValinnanvaiheOid(String valinnanvaiheOid);

    Valintatapajono findByValintatapajonoOidHakemusOidAndJarjestyskriteeriPrioriteetti(String valintatapajonoOid, String hakemusOid, Integer jarjestyskriteeriPrioriteetti);

    void saveOrUpdate(Valintatapajono jono);
}
