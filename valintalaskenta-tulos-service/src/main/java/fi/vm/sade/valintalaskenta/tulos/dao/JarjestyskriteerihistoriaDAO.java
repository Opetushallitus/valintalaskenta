package fi.vm.sade.valintalaskenta.tulos.dao;

import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;

import java.util.List;

/**
 * User: tommiha
 * Date: 8/12/13
 * Time: 2:20 PM
 */
public interface JarjestyskriteerihistoriaDAO {
    List<Jarjestyskriteerihistoria> findByValintatapajonoAndVersioAndHakemusOid(String valintatapajonoOid, String hakemusOid);
}
