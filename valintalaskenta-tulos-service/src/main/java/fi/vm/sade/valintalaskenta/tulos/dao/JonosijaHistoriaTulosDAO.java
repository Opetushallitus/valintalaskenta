package fi.vm.sade.valintalaskenta.tulos.dao;

import fi.vm.sade.valintalaskenta.domain.JonosijaHistoria;

import java.util.List;

/**
 * User: tommiha
 * Date: 8/12/13
 * Time: 2:20 PM
 */
public interface JonosijaHistoriaTulosDAO {
    List<JonosijaHistoria> findByValintatapajonoAndVersioAndHakemusOid(String valintatapajonoOid, Integer versio, String hakemusOid);
}
