package fi.vm.sade.valintalaskenta.dao;

import java.util.List;

import fi.vm.sade.valintalaskenta.domain.VersiohallintaHakukohde;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
public interface VersiohallintaHakukohdeDAO {

    void createOrUpdate(VersiohallintaHakukohde v);

    VersiohallintaHakukohde readByHakukohdeOidAndJarjestysnumero(String hakukohdeoid, Integer jarjestysnumero);

    /**
     * 
     * @param hakukohdeoid
     * @return 0..2 with latest jarjestysnumero by hakukohdeoid
     */
    List<VersiohallintaHakukohde> findTwoLatestByHakukohdeOid(String hakukohdeoid);
}
