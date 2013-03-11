package fi.vm.sade.valintalaskenta.dao;

import java.util.List;

import fi.vm.sade.valintalaskenta.domain.Hakukohde;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
public interface HakukohdeDAO {

    /**
     * @param hakuoid
     * @return Hakukohteet (uusimmalla versiolla) tietyllä hakuoidilla
     */
    List<Hakukohde> readByHakuOid(String hakuoid);

    /**
     * 
     * @return Kaikki hakukohteet (uusimmalla versiolla) kaikissa hauissa
     */
    List<Hakukohde> readAll();

}
