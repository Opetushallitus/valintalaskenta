package fi.vm.sade.valintalaskenta.tulos.dao;

import fi.vm.sade.valintalaskenta.domain.Versioituhakukohde;

import java.util.List;

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
    List<Versioituhakukohde> readByHakuOid(String hakuoid);

    /**
     * 
     * @return Kaikki hakukohteet (uusimmalla versiolla) kaikissa hauissa
     */
    List<Versioituhakukohde> readAll();

}
