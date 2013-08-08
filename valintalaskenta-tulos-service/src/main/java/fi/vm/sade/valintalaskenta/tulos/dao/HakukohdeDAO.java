package fi.vm.sade.valintalaskenta.tulos.dao;

import fi.vm.sade.valintalaskenta.domain.Valintatapajono;
import fi.vm.sade.valintalaskenta.domain.VersiohallintaHakukohde;

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
   // List<Versioituhakukohde> readByHakuOid(String hakuoid);

    /**
     * 
     * @return Kaikki hakukohteet (uusimmalla versiolla) kaikissa hauissa
     */
   // List<Versioituhakukohde> readAll();

    VersiohallintaHakukohde findByValintatapajono(Valintatapajono valintatapajono);
}
