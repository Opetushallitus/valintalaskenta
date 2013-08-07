package fi.vm.sade.valintalaskenta.tulos.dao;

import fi.vm.sade.valintalaskenta.domain.MuokattuJonosija;
import fi.vm.sade.valintalaskenta.domain.Valintatapajono;
import fi.vm.sade.valintalaskenta.domain.Versioituhakukohde;

import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: kkammone
 * Date: 5.8.2013
 * Time: 15:04
 * To change this template use File | Settings | File Templates.
 */
public interface MuokattuJonosijaDAO {

    /**
     *
     * @param valintatapajonoOid
     * @return
     */
    //List<MuokattuJonosija> readByValintatapajonoOid(String valintatapajonoOid);


    /**
     *
     * @param valintatapajonoOid
     * @param hakemusOid
     * @return
     */
    MuokattuJonosija readByValintatapajonoOid(String valintatapajonoOid, String hakemusOid);

    /**
     * 
     * @return Kaikki hakukohteet (uusimmalla versiolla) kaikissa hauissa
     */
    void saveOrUpdate(MuokattuJonosija muokattuJonosija);

}
