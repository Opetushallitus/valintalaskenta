package fi.vm.sade.valintalaskenta.tulos.dao;

import fi.vm.sade.valintalaskenta.domain.valinta.MuokattuJonosija;

import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: kkammone
 * Date: 5.8.2013
 * Time: 15:04
 * To change this template use File | Settings | File Templates.
 */
public interface MuokattuJonosijaDAO {

    MuokattuJonosija readByValintatapajonoOid(String valintatapajonoOid, String hakemusOid);

    void saveOrUpdate(MuokattuJonosija muokattuJonosija);

    List<MuokattuJonosija> readByHakuOid(String hakuOid);

    List<MuokattuJonosija> readByhakukohdeOid(String hakukohdeOid);

}
