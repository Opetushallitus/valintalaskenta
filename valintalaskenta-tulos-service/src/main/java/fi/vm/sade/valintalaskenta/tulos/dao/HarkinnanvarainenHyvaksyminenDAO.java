package fi.vm.sade.valintalaskenta.tulos.dao;

import fi.vm.sade.valintalaskenta.domain.valinta.HarkinnanvarainenHyvaksyminen;

import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: kkammone
 * Date: 12.9.2013
 * Time: 14:27
 * To change this template use File | Settings | File Templates.
 */
public interface HarkinnanvarainenHyvaksyminenDAO {

    HarkinnanvarainenHyvaksyminen haeHarkinnanvarainenHyvaksyminen(String hakukohdeOid, String hakemusOid);

    void tallennaHarkinnanvarainenHyvaksyminen(HarkinnanvarainenHyvaksyminen harkinnanvarainenHyvaksyminen);

    List<HarkinnanvarainenHyvaksyminen> haeHarkinnanvarainenHyvaksyminen(String hakukohdeoid);

    List<HarkinnanvarainenHyvaksyminen> haeHarkinnanvaraisetHyvaksymisetHaulle(String hakuOid);

    List<HarkinnanvarainenHyvaksyminen> readByHakuOidAndHakemusOid(String hakuOid, String hakemusOid);
}
