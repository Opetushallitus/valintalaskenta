package fi.vm.sade.valintalaskenta.tulos.dao;

import fi.vm.sade.auditlog.User;
import fi.vm.sade.valintalaskenta.domain.valinta.HarkinnanvarainenHyvaksyminen;

import java.util.List;

public interface HarkinnanvarainenHyvaksyminenDAO {
    HarkinnanvarainenHyvaksyminen haeHarkinnanvarainenHyvaksyminen(String hakukohdeOid, String hakemusOid);

    void tallennaHarkinnanvarainenHyvaksyminen(HarkinnanvarainenHyvaksyminen harkinnanvarainenHyvaksyminen, User auditUser);

    List<HarkinnanvarainenHyvaksyminen> haeHarkinnanvarainenHyvaksyminen(String hakukohdeoid);

    List<HarkinnanvarainenHyvaksyminen> haeHarkinnanvaraisetHyvaksymisetHaulle(String hakuOid);

    List<HarkinnanvarainenHyvaksyminen> readByHakuOidAndHakemusOid(String hakuOid, String hakemusOid);
}
