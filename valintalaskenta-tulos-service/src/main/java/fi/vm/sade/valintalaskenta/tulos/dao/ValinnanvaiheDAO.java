package fi.vm.sade.valintalaskenta.tulos.dao;

import fi.vm.sade.auditlog.User;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.valinta.ValintatapajonoMigrationDTO;
import org.apache.commons.lang3.tuple.Pair;

import java.util.List;

public interface ValinnanvaiheDAO {
    List<Valinnanvaihe> readByHakukohdeOid(String hakukohdeoid, User auditUser);

    List<Valinnanvaihe> readByHakuOid(String hakuoid, User auditUser);

    List<ValintatapajonoMigrationDTO> valintatapajonotJotkaEivatKaytaLaskentaa();

    List<Pair<String, String>> hakuOidHakukohdeOidPairsForJonos(List<ValintatapajonoMigrationDTO> validValintatapajonos);

    List<Valinnanvaihe> readByHakuOidAndHakemusOid(String hakuOid, String hakemusOid, User auditUser);

    Valinnanvaihe findByValintatapajonoOid(String valintatapajonoOid, User auditUser);

    Valinnanvaihe haeValinnanvaihe(String valinnanvaiheOid, User auditUser);

    void saveOrUpdate(Valinnanvaihe vaihe, User auditUser);
}
