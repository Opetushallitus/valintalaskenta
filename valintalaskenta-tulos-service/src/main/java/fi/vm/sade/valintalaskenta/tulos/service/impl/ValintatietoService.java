package fi.vm.sade.valintalaskenta.tulos.service.impl;

import fi.vm.sade.auditlog.User;
import fi.vm.sade.service.valintaperusteet.dto.ValintatapajonoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.HakemusOsallistuminenDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.HakuDTO;

import java.util.List;
import java.util.Map;
import java.util.Optional;

public interface ValintatietoService {

    List<HakemusOsallistuminenDTO> haeValintatiedotHakukohteelle(List<String> valintakoeTunnisteet, String hakukohdeOid);

    HakuDTO haeValintatiedot(String hakuOid, User auditUser);

    HakuDTO haeValintatiedotJonoille(String hakuoid, Map<String, List<String>> jonot, Optional<Map<String, List<ValintatapajonoDTO>>> valintaperusteet, User auditUser);
}
