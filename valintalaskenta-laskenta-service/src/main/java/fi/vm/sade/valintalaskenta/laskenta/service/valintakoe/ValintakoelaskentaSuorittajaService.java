package fi.vm.sade.valintalaskenta.laskenta.service.valintakoe;

import fi.vm.sade.auditlog.User;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.laskenta.resource.ValintakoelaskennanKumulatiivisetTulokset;

import java.util.List;

public interface ValintakoelaskentaSuorittajaService {
    void laske(HakemusDTO hakemus, List<ValintaperusteetDTO> valintaperusteet, String uuid, ValintakoelaskennanKumulatiivisetTulokset kumulatiivisetTulokset, boolean korkeakouluhaku, User auditUser);
    void siivoaValintakoeOsallistumiset(List<HakemusDTO> hakemukset, String hakukohdeOid, List<String> saastettavienValinnanvaiheidenOidit);
}
