package fi.vm.sade.valintalaskenta.domain.dto;

import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetHakijaryhmaDTO;
import org.apache.commons.lang3.StringUtils;

import java.util.Collections;
import java.util.List;

public class LaskeDTO {
    private final String uuid; // <- Käynnistetyn laskennan tunniste, referenssi logeihin
    private final String hakukohdeOid;
    private final boolean erillishaku;
    private final List<HakemusDTO> hakemus;
    private final List<ValintaperusteetDTO> valintaperuste;
    private final List<ValintaperusteetHakijaryhmaDTO> hakijaryhmat;
    private final boolean korkeakouluhaku;

    public LaskeDTO() {
        this.uuid = null;
        this.hakemus = Collections.emptyList();
        this.valintaperuste = Collections.emptyList();
        this.hakukohdeOid = StringUtils.EMPTY;
        this.hakijaryhmat = Collections.emptyList();
        this.korkeakouluhaku = false;
        this.erillishaku = false;
    }

    public LaskeDTO(String uuid, boolean korkeakouluhaku, boolean erillishaku, String hakukohdeOid,
                    List<HakemusDTO> hakemus, List<ValintaperusteetDTO> valintaperuste) {
        this.uuid = uuid;
        this.hakukohdeOid = hakukohdeOid;
        this.hakemus = hakemus != null ? hakemus : Collections.<HakemusDTO>emptyList();
        this.valintaperuste = valintaperuste != null ? valintaperuste : Collections.<ValintaperusteetDTO>emptyList();
        this.hakijaryhmat = Collections.emptyList();
        this.korkeakouluhaku = korkeakouluhaku;
        this.erillishaku = erillishaku;
    }

    public LaskeDTO(String uuid, boolean korkeakouluhaku, boolean erillishaku, String hakukohdeOid,
                    List<HakemusDTO> hakemus, List<ValintaperusteetDTO> valintaperuste,
                    List<ValintaperusteetHakijaryhmaDTO> hakijaryhmat) {
        this.uuid = uuid;
        this.hakukohdeOid = hakukohdeOid;
        this.hakemus = hakemus != null ? hakemus : Collections.<HakemusDTO>emptyList();
        this.valintaperuste = valintaperuste != null ? valintaperuste : Collections.<ValintaperusteetDTO>emptyList();
        this.hakijaryhmat = hakijaryhmat != null ? hakijaryhmat : Collections.<ValintaperusteetHakijaryhmaDTO>emptyList();
        this.korkeakouluhaku = korkeakouluhaku;
        this.erillishaku = erillishaku;
    }

    public String getUuid() {
        return uuid;
    }

    public String getHakukohdeOid() {
        return hakukohdeOid;
    }

    public List<HakemusDTO> getHakemus() {
        return hakemus;
    }

    public List<ValintaperusteetDTO> getValintaperuste() {
        return valintaperuste;
    }

    public List<ValintaperusteetHakijaryhmaDTO> getHakijaryhmat() {
        return hakijaryhmat;
    }

    public boolean isErillishaku() {
        return erillishaku;
    }

    public boolean isKorkeakouluhaku() {
        return korkeakouluhaku;
    }
}
