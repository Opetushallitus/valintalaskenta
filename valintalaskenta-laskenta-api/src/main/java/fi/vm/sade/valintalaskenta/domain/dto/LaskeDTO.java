package fi.vm.sade.valintalaskenta.domain.dto;

import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetHakijaryhmaDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.apache.commons.lang.StringUtils;

/**
 * Created by jukais on 26.3.2014.
 */
public class LaskeDTO {
    private final String uuid; // <- Käynnistetyn laskennan tunniste, referenssi logeihin
    private final String hakukohdeOid;
    private final boolean erillishaku;
    private final List<HakemusDTO> hakemus;
    private final List<ValintaperusteetDTO> valintaperuste;
    private final List<ValintaperusteetHakijaryhmaDTO> hakijaryhmat;

    public LaskeDTO() {
        this.uuid = null;
        this.hakemus = Collections.emptyList();
        this.valintaperuste = Collections.emptyList();
        this.hakukohdeOid = StringUtils.EMPTY;
        this.hakijaryhmat = Collections.emptyList();
        this.erillishaku = false;
    }

    public LaskeDTO(
            String uuid,
            boolean erillishaku, String hakukohdeOid,
            List<HakemusDTO> hakemus, List<ValintaperusteetDTO> valintaperuste) {
        this.uuid = uuid;
        this.hakukohdeOid = hakukohdeOid;
        this.hakemus = hakemus != null ? hakemus : Collections
                .<HakemusDTO>emptyList();
        this.valintaperuste = valintaperuste != null ? valintaperuste
                : Collections.<ValintaperusteetDTO>emptyList();
        this.hakijaryhmat = Collections.emptyList();
        this.erillishaku = erillishaku;
    }

    public LaskeDTO(
            String uuid,
            boolean erillishaku, String hakukohdeOid,
            List<HakemusDTO> hakemus, List<ValintaperusteetDTO> valintaperuste,
            List<ValintaperusteetHakijaryhmaDTO> hakijaryhmat) {
        this.uuid = uuid;
        this.hakukohdeOid = hakukohdeOid;
        this.hakemus = hakemus != null ? hakemus : Collections
                .<HakemusDTO>emptyList();
        this.valintaperuste = valintaperuste != null ? valintaperuste
                : Collections.<ValintaperusteetDTO>emptyList();
        this.hakijaryhmat = hakijaryhmat != null ? hakijaryhmat : Collections
                .<ValintaperusteetHakijaryhmaDTO>emptyList();
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

}
