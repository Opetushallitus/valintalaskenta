package fi.vm.sade.valintalaskenta.domain.dto;


import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by jukais on 26.3.2014.
 */
public class LaskeDTO {
    private List<HakemusDTO> hakemus;
    private List<ValintaperusteetDTO> valintaperuste;

    public List<HakemusDTO> getHakemus() {
        if(hakemus == null) {
            hakemus = new ArrayList<HakemusDTO>();
        }
        return hakemus;
    }

    public void setHakemus(List<HakemusDTO> hakemus) {
        this.hakemus = hakemus;
    }

    public List<ValintaperusteetDTO> getValintaperuste() {
        if(valintaperuste == null) {
            valintaperuste = new ArrayList<ValintaperusteetDTO>();
        }
        return valintaperuste;
    }

    public void setValintaperuste(List<ValintaperusteetDTO> valintaperuste) {
        this.valintaperuste = valintaperuste;
    }
}
