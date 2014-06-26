package fi.vm.sade.valintalaskenta.domain.dto.valintatieto;

import fi.vm.sade.valintalaskenta.domain.dto.HakijaDTO;
import fi.vm.sade.valintalaskenta.domain.dto.ValintatapajonoDTO;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by jukais on 20.3.2014.
 */
public class ValintatietoValintatapajonoDTO extends ValintatapajonoDTO {
    private List<HakijaDTO> hakija;

    public List<HakijaDTO> getHakija() {
        if(hakija == null) {
            hakija = new ArrayList<HakijaDTO>();
        }
        return hakija;
    }

    public void setHakija(List<HakijaDTO> hakija) {
        this.hakija = hakija;
    }
}
