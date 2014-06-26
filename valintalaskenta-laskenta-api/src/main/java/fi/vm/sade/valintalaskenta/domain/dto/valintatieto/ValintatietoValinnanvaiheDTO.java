package fi.vm.sade.valintalaskenta.domain.dto.valintatieto;

import fi.vm.sade.valintalaskenta.domain.dto.ValinnanvaiheDTO;

/**
 * Created by jukais on 20.3.2014.
 */
public class ValintatietoValinnanvaiheDTO extends ValinnanvaiheDTO {
    private int valinnanvaihe;

    public void setValinnanvaihe(int valinnanvaihe) {
        this.valinnanvaihe = valinnanvaihe;
    }

    public int getValinnanvaihe() {
        return valinnanvaihe;
    }
}
