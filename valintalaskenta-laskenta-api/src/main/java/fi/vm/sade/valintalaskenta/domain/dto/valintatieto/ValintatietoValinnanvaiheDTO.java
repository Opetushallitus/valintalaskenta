package fi.vm.sade.valintalaskenta.domain.dto.valintatieto;

import fi.vm.sade.valintalaskenta.domain.dto.ValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeDTO;

import java.util.Date;
import java.util.List;

public class ValintatietoValinnanvaiheDTO extends ValinnanvaiheDTO {
    public ValintatietoValinnanvaiheDTO() {
    }

    public ValintatietoValinnanvaiheDTO(int jarjestysnumero, String valinnanvaiheoid, String hakuOid, String nimi, Date createdAt, List<ValintatietoValintatapajonoDTO> valintatapajonot, List<ValintakoeDTO> valintakokeet) {
        super(jarjestysnumero, valinnanvaiheoid, hakuOid, nimi, createdAt, valintatapajonot, valintakokeet);
        this.valinnanvaihe = jarjestysnumero;
    }

    private int valinnanvaihe;

    public void setValinnanvaihe(int valinnanvaihe) {
        this.valinnanvaihe = valinnanvaihe;
    }

    public int getValinnanvaihe() {
        return valinnanvaihe;
    }
}
