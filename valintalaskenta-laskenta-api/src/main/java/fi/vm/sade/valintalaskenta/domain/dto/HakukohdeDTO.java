package fi.vm.sade.valintalaskenta.domain.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.ValintatietoValinnanvaiheDTO;

import java.util.ArrayList;
import java.util.List;

@ApiModel(value = "valintalaskenta.domain.dto.Hakukohde", description = "Hakukohde")
public class HakukohdeDTO {
    @ApiModelProperty(value = "Haku OID", required = true)
    private String hakuoid;

    @ApiModelProperty(value = "Tarjoaja OID", required = true)
    private String tarjoajaoid;

    @ApiModelProperty(value = "Hakukohde OID", required = true)
    private String hakukohdeoid;

    @ApiModelProperty(value = "Valinnan vaiheet", required = true)
    private List<ValintatietoValinnanvaiheDTO> valinnanvaihe = new ArrayList<ValintatietoValinnanvaiheDTO>();

    @ApiModelProperty(value = "Prioriteetti", required = true)
    private int prioriteetti;

    private boolean kaikkiJonotSijoiteltu = true;

    private boolean harkinnanvaraisuus = false;

    private List<HakijaryhmaDTO> hakijaryhma = new ArrayList<HakijaryhmaDTO>();

    public String getHakuoid() {
        return hakuoid;
    }

    public void setHakuoid(String hakuoid) {
        this.hakuoid = hakuoid;
    }

    public String getOid() {
        return hakukohdeoid;
    }

    public void setOid(String oid) {
        this.hakukohdeoid = oid;
    }

    public List<ValintatietoValinnanvaiheDTO> getValinnanvaihe() {
        return valinnanvaihe;
    }

    public void setValinnanvaihe(List<ValintatietoValinnanvaiheDTO> valinnanvaihe) {
        this.valinnanvaihe = valinnanvaihe;
    }

    public String getTarjoajaoid() {
        return tarjoajaoid;
    }

    public void setTarjoajaoid(String tarjoajaoid) {
        this.tarjoajaoid = tarjoajaoid;
    }

    public int getPrioriteetti() {
        return prioriteetti;
    }

    public void setPrioriteetti(int prioriteetti) {
        this.prioriteetti = prioriteetti;
    }

    public boolean isHarkinnanvaraisuus() {
        return harkinnanvaraisuus;
    }

    public void setHarkinnanvaraisuus(boolean harkinnanvaraisuus) {
        this.harkinnanvaraisuus = harkinnanvaraisuus;
    }

    public List<HakijaryhmaDTO> getHakijaryhma() {
        return hakijaryhma;
    }

    public void setHakijaryhma(List<HakijaryhmaDTO> hakijaryhma) {
        this.hakijaryhma = hakijaryhma;
    }

    public boolean isKaikkiJonotSijoiteltu() {
        return kaikkiJonotSijoiteltu;
    }

    public void setKaikkiJonotSijoiteltu(boolean kaikkiJonotSijoiteltu) {
        this.kaikkiJonotSijoiteltu = kaikkiJonotSijoiteltu;
    }
}
