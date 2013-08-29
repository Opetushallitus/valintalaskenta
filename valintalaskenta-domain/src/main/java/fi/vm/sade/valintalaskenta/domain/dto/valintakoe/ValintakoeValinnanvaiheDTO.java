package fi.vm.sade.valintalaskenta.domain.dto.valintakoe;

import java.util.ArrayList;
import java.util.List;

/**
 * User: wuoti
 * Date: 29.8.2013
 * Time: 8.31
 */
public class ValintakoeValinnanvaiheDTO {
    private String valinnanVaiheOid;
    private Integer valinnanVaiheJarjestysluku;
    private List<ValintakoeDTO> valintakokeet = new ArrayList<ValintakoeDTO>();

    public String getValinnanVaiheOid() {
        return valinnanVaiheOid;
    }

    public void setValinnanVaiheOid(String valinnanVaiheOid) {
        this.valinnanVaiheOid = valinnanVaiheOid;
    }

    public Integer getValinnanVaiheJarjestysluku() {
        return valinnanVaiheJarjestysluku;
    }

    public void setValinnanVaiheJarjestysluku(Integer valinnanVaiheJarjestysluku) {
        this.valinnanVaiheJarjestysluku = valinnanVaiheJarjestysluku;
    }

    public List<ValintakoeDTO> getValintakokeet() {
        return valintakokeet;
    }

    public void setValintakokeet(List<ValintakoeDTO> valintakokeet) {
        this.valintakokeet = valintakokeet;
    }
}
