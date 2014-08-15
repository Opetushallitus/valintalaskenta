package fi.vm.sade.valintalaskenta.domain.valintakoe;

import org.mongodb.morphia.annotations.Embedded;

import java.util.ArrayList;
import java.util.List;

/**
 * User: wuoti
 * Date: 2.5.2013
 * Time: 13.31
 */
@Embedded
public class ValintakoeValinnanvaihe {
    private String valinnanVaiheOid;
    private Integer valinnanVaiheJarjestysluku;

    @Embedded
    private List<Valintakoe> valintakokeet = new ArrayList<Valintakoe>();

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

    public List<Valintakoe> getValintakokeet() {
        return valintakokeet;
    }

    public void setValintakokeet(List<Valintakoe> valintakokeet) {
        this.valintakokeet = valintakokeet;
    }
}
