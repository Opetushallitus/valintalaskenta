package fi.vm.sade.valintalaskenta.domain.dto;

import java.util.List;

/**
 * Created by jukais on 21.3.2014.
 */
public class HakijaryhmaDTO {
    private int paikat;
    private String nimi;
    private String oid;
    private Integer prioriteetti;
    private List<String> hakijaOids;

    public int getPaikat() {
        return paikat;
    }

    public void setPaikat(int paikat) {
        this.paikat = paikat;
    }

    public String getNimi() {
        return nimi;
    }

    public void setNimi(String nimi) {
        this.nimi = nimi;
    }

    public String getOid() {
        return oid;
    }

    public void setOid(String oid) {
        this.oid = oid;
    }

    public Integer getPrioriteetti() {
        return prioriteetti;
    }

    public void setPrioriteetti(Integer prioriteetti) {
        this.prioriteetti = prioriteetti;
    }

    public List<String> getHakijaOids() {
        return hakijaOids;
    }

    public void setHakijaOids(List<String> hakijaOids) {
        this.hakijaOids = hakijaOids;
    }
}
