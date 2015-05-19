package fi.vm.sade.valintalaskenta.domain.dto.valintatieto;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * Created by jukais on 20.3.2014.
 */
public class HakemusOsallistuminenDTO {
    private List<ValintakoeOsallistuminenDTO> osallistumiset;
    private Date luontiPvm;
    private String etunimi;
    private String sukunimi;
    private String hakemusOid;
    private String hakukohdeOid; // <- hakijan hakutoive tämän hakukohteen hakemuksella

    public List<ValintakoeOsallistuminenDTO> getOsallistumiset() {
        if(osallistumiset == null) {
            osallistumiset = new ArrayList<ValintakoeOsallistuminenDTO>();
        }
        return osallistumiset;
    }

    public void setOsallistumiset(List<ValintakoeOsallistuminenDTO> osallistumiset) {
        this.osallistumiset = osallistumiset;
    }

    public String getHakukohdeOid() {
        return hakukohdeOid;
    }

    public void setHakukohdeOid(String hakukohdeOid) {
        this.hakukohdeOid = hakukohdeOid;
    }

    public void setLuontiPvm(Date luontiPvm) {
        this.luontiPvm = luontiPvm;
    }

    public Date getLuontiPvm() {
        return luontiPvm;
    }

    public void setEtunimi(String etunimi) {
        this.etunimi = etunimi;
    }

    public String getEtunimi() {
        return etunimi;
    }

    public void setSukunimi(String sukunimi) {
        this.sukunimi = sukunimi;
    }

    public String getSukunimi() {
        return sukunimi;
    }

    public void setHakemusOid(String hakemusOid) {
        this.hakemusOid = hakemusOid;
    }

    public String getHakemusOid() {
        return hakemusOid;
    }
}
