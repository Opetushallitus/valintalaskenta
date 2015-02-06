package fi.vm.sade.valintalaskenta.domain.dto;

import com.wordnik.swagger.annotations.ApiModel;
import com.wordnik.swagger.annotations.ApiModelProperty;

import java.util.ArrayList;
import java.util.List;

/**
 * User: wuoti
 * Date: 9.12.2013
 * Time: 9.54
 */
@ApiModel(value = "MuokattuJonosijaDTO", description = "Muokattu jonosija")
public class MuokattuJonosijaDTO {

    @ApiModelProperty(value = "Hakukohde OID", required = true)
    private String hakukohdeOid;

    @ApiModelProperty(value = "Haku OID", required = true)
    private String hakuOid;

    @ApiModelProperty(value = "Valintatapajono OID", required = true)
    private String valintatapajonoOid;

    @ApiModelProperty(value = "Hakukohde OID", required = true)
    private String hakemusOid;

    @ApiModelProperty(value = "Prioriteetti", required = true)
    private Integer prioriteetti; // hakutoive

    @ApiModelProperty(value = "Harkinnanvaraisuus")
    private Boolean harkinnanvarainen;

    @ApiModelProperty(value = "Järjestyskriteeritulokset", required = true)
    private List<JarjestyskriteeritulosDTO> jarjestyskriteerit = new ArrayList<JarjestyskriteeritulosDTO>();

    @ApiModelProperty(value = "Lokiviestit", required = true)
    private List<LogEntryDTO> logEntries = new ArrayList<LogEntryDTO>();

    public String getHakukohdeOid() {
        return hakukohdeOid;
    }

    public void setHakukohdeOid(String hakukohdeOid) {
        this.hakukohdeOid = hakukohdeOid;
    }

    public String getHakuOid() {
        return hakuOid;
    }

    public void setHakuOid(String hakuOid) {
        this.hakuOid = hakuOid;
    }

    public String getValintatapajonoOid() {
        return valintatapajonoOid;
    }

    public void setValintatapajonoOid(String valintatapajonoOid) {
        this.valintatapajonoOid = valintatapajonoOid;
    }

    public String getHakemusOid() {
        return hakemusOid;
    }

    public void setHakemusOid(String hakemusOid) {
        this.hakemusOid = hakemusOid;
    }

    public Integer getPrioriteetti() {
        return prioriteetti;
    }

    public void setPrioriteetti(Integer prioriteetti) {
        this.prioriteetti = prioriteetti;
    }

    public Boolean getHarkinnanvarainen() {
        return harkinnanvarainen;
    }

    public void setHarkinnanvarainen(Boolean harkinnanvarainen) {
        this.harkinnanvarainen = harkinnanvarainen;
    }

    public List<JarjestyskriteeritulosDTO> getJarjestyskriteerit() {
        return jarjestyskriteerit;
    }

    public void setJarjestyskriteerit(List<JarjestyskriteeritulosDTO> jarjestyskriteerit) {
        this.jarjestyskriteerit = jarjestyskriteerit;
    }

    public List<LogEntryDTO> getLogEntries() {
        return logEntries;
    }

    public void setLogEntries(List<LogEntryDTO> logEntries) {
        this.logEntries = logEntries;
    }
}
