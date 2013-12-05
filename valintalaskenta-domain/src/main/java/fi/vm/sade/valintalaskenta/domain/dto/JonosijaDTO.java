package fi.vm.sade.valintalaskenta.domain.dto;

import com.wordnik.swagger.annotations.ApiModel;
import com.wordnik.swagger.annotations.ApiModelProperty;
import fi.vm.sade.valintalaskenta.domain.JsonViews;
import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila;
import org.codehaus.jackson.map.annotate.JsonView;

import java.util.ArrayList;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Created with IntelliJ IDEA. User: kkammone Date: 13.5.2013 Time: 9:50 To
 * change this template use File | Settings | File Templates.
 */
@ApiModel(value = "JonosijaDTO", description = "Jonosija")
public class JonosijaDTO {

    @ApiModelProperty(value = "Jonosijanumero", required = true)
    @JsonView(JsonViews.Basic.class)
    private int jonosija;

    @ApiModelProperty(value = "Hakemus OID", required = true)
    @JsonView(JsonViews.Basic.class)
    private String hakemusOid;

    @ApiModelProperty(value = "Hakija OID")
    @JsonView(JsonViews.Basic.class)
    private String hakijaOid;

    @ApiModelProperty(value = "Järjestyskriteerit", required = true)
    @JsonView(JsonViews.Basic.class)
    private SortedSet<JarjestyskriteeritulosDTO> jarjestyskriteerit = new TreeSet<JarjestyskriteeritulosDTO>();

    @ApiModelProperty(value = "Prioriteetti", required = true)
    @JsonView(JsonViews.Basic.class)
    private int prioriteetti;

    @ApiModelProperty(value = "Sukunimi")
    @JsonView(JsonViews.Basic.class)
    private String sukunimi;

    @ApiModelProperty(value = "Etunimi")
    @JsonView(JsonViews.Basic.class)
    private String etunimi;

    @ApiModelProperty(value = "Onko hakija hakenut harkinnanvaraisesti")
    @JsonView(JsonViews.Basic.class)
    private boolean harkinnanvarainen = false;

    @ApiModelProperty(value = "Jonosijan tila", required = true)
    @JsonView(JsonViews.Basic.class)
    private JarjestyskriteerituloksenTila tuloksenTila;

    @ApiModelProperty(value = "Jonosijan historiat")
    @JsonView(JsonViews.Basic.class)
    private List<String> historiat;

    @ApiModelProperty(value = "Jonosijan syötetyt arvot")
    @JsonView(JsonViews.Basic.class)
    private List<SyotettyArvoDTO> syotetytArvot = new ArrayList<SyotettyArvoDTO>();

    @ApiModelProperty(value = "Onko jonosijaa muokattu manuaalisesti")
    @JsonView(JsonViews.Basic.class)
    private boolean muokattu = false;

    public List<String> getHistoriat() {
        return historiat;
    }

    public void setHistoriat(List<String> historiat) {
        this.historiat = historiat;
    }

    public SortedSet<JarjestyskriteeritulosDTO> getJarjestyskriteerit() {
        return jarjestyskriteerit;
    }

    public void setJarjestyskriteerit(SortedSet<JarjestyskriteeritulosDTO> jarjestyskriteerit) {
        this.jarjestyskriteerit = jarjestyskriteerit;
    }

    public String getHakijaOid() {
        return hakijaOid;
    }

    public void setHakijaOid(String hakijaOid) {
        this.hakijaOid = hakijaOid;
    }

    public String getHakemusOid() {
        return hakemusOid;
    }

    public void setHakemusOid(String hakemusOid) {
        this.hakemusOid = hakemusOid;
    }

    public int getJonosija() {
        return jonosija;
    }

    public void setJonosija(int jonosija) {
        this.jonosija = jonosija;
    }

    public void setEtunimi(String etunimi) {
        this.etunimi = etunimi;
    }

    public void setSukunimi(String sukunimi) {
        this.sukunimi = sukunimi;
    }

    public void setPrioriteetti(int prioriteetti) {
        this.prioriteetti = prioriteetti;
    }

    public int getPrioriteetti() {
        return prioriteetti;
    }

    public String getSukunimi() {
        return sukunimi;
    }

    public String getEtunimi() {
        return etunimi;
    }

    public JarjestyskriteerituloksenTila getTuloksenTila() {
        return tuloksenTila;
    }

    public void setTuloksenTila(JarjestyskriteerituloksenTila tuloksenTila) {
        this.tuloksenTila = tuloksenTila;
    }

    public boolean isHarkinnanvarainen() {
        return harkinnanvarainen;
    }

    public void setHarkinnanvarainen(boolean harkinnanvarainen) {
        this.harkinnanvarainen = harkinnanvarainen;
    }

    public boolean isMuokattu() {
        return muokattu;
    }

    public void setMuokattu(boolean muokattu) {
        this.muokattu = muokattu;
    }

    public List<SyotettyArvoDTO> getSyotetytArvot() {
        return syotetytArvot;
    }

    public void setSyotetytArvot(List<SyotettyArvoDTO> syotetytArvot) {
        this.syotetytArvot = syotetytArvot;
    }
}
