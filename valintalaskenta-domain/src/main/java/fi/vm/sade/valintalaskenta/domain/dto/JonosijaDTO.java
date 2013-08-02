package fi.vm.sade.valintalaskenta.domain.dto;

import java.util.List;
import java.util.TreeMap;

import org.codehaus.jackson.map.annotate.JsonView;

import fi.vm.sade.valintalaskenta.domain.JarjestyskriteerituloksenTila;
import fi.vm.sade.valintalaskenta.domain.JsonViews;

/**
 * Created with IntelliJ IDEA. User: kkammone Date: 13.5.2013 Time: 9:50 To
 * change this template use File | Settings | File Templates.
 */
public class JonosijaDTO {

    @JsonView(JsonViews.Basic.class)
    private int jonosija;

    @JsonView(JsonViews.Basic.class)
    private String hakemusOid;

    @JsonView(JsonViews.Basic.class)
    private String hakijaOid;

    @JsonView(JsonViews.Basic.class)
    private TreeMap<Integer, JarjestyskriteeritulosDTO> jarjestyskriteerit = new TreeMap<Integer, JarjestyskriteeritulosDTO>();

    @JsonView(JsonViews.Basic.class)
    private int prioriteetti;

    @JsonView(JsonViews.Basic.class)
    private String sukunimi;

    @JsonView(JsonViews.Basic.class)
    private String etunimi;

    @JsonView(JsonViews.Basic.class)
    private boolean harkinnanvarainen = false;

    @JsonView(JsonViews.Basic.class)
    private JarjestyskriteerituloksenTila tuloksenTila;

    private List<String> historiat;

    public List<String> getHistoriat() {
        return historiat;
    }

    public void setHistoriat(List<String> historiat) {
        this.historiat = historiat;
    }

    public TreeMap<Integer, JarjestyskriteeritulosDTO> getJarjestyskriteerit() {
        return jarjestyskriteerit;
    }

    public void setJarjestyskriteerit(TreeMap<Integer, JarjestyskriteeritulosDTO> jarjestyskriteerit) {
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
}
