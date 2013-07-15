package fi.vm.sade.valintalaskenta.domain;

import java.util.ArrayList;
import java.util.List;
import java.util.TreeMap;

import org.codehaus.jackson.map.annotate.JsonView;

import com.google.code.morphia.annotations.Embedded;

/**
 * User: kkammone Date: 13.5.2013 Time: 9:50
 */
@Embedded("Jonosija")
public class Jonosija {

    @JsonView(JsonViews.Basic.class)
    private int jonosija;

    @JsonView(JsonViews.Basic.class)
    private JarjestyskriteerituloksenTila tuloksenTila;

    @JsonView(JsonViews.Basic.class)
    private String kuvaus;

    @JsonView(JsonViews.Basic.class)
    private String hakemusoid;

    @JsonView(JsonViews.Basic.class)
    private String hakijaoid;

    @JsonView(JsonViews.Basic.class)
    private String etunimi;

    @JsonView(JsonViews.Basic.class)
    private String sukunimi;

    @JsonView(JsonViews.Basic.class)
    private int prioriteetti; // hakutoive

    @JsonView(JsonViews.Basic.class)
    private boolean harkinnanvarainen = false;

    // @JsonSerialize(using = HtmlSerializer.class)
    @Embedded
    private List<String> historiat = new ArrayList<String>();

    @Embedded
    private TreeMap<Integer, Jarjestyskriteeritulos> jarjestyskriteerit = new TreeMap<Integer, Jarjestyskriteeritulos>();

    public List<String> getHistoriat() {
        return historiat;
    }

    public void setHistoriat(List<String> historiat) {
        this.historiat = historiat;
    }

    public int getJonosija() {
        return jonosija;
    }

    public void setJonosija(int jonosija) {
        this.jonosija = jonosija;
    }

    public String getKuvaus() {
        return kuvaus;
    }

    public void setKuvaus(String kuvaus) {
        this.kuvaus = kuvaus;
    }

    public String getHakemusoid() {
        return hakemusoid;
    }

    public void setHakemusoid(String hakemusoid) {
        this.hakemusoid = hakemusoid;
    }

    public String getHakijaoid() {
        return hakijaoid;
    }

    public void setHakijaoid(String hakijaoid) {
        this.hakijaoid = hakijaoid;
    }

    public String getEtunimi() {
        return etunimi;
    }

    public void setEtunimi(String etunimi) {
        this.etunimi = etunimi;
    }

    public String getSukunimi() {
        return sukunimi;
    }

    public void setSukunimi(String sukunimi) {
        this.sukunimi = sukunimi;
    }

    public TreeMap<Integer, Jarjestyskriteeritulos> getJarjestyskriteerit() {
        return jarjestyskriteerit;
    }

    public void setJarjestyskriteerit(TreeMap<Integer, Jarjestyskriteeritulos> jarjestyskriteerit) {
        this.jarjestyskriteerit = jarjestyskriteerit;
    }

    public int getPrioriteetti() {
        return prioriteetti;
    }

    public void setPrioriteetti(int prioriteetti) {
        this.prioriteetti = prioriteetti;
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
