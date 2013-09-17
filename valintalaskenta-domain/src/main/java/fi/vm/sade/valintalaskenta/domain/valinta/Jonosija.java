package fi.vm.sade.valintalaskenta.domain.valinta;

import com.google.code.morphia.annotations.Embedded;
import com.google.code.morphia.annotations.PrePersist;
import fi.vm.sade.valintalaskenta.domain.JsonViews;
import org.codehaus.jackson.map.annotate.JsonView;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

/**
 * User: wuoti
 * Date: 4.9.2013
 * Time: 10.26
 */
@Embedded
public class Jonosija {
    @JsonView(JsonViews.Basic.class)
    private String hakemusOid;

    @JsonView(JsonViews.Basic.class)
    private String hakijaOid;

    @JsonView(JsonViews.Basic.class)
    private String etunimi;

    @JsonView(JsonViews.Basic.class)
    private String sukunimi;

    @JsonView(JsonViews.Basic.class)
    private int hakutoiveprioriteetti; // hakutoive

    @JsonView(JsonViews.Basic.class)
    private boolean harkinnanvarainen = false;

    @Embedded
    private List<Jarjestyskriteeritulos> jarjestyskriteeritulokset = new ArrayList<Jarjestyskriteeritulos>();

    @Embedded
    private List<SyotettyArvo> syotetytArvot = new ArrayList<SyotettyArvo>();

    public String getHakemusOid() {
        return hakemusOid;
    }

    public void setHakemusOid(String hakemusOid) {
        this.hakemusOid = hakemusOid;
    }

    public String getHakijaOid() {
        return hakijaOid;
    }

    public void setHakijaOid(String hakijaOid) {
        this.hakijaOid = hakijaOid;
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

    public int getHakutoiveprioriteetti() {
        return hakutoiveprioriteetti;
    }

    public void setHakutoiveprioriteetti(int hakutoiveprioriteetti) {
        this.hakutoiveprioriteetti = hakutoiveprioriteetti;
    }

    public boolean isHarkinnanvarainen() {
        return harkinnanvarainen;
    }

    public void setHarkinnanvarainen(boolean harkinnanvarainen) {
        this.harkinnanvarainen = harkinnanvarainen;
    }

    public List<Jarjestyskriteeritulos> getJarjestyskriteeritulokset() {
        return jarjestyskriteeritulokset;
    }

    public void setJarjestyskriteeritulokset(List<Jarjestyskriteeritulos> jarjestyskriteeritulokset) {
        this.jarjestyskriteeritulokset = jarjestyskriteeritulokset;
    }

    @PrePersist
    private void jarjestaJarjestyskriteeritulokset() {
        Collections.sort(jarjestyskriteeritulokset, new Comparator<Jarjestyskriteeritulos>() {
            @Override
            public int compare(Jarjestyskriteeritulos o1, Jarjestyskriteeritulos o2) {
                return o1.getPrioriteetti() - o2.getPrioriteetti();
            }
        });
    }

    public List<SyotettyArvo> getSyotetytArvot() {
        return syotetytArvot;
    }

    public void setSyotetytArvot(List<SyotettyArvo> syotetytArvot) {
        this.syotetytArvot = syotetytArvot;
    }
}
