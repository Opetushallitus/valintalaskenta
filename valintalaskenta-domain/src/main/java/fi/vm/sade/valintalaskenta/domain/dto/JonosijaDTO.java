package fi.vm.sade.valintalaskenta.domain.dto;

import fi.vm.sade.valintalaskenta.domain.JarjestyskriteerituloksenTila;
import fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos;

import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.TreeSet;

/**
 * Created with IntelliJ IDEA.
 * User: kkammone
 * Date: 13.5.2013
 * Time: 9:50
 * To change this template use File | Settings | File Templates.
 */
public class JonosijaDTO implements Comparable<JonosijaDTO> {

    private int jonosija;

    private String hakemusOid;

    private String hakijaOid;

    private Map<Integer, Jarjestyskriteeritulos> jarjestyskriteerit = new TreeMap<Integer, Jarjestyskriteeritulos>();

    private int prioriteetti;

    private String sukunimi;

    private String etunimi;

    private JarjestyskriteerituloksenTila tila;


    public Map<Integer, Jarjestyskriteeritulos> getJarjestyskriteerit() {
        return jarjestyskriteerit;
    }

    public void setJarjestyskriteerit(Map<Integer, Jarjestyskriteeritulos> jarjestyskriteerit) {
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

    @Override
    public int compareTo(JonosijaDTO other) {

        TreeSet<Integer> keys = new TreeSet<Integer>();
        keys.addAll(this.jarjestyskriteerit.keySet());
        keys.addAll(other.jarjestyskriteerit.keySet());
        for(Integer i : keys) {
            Jarjestyskriteeritulos thisValue =  null;
            if(this.getJarjestyskriteerit().containsKey(i)){
                thisValue = this.getJarjestyskriteerit().get(i);
            }
            Jarjestyskriteeritulos otherValue =  null;
            if(other.getJarjestyskriteerit().containsKey(i)){
                otherValue = this.getJarjestyskriteerit().get(i);
            }
            if(thisValue == null && otherValue == null) {
                continue;
            } else if(thisValue == null) {
                return -1;
            } else if(otherValue == null) {
                return 1;
            } else if(otherValue.getArvo() == thisValue.getArvo()) {
                continue;
            } else if(otherValue.getArvo() > thisValue.getArvo()) {
                return -1;
            }else if(otherValue.getArvo() < thisValue.getArvo()) {
                return  -1;
            }
        }
        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public int getJonosija() {
        return jonosija;
    }

    public void setJonosija(int jonosija) {
        this.jonosija = jonosija;
    }

    public void setTila(JarjestyskriteerituloksenTila tila) {
        this.tila = tila;
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

    public JarjestyskriteerituloksenTila getTila() {
        return tila;
    }
}
