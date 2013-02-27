package fi.vm.sade.service.valintaperusteet.algoritmi.domain;

import java.util.SortedSet;
import java.util.TreeSet;

import com.google.code.morphia.annotations.Embedded;
import com.google.code.morphia.annotations.Entity;

/**
 * 
 * @author Jussi Jartamo
 * 
 *         Versioi yhden (hakukohdeoid + järjestysnumero) avaimen hakukohteita.
 *         Toteutettu järjestetyllä joukolla jossa viimeisenä
 *         (hakukohteet.last()) on uusimmalla aikaleimalla lisätty
 *         valinnanvaihe.
 * 
 */
@Entity("Haku")
public class VersiohallintaHakukohde {

    private String hakuoid;
    private String hakukohdeoid;
    private int jarjestysnumero;

    @Embedded(concreteClass = java.util.TreeSet.class)
    private SortedSet<Versioituhakukohde> hakukohteet = new TreeSet<Versioituhakukohde>();

    public SortedSet<Versioituhakukohde> getHakukohteet() {
        return hakukohteet;
    }

    public String getHakuoid() {
        return hakuoid;
    }

    public void setHakuoid(String hakuoid) {
        this.hakuoid = hakuoid;
    }

    public void setHakukohteet(SortedSet<Versioituhakukohde> hakukohteet) {
        this.hakukohteet = hakukohteet;
    }

    public int getJarjestysnumero() {
        return jarjestysnumero;
    }

    public void setJarjestysnumero(int jarjestysnumero) {
        this.jarjestysnumero = jarjestysnumero;
    }

    public String getHakukohdeoid() {
        return hakukohdeoid;
    }

    public void setHakukohdeoid(String hakukohdeoid) {
        this.hakukohdeoid = hakukohdeoid;
    }
}
