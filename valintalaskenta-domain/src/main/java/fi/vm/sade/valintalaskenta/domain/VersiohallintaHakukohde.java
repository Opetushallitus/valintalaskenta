package fi.vm.sade.valintalaskenta.domain;

import java.util.SortedSet;
import java.util.TreeSet;

import org.bson.types.ObjectId;

import com.google.code.morphia.annotations.Embedded;
import com.google.code.morphia.annotations.Entity;
import com.google.code.morphia.annotations.Id;
import com.google.code.morphia.annotations.Indexed;

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

    @Id
    private ObjectId id;
    @Indexed
    private String hakuoid;
    @Indexed
    private String hakukohdeoid;
    @Indexed
    private String valinnanvaiheoid;

    private int jarjestysnumero;

    @Embedded(concreteClass = java.util.TreeSet.class)
    private SortedSet<Versioituhakukohde> hakukohteet = new TreeSet<Versioituhakukohde>();

    public SortedSet<Versioituhakukohde> getHakukohteet() {
        return hakukohteet;
    }

    public String getValinnanvaiheoid() {
        return valinnanvaiheoid;
    }

    public void setValinnanvaiheoid(String valinnanvaiheoid) {
        this.valinnanvaiheoid = valinnanvaiheoid;
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

    public ObjectId getId() {
        return id;
    }

    public void setId(ObjectId id) {
        this.id = id;
    }

}
