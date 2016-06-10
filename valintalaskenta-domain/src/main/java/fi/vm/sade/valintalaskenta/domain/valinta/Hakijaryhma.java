package fi.vm.sade.valintalaskenta.domain.valinta;

import org.bson.types.ObjectId;
import org.mongodb.morphia.annotations.*;

import java.util.*;

@Entity(value = "Hakijaryhma", noClassnameStored = true)
public class Hakijaryhma {
    @Id
    private ObjectId id;

    @Indexed
    private String hakijaryhmaOid;

    private int prioriteetti;

    private Date createdAt;

    @Indexed(unique = false, dropDups = false)
    private String hakukohdeOid;

    private String nimi;

    private String kuvaus;

    private int kiintio;

    private boolean kaytaKaikki;

    private boolean tarkkaKiintio;

    private boolean kaytetaanRyhmaanKuuluvia;

    private String valintatapajonoOid;

    private List<ObjectId> jonosijaIdt;

    @Transient
    private List<Jonosija> jonosijat;

    @PrePersist
    private void prePersist() {
        createdAt = new Date();
    }

    public void setId(ObjectId id) {
        this.id = id;
    }

    public Date getCreatedAt() {
        return createdAt;
    }

    public void setCreatedAt(Date createdAt) {
        this.createdAt = createdAt;
    }

    public String getHakukohdeOid() {
        return hakukohdeOid;
    }

    public void setHakukohdeOid(String hakukohdeOid) {
        this.hakukohdeOid = hakukohdeOid;
    }

    public String getNimi() {
        return nimi;
    }

    public void setNimi(String nimi) {
        this.nimi = nimi;
    }

    public String getHakijaryhmaOid() {
        return hakijaryhmaOid;
    }

    public void setHakijaryhmaOid(String hakijaryhmaOid) {
        this.hakijaryhmaOid = hakijaryhmaOid;
    }

    public int getPrioriteetti() {
        return prioriteetti;
    }

    public void setPrioriteetti(int prioriteetti) {
        this.prioriteetti = prioriteetti;
    }

    public String getKuvaus() {
        return kuvaus;
    }

    public void setKuvaus(String kuvaus) {
        this.kuvaus = kuvaus;
    }

    public int getKiintio() {
        return kiintio;
    }

    public void setKiintio(int kiintio) {
        this.kiintio = kiintio;
    }

    public boolean isKaytaKaikki() {
        return kaytaKaikki;
    }

    public void setKaytaKaikki(boolean kaytaKaikki) {
        this.kaytaKaikki = kaytaKaikki;
    }

    public boolean isTarkkaKiintio() {
        return tarkkaKiintio;
    }

    public void setTarkkaKiintio(boolean tarkkaKiintio) {
        this.tarkkaKiintio = tarkkaKiintio;
    }

    public boolean isKaytetaanRyhmaanKuuluvia() {
        return kaytetaanRyhmaanKuuluvia;
    }

    public void setKaytetaanRyhmaanKuuluvia(boolean kaytetaanRyhmaanKuuluvia) {
        this.kaytetaanRyhmaanKuuluvia = kaytetaanRyhmaanKuuluvia;
    }

    public String getValintatapajonoOid() {
        return valintatapajonoOid;
    }

    public void setValintatapajonoOid(String valintatapajonoOid) {
        this.valintatapajonoOid = valintatapajonoOid;
    }

    public List<ObjectId> getJonosijaIdt() {
        return jonosijaIdt;
    }

    public void setJonosijaIdt(List<ObjectId> jonosijaIdt) {
        this.jonosijaIdt = jonosijaIdt;
    }

    public List<Jonosija> getJonosijat() {
        if (null == jonosijat) {
            throw new IllegalStateException("Jonosijat not loaded");
        }
        return jonosijat;
    }

    public void setJonosijat(List<Jonosija> jonosijat) {
        this.jonosijat = jonosijat;
    }
}
