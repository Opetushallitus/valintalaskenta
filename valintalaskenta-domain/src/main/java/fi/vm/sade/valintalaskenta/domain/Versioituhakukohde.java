package fi.vm.sade.valintalaskenta.domain;

import com.google.code.morphia.annotations.Embedded;

/**
 * 
 * @author Jussi Jartamo
 * 
 *         Hakukohde järjestysnumerolla. Tätä käytetään yhdessä sortedsetin
 *         kanssa niin että ensimmäinen tai viimeinen alkio on helposti
 *         saatavissa.
 * 
 */
@Embedded("VersioituHakukohde")
public class Versioituhakukohde implements Comparable<Versioituhakukohde> {

    private Long versio;

    @Embedded
    private Hakukohde hakukohde;

    public Hakukohde getHakukohde() {
        return hakukohde;
    }

    public void setHakukohde(Hakukohde hakukohde) {
        this.hakukohde = hakukohde;
    }

    public Long getVersio() {
        return versio;
    }

    public void setVersio(Long versio) {
        this.versio = versio;
    }

    public int compareTo(Versioituhakukohde o) {
        if (equals(o)) {
            return 0;
        }
        return versio.compareTo(o.versio);
    }

    public boolean equals(Object obj) {
        if (obj instanceof Versioituhakukohde) {
            Versioituhakukohde vh = (Versioituhakukohde) obj;
            return this == vh;
        }
        return false;
    }

    public int hashCode() {
        return versio.intValue();
    }
}
