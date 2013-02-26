package fi.vm.sade.service.valintaperusteet.algoritmi.domain;

import com.google.code.morphia.annotations.Embedded;
import com.google.code.morphia.annotations.Entity;

/**
 * 
 * @author Jussi Jartamo
 * 
 *         Hakukohde järjestysnumerolla. Tätä käytetään yhdessä sortedsetin
 *         kanssa niin että ensimmäinen tai viimeinen alkio on helposti
 *         saatavissa.
 * 
 */
@Entity("VersioituHakukohde")
public class Versioituhakukohde implements Comparable<Versioituhakukohde> {

    private Integer versio;

    @Override
    public int compareTo(Versioituhakukohde o) {
        return versio.compareTo(o.versio);
    }

    @Embedded
    private Hakukohde hakukohde;

    public Hakukohde getHakukohde() {
        return hakukohde;
    }

    public void setHakukohde(Hakukohde hakukohde) {
        this.hakukohde = hakukohde;
    }

    public Integer getVersio() {
        return versio;
    }

    public void setVersio(Integer versio) {
        this.versio = versio;
    }
}
