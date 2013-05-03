package fi.vm.sade.valintalaskenta.domain.valintakoe;

import com.google.code.morphia.annotations.Embedded;
import com.google.code.morphia.annotations.Entity;

import java.util.ArrayList;
import java.util.List;

/**
 * User: wuoti
 * Date: 2.5.2013
 * Time: 13.02
 */
@Entity("ValintakoeOsallistuminen")
public class ValintakoeOsallistuminen {

    private String hakuOid;
    private String hakemusOid;
    private String hakijaOid;

    @Embedded
    private List<Hakutoive> hakutoiveet = new ArrayList<Hakutoive>();

    public String getHakuOid() {
        return hakuOid;
    }

    public String getHakemusOid() {
        return hakemusOid;
    }

    public String getHakijaOid() {
        return hakijaOid;
    }

    public List<Hakutoive> getHakutoiveet() {
        return hakutoiveet;
    }
}
