package fi.vm.sade.valintalaskenta.domain.valinta;

import com.google.code.morphia.annotations.Entity;
import com.google.code.morphia.annotations.Id;
import org.bson.types.ObjectId;

/**
 * Created with IntelliJ IDEA.
 * User: kkammone
 * Date: 12.9.2013
 * Time: 14:23
 * To change this template use File | Settings | File Templates.
 */
@Entity("MuokattuJonosija")
public class HarkinnanvarainenHyvaksyminen {

    @Id
    private ObjectId id;

    private boolean hyvaksyttyHarkinnanvaraisesti = false;

    private String hakukohdeOid;

    private String hakemusOid;


    public boolean isHyvaksyttyHarkinnanvaraisesti() {
        return hyvaksyttyHarkinnanvaraisesti;
    }

    public void setHyvaksyttyHarkinnanvaraisesti(boolean hyvaksyttyHarkinnanvaraisesti) {
        this.hyvaksyttyHarkinnanvaraisesti = hyvaksyttyHarkinnanvaraisesti;
    }

    public String getHakemusOid() {
        return hakemusOid;
    }

    public void setHakemusOid(String hakemusOid) {
        this.hakemusOid = hakemusOid;
    }

    public String getHakukohdeOid() {
        return hakukohdeOid;
    }

    public void setHakukohdeOid(String hakukohdeOid) {
        this.hakukohdeOid = hakukohdeOid;
    }
}
