/**package fi.vm.sade.valintalaskenta.laskenta;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakemus;


 * 
 * @author Jussi Jartamo
 * 
 *         Apuluokka hakemusten käsittelyyn. Tarjoaa säiliön ja rajapinnan sekä
 *         hakemustyypin että hakemuksen tietojen hakuun hakemusoid:lla.
X
public class HakemusHelper {

    // hakemusoid -> hakemustyyppi
    private Map<String, HakemusTyyppi> hakemustyypit = new HashMap<String, HakemusTyyppi>();
    // hakemusoid -> laskennan hakemus
    private Map<String, Hakemus> laskentahakemukset = new HashMap<String, Hakemus>();

    public void setHakemusOidHakemukset(String hakemusOid, Hakemus hakemus, HakemusTyyppi hakemusTyyppi) {
        laskentahakemukset.put(hakemusOid, hakemus);
        hakemustyypit.put(hakemusOid, hakemusTyyppi);
    }

    public String getEtunimi(String hakemusOid) {
        return hakemustyypit.get(hakemusOid).getHakijanEtunimi();
    }

    public String getSukunimi(String hakemusOid) {
        return hakemustyypit.get(hakemusOid).getHakijanSukunimi();
    }

    public Collection<Hakemus> getHakemukset() {
        return laskentahakemukset.values();
    }
}
*/