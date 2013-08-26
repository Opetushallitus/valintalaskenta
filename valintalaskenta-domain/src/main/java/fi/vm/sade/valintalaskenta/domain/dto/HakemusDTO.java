package fi.vm.sade.valintalaskenta.domain.dto;

import java.util.ArrayList;
import java.util.List;

/**
 * 
 * @author Jussi Jartamo
 * 
 *         Yhden hakemuksen tulokset
 */
public class HakemusDTO {

    private String hakuoid; // mika haku
    private String hakemusoid;
    private List<HakukohdeDTO> hakukohteet;

    public HakemusDTO(String hakuoid, String hakemusoid, List<HakukohdeDTO> hakukohteet) {
        this.hakuoid = hakuoid;
        this.hakemusoid = hakemusoid;
        this.hakukohteet = hakukohteet;
    }

    public HakemusDTO() {
        hakukohteet = new ArrayList<HakukohdeDTO>();
    }

    public List<HakukohdeDTO> getHakukohteet() {
        return hakukohteet;
    }

    public String getHakemusoid() {
        return hakemusoid;
    }

    public String getHakuoid() {
        return hakuoid;
    }

}
