package fi.vm.sade.valintalaskenta.domain.dto;

import com.wordnik.swagger.annotations.ApiModel;
import com.wordnik.swagger.annotations.ApiModelProperty;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Jussi Jartamo
 *         <p/>
 *         Yhden hakemuksen tulokset
 */
@ApiModel(value = "HakemusDTO", description = "Yhden hakemuksen tiedot")
public class HakemusDTO {

    @ApiModelProperty(value = "Haku OID", required = true)
    private String hakuoid; // mika haku

    @ApiModelProperty(value = "Hakemus OID", required = true)
    private String hakemusoid;

    @ApiModelProperty(value = "Hakutoiveet", required = true)
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
