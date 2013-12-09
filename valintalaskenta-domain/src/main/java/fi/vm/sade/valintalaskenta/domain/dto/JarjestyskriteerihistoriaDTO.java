package fi.vm.sade.valintalaskenta.domain.dto;

import com.wordnik.swagger.annotations.ApiModel;
import com.wordnik.swagger.annotations.ApiModelProperty;

/**
 * User: wuoti
 * Date: 9.12.2013
 * Time: 9.08
 */
@ApiModel(value = "JarjestyskriteerihistoriaDTO", description = "Järjestyskriteerihistoria")
public class JarjestyskriteerihistoriaDTO {

    @ApiModelProperty(value = "Historia JSON", required = true)
    private String historia;

    public String getHistoria() {
        return historia;
    }

    public void setHistoria(String historia) {
        this.historia = historia;
    }
}
