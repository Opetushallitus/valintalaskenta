package fi.vm.sade.valintalaskenta.domain.dto;

import com.wordnik.swagger.annotations.ApiModel;
import com.wordnik.swagger.annotations.ApiModelProperty;

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
