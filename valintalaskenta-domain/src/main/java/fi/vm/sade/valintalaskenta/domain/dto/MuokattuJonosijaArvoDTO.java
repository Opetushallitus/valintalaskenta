package fi.vm.sade.valintalaskenta.domain.dto;

import com.wordnik.swagger.annotations.ApiModel;
import com.wordnik.swagger.annotations.ApiModelProperty;
import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila;

import java.math.BigDecimal;

/**
 * User: jukais
 * Date: 15.8.2013
 * Time: 7.33
 */
@ApiModel(value="MuokattuJonosijaArvoDTO", description = "Muokattu jonosija")
public class MuokattuJonosijaArvoDTO {

    @ApiModelProperty(value="Tila", required = true)
    private JarjestyskriteerituloksenTila tila;

    @ApiModelProperty(value="Arvo", required = true)
    private BigDecimal arvo;

    @ApiModelProperty(value="Selite", required = true)
    private String selite;

    public JarjestyskriteerituloksenTila getTila() {
        return tila;
    }

    public void setTila(JarjestyskriteerituloksenTila tila) {
        this.tila = tila;
    }

    public BigDecimal getArvo() {
        return arvo;
    }

    public void setArvo(BigDecimal arvo) {
        this.arvo = arvo;
    }

    public String getSelite() {
        return selite;
    }

    public void setSelite(String selite) {
        this.selite = selite;
    }
}
