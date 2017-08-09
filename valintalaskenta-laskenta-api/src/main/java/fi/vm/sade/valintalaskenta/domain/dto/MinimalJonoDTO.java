package fi.vm.sade.valintalaskenta.domain.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

@ApiModel(value = "valintalaskenta.domain.dto.MinimalJonoDTO", description = "Valintatapajono, jossa vain pakolliset kentät ODWlle")
public class MinimalJonoDTO {
    private static final Logger LOGGER = LoggerFactory.getLogger(MinimalJonoDTO.class);

    @ApiModelProperty(value = "Haku OID", required = true)
    private String hakuOid;

    @ApiModelProperty(value = "Hakukohde OID", required = true)
    private String hakukohdeOid;

    @ApiModelProperty(value = "Valintatapajono OID", required = true)
    private String valintatapajonoOid;

    @ApiModelProperty(value = "Hakemusten määrä jonossa", required = true)
    private int hakemusCount;

    @ApiModelProperty(value = "Jono käyttää valintalaskentaa", required = true)
    private final boolean kaytetaanValintalaskentaa;

    @ApiModelProperty(value = "Jono on siirretty sijoitteluun", required = true)
    private final boolean siirretaanSijoitteluun;


    public MinimalJonoDTO(String hakuOid, String hakukohdeOid, String valintatapajonoOid, List<JonosijaDTO> jonosijaDTOS, boolean kaytetaanValintalaskentaa, boolean siirretaanSijoitteluun) {
        this.hakuOid = hakuOid;
        this.hakukohdeOid = hakukohdeOid;
        this.valintatapajonoOid = valintatapajonoOid;
        this.kaytetaanValintalaskentaa = kaytetaanValintalaskentaa;
        this.siirretaanSijoitteluun = siirretaanSijoitteluun;

        if(jonosijaDTOS == null){
            LOGGER.warn("Valintatapajono {} had null jonosijas.", valintatapajonoOid);
            this.hakemusCount = 0;
        } else {
            this.hakemusCount = jonosijaDTOS.size();
        }
    }

    public String getHakuOid() {
        return hakuOid;
    }

    public String getHakukohdeOid() {
        return hakukohdeOid;
    }

    public String getValintatapajonoOid() {
        return valintatapajonoOid;
    }

    public int getHakemusCount() {
        return hakemusCount;
    }

    public boolean isKaytetaanValintalaskentaa() {
        return kaytetaanValintalaskentaa;
    }

    public boolean isSiirretaanSijoitteluun() {
        return siirretaanSijoitteluun;
    }
}
