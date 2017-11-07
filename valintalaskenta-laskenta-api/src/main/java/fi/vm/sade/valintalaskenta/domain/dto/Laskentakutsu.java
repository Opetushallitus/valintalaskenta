package fi.vm.sade.valintalaskenta.domain.dto;

import java.util.List;

public class Laskentakutsu {
    private final boolean isValintaryhmalaskenta;
    private final LaskeDTO laskeDTO;
    private final List<LaskeDTO> laskeDTOs;
    private final String uuid;
    private final String pollKey;

    public Laskentakutsu(LaskeDTO laskeDTO) {
        this.isValintaryhmalaskenta = false;
        this.laskeDTO = laskeDTO;
        this.laskeDTOs = null;
        this.uuid = laskeDTO.getUuid();
        this.pollKey = uuid+laskeDTO.getHakukohdeOid();
    }

    //Tulkitaan laskentakutsu valintaryhmälaskennaksi aina, jos parametri on lista laskeDTO-arvoja
    public Laskentakutsu(List<LaskeDTO> laskeDTOs) {
        this.isValintaryhmalaskenta = true;
        this.laskeDTO = null;
        this.laskeDTOs = laskeDTOs;
        this.uuid = laskeDTOs.iterator().next().getUuid();
        this.pollKey = uuid+"valintaryhmalaskenta";


    }

    public boolean isValintaryhmalaskenta() {
        return isValintaryhmalaskenta;
    }

    public LaskeDTO getLaskeDTO() {
        return laskeDTO;
    }

    public List<LaskeDTO> getLaskeDTOs() {
        return laskeDTOs;
    }

    public String getPollKey() {
        return pollKey;
    }

    public String getUuid() {
        return uuid;
    }

}
