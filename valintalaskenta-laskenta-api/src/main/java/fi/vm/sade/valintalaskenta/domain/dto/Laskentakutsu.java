package fi.vm.sade.valintalaskenta.domain.dto;

import java.util.List;

public class Laskentakutsu {
    private boolean isValintaryhmalaskenta;
    private LaskeDTO laskeDTO; //Tavalliset laskennat
    private List<LaskeDTO> laskeDTOs; //Valintaryhmälaskennat
    private SuoritustiedotDTO suoritustiedotDTO; // Koski-opiskeluoikeustiedot laskentoja varten. Sure-suoritukset täytyy asettaa hakemuksille jo valintalaskentakoostepalvelussa.
    private String uuid;
    private String pollKey;

    /**
     * Empty constructor needed for Jackson deserialization
     */
    public Laskentakutsu() {
    }

    public Laskentakutsu(LaskeDTO laskeDTO, SuoritustiedotDTO suoritustiedotDTO) {
        this.suoritustiedotDTO = suoritustiedotDTO;
        this.isValintaryhmalaskenta = false;
        this.laskeDTO = laskeDTO;
        this.laskeDTOs = null;
        this.uuid = laskeDTO.getUuid();
        this.pollKey = String.format("%s_%s", uuid, laskeDTO.getHakukohdeOid());
    }

    //Tulkitaan laskentakutsu valintaryhmälaskennaksi aina, jos parametri on lista laskeDTO-arvoja
    public Laskentakutsu(List<LaskeDTO> laskeDTOs, SuoritustiedotDTO suoritustiedotDTO) {
        this.suoritustiedotDTO = suoritustiedotDTO;
        this.isValintaryhmalaskenta = true;
        this.laskeDTO = null;
        this.laskeDTOs = laskeDTOs;
        this.uuid = laskeDTOs.iterator().next().getUuid();
        this.pollKey = uuid+"_valintaryhmalaskenta";
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

    /**
     * Jackson-deserialisointi vaatii tämän
     */
    public SuoritustiedotDTO getSuoritustiedotDTO() {
        return suoritustiedotDTO;
    }

    public void populoiSuoritustiedotLaskeDtoille() {
        if (laskeDTOs != null) {
            laskeDTOs.forEach(ldto -> ldto.populoiSuoritustiedotHakemuksille(suoritustiedotDTO));
        }
        if (laskeDTO != null) {
            laskeDTO.populoiSuoritustiedotHakemuksille(suoritustiedotDTO);
        }
    }
}
