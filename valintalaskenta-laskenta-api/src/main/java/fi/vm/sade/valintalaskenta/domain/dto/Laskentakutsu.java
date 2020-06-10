package fi.vm.sade.valintalaskenta.domain.dto;

import com.google.gson.Gson;

import fi.vm.sade.valintalaskenta.domain.GzipUtil;
import org.apache.commons.io.IOUtils;

import java.io.IOException;
import java.util.Base64;
import java.util.LinkedList;
import java.util.List;

public class Laskentakutsu {
    private static final Gson GSON = new Gson();

    private boolean isValintaryhmalaskenta;
    private LaskeDTO laskeDTO; //Tavalliset laskennat
    private List<LaskeDTO> laskeDTOs; //Valintaryhmälaskennat
    private String suoritustiedotDtoBase64Gzip; // Koski-opiskeluoikeustiedot laskentoja varten. Sure-suoritukset täytyy asettaa hakemuksille jo valintalaskentakoostepalvelussa.
    private String uuid;
    private String pollKey;

    /**
     * Empty constructor needed for Jackson deserialization
     */
    public Laskentakutsu() {
    }

    public Laskentakutsu(LaskeDTO laskeDTO, SuoritustiedotDTO suoritustiedotDTO) {
        this.suoritustiedotDtoBase64Gzip = toBase64Gzip(suoritustiedotDTO);
        this.isValintaryhmalaskenta = false;
        this.laskeDTO = laskeDTO;
        this.laskeDTOs = null;
        this.uuid = laskeDTO.getUuid();
        this.pollKey = String.format("%s_%s", uuid, laskeDTO.getHakukohdeOid());
    }

    private Laskentakutsu(boolean isValintaryhmalaskenta, String uuid) {
        this.isValintaryhmalaskenta = true;
        this.laskeDTO = null;
        this.uuid = uuid;
        this.pollKey = uuid+"_valintaryhmalaskenta";
    }

    //Tulkitaan laskentakutsu valintaryhmälaskennaksi aina, jos parametri on lista laskeDTO-arvoja

    public Laskentakutsu(List<LaskeDTO> laskeDTOs, SuoritustiedotDTO suoritustiedotDTO) {
        this(true, laskeDTOs.iterator().next().getUuid());
        this.suoritustiedotDtoBase64Gzip = toBase64Gzip(suoritustiedotDTO);
        this.laskeDTOs = laskeDTOs;
    }

    public static String toBase64Gzip(SuoritustiedotDTO suoritustiedotDTO) {
        try {
            return IOUtils.toString(Base64.getEncoder().encode(GzipUtil.enkoodaa(GSON.toJson(suoritustiedotDTO))));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private static SuoritustiedotDTO fromBase64Gzip(String suoritustiedotDtoBase64Gzip) {
        return GSON.fromJson(GzipUtil.dekoodaa(Base64.getDecoder().decode(suoritustiedotDtoBase64Gzip)), SuoritustiedotDTO.class);
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
    public String getSuoritustiedotDtoBase64Gzip() {
        return suoritustiedotDtoBase64Gzip;
    }

    public void setSuoritustiedotDtoBase64Gzip(String suoritustiedotDtoBase64Gzip) {
        this.suoritustiedotDtoBase64Gzip = suoritustiedotDtoBase64Gzip;
    }

    public void populoiSuoritustiedotLaskeDtoille() {
        SuoritustiedotDTO suoritustiedotDTO = fromBase64Gzip(suoritustiedotDtoBase64Gzip);
        if (laskeDTOs != null) {
            laskeDTOs.forEach(ldto -> ldto.populoiSuoritustiedotHakemuksille(suoritustiedotDTO));
        }
        if (laskeDTO != null) {
            laskeDTO.populoiSuoritustiedotHakemuksille(suoritustiedotDTO);
        }
    }

    public synchronized void lisaaLaskeDto(LaskeDTO laskeDto) {
        if (laskeDTOs == null) {
            laskeDTOs = new LinkedList<>();
        }
        laskeDTOs.add(laskeDto);
    }

    public static Laskentakutsu luoTyhjaValintaryhmaLaskentaPalasissaSiirtoaVarten(String uuid) {
        return new Laskentakutsu(true, uuid);
    }
}
