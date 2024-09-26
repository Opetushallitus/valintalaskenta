package fi.vm.sade.valintalaskenta.domain.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.google.gson.Gson;
import fi.vm.sade.valintalaskenta.domain.GzipUtil;
import java.util.Base64;
import java.util.LinkedList;
import java.util.List;

@JsonIgnoreProperties(ignoreUnknown = true)
public class Laskentakutsu {
  private static final Gson GSON = new Gson();

  private boolean isValintaryhmalaskenta;
  private LaskeDTO laskeDTO; // Tavalliset laskennat
  private List<LaskeDTO> laskeDTOs; // Valintaryhmälaskennat
  // täytyy asettaa hakemuksille jo
  // valintalaskentakoostepalvelussa.
  private String uuid;
  private String pollKey;

  /** Empty constructor needed for Jackson deserialization */
  public Laskentakutsu() {}

  public Laskentakutsu(LaskeDTO laskeDTO) {
    this.isValintaryhmalaskenta = false;
    this.laskeDTO = laskeDTO;
    this.laskeDTOs = null;
    this.uuid = laskeDTO.getUuid();
    this.pollKey = String.format("%s_%s", uuid, laskeDTO.getHakukohdeOid());
  }

  private Laskentakutsu(boolean isValintaryhmalaskenta, String uuid) {
    this.isValintaryhmalaskenta = isValintaryhmalaskenta;
    this.laskeDTO = null;
    this.uuid = uuid;
    this.pollKey = uuid + "_valintaryhmalaskenta";
  }

  public static Laskentakutsu valintaRyhmaLaskentaKutsu(List<LaskeDTO> laskeDTOs) {
    Laskentakutsu laskentakutsu = new Laskentakutsu(true, laskeDTOs.iterator().next().getUuid());
    laskentakutsu.laskeDTOs = laskeDTOs;
    return laskentakutsu;
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
