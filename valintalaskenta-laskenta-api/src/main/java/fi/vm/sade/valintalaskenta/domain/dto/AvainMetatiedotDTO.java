package fi.vm.sade.valintalaskenta.domain.dto;

import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * @author Jussi Jartamo
 */
public class AvainMetatiedotDTO {
    private final String avain;
    private final List<Map<String, String>> metatiedot;

    public AvainMetatiedotDTO() {
        this.avain = "";
        this.metatiedot = Collections.emptyList();
    }
    public AvainMetatiedotDTO(String avain, List<Map<String, String>> metatiedot) {
        if(avain == null) {
            this.avain = "";
        } else {
            this.avain = avain;
        }
        this.metatiedot = metatiedot;
    }

    public List<Map<String, String>> getSuoritustiedot() {
        return metatiedot;
    }

    public String getAvain() {
        return avain;
    }
}
