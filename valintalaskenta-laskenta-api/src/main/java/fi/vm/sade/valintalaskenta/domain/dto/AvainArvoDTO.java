package fi.vm.sade.valintalaskenta.domain.dto;

import org.apache.commons.lang.builder.HashCodeBuilder;

import java.util.Optional;

/**
 * Created by jukais on 20.3.2014.
 */
public class AvainArvoDTO {
    private String avain;
    private String arvo;

    public AvainArvoDTO() {}

    public AvainArvoDTO(String avain, String arvo) {
        this.avain = avain;
        this.arvo = arvo;
    }

    public String getAvain() {
        return avain;
    }

    public void setAvain(String avain) {
        this.avain = avain;
    }

    public String getArvo() {
        return arvo;
    }

    public void setArvo(String arvo) {
        this.arvo = arvo;
    }

    @Override
    public String toString() {
        return "AvainArvoDTO(" + avain + ", " + arvo + ")";
    }

    @Override
    public int hashCode() {
        return Optional.ofNullable(avain).orElse("").hashCode() + 27 * Optional.ofNullable(arvo).orElse("").hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if(obj == null || !(obj instanceof AvainArvoDTO)) {
            return false;
        } else {
            AvainArvoDTO a = (AvainArvoDTO)obj;
            return Optional.ofNullable(avain).orElse("").equals(a.avain) &&
                    Optional.ofNullable(arvo).orElse("").equals(a.arvo);
        }
    }
}
