package fi.vm.sade.valinta.kooste.util.sure;

import fi.vm.sade.valinta.kooste.external.resource.suoritusrekisteri.dto.Arvosana;
import fi.vm.sade.valintalaskenta.domain.dto.AvainArvoDTO;

public class ArvosanaJaAvainArvo implements Comparable<ArvosanaJaAvainArvo> {
  private final Arvosana arvosana;
  private final AvainArvoDTO avainArvoDTO;
  private final String avain;

  public ArvosanaJaAvainArvo(String avain, Arvosana arvosana, AvainArvoDTO avainArvoDTO) {
    this.arvosana = arvosana;
    this.avainArvoDTO = avainArvoDTO;
    this.avain = avain;
  }

  public ArvosanaJaAvainArvo(Arvosana arvosana, AvainArvoDTO avainArvoDTO) {
    this.arvosana = arvosana;
    this.avainArvoDTO = avainArvoDTO;
    this.avain = avainArvoDTO.getAvain();
  }

  @Override
  public boolean equals(Object obj) {
    if (avain == null || obj == null || !(obj instanceof ArvosanaJaAvainArvo)) {
      return false;
    } else {
      return avain.equals(((ArvosanaJaAvainArvo) obj).avain);
    }
  }

  @Override
  public int compareTo(ArvosanaJaAvainArvo o) {
    return avain.compareTo(o.avain);
  }

  public String getAvain() {
    return avain;
  }

  public Arvosana getArvosana() {
    return arvosana;
  }

  public AvainArvoDTO getAvainArvoDTO() {
    return avainArvoDTO;
  }
}
