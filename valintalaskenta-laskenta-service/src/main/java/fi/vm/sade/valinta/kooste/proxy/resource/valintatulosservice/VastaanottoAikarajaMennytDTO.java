package fi.vm.sade.valinta.kooste.proxy.resource.valintatulosservice;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import org.joda.time.DateTime;

@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
public class VastaanottoAikarajaMennytDTO {
  private String hakemusOid;
  private boolean mennyt;
  private DateTime vastaanottoDeadline;

  public String getHakemusOid() {
    return hakemusOid;
  }

  public void setHakemusOid(String hakemusOid) {
    this.hakemusOid = hakemusOid;
  }

  public boolean isMennyt() {
    return mennyt;
  }

  public void setMennyt(boolean mennyt) {
    this.mennyt = mennyt;
  }

  public DateTime getVastaanottoDeadline() {
    return vastaanottoDeadline;
  }

  public void setVastaanottoDeadline(DateTime vastaanottoDeadline) {
    this.vastaanottoDeadline = vastaanottoDeadline;
  }
}
