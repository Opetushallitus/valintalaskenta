package fi.vm.sade.valinta.kooste.external.resource.valintatulosservice.dto;

public class Lukuvuosimaksu {
  private String personOid;
  private Maksuntila maksuntila;

  public Maksuntila getMaksuntila() {
    return maksuntila;
  }

  public void setMaksuntila(Maksuntila maksuntila) {
    this.maksuntila = maksuntila;
  }

  public String getPersonOid() {
    return personOid;
  }

  public void setPersonOid(String personOid) {
    this.personOid = personOid;
  }
}
