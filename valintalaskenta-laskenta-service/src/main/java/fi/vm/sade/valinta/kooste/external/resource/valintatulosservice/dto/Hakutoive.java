package fi.vm.sade.valinta.kooste.external.resource.valintatulosservice.dto;

import java.util.Map;

public class Hakutoive {
  private Map<String, String> hakemusData;
  private String hakuaikaId;

  public Hakutoive(Map<String, String> hakemusData, String hakuaikaId) {
    this.hakemusData = hakemusData;
    this.hakuaikaId = hakuaikaId;
  }

  public Map<String, String> getHakemusData() {
    return hakemusData;
  }

  public String getHakuaikaId() {
    return hakuaikaId;
  }
}
