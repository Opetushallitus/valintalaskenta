package fi.vm.sade.valinta.kooste.external.resource.organisaatio.dto;

import java.util.List;
import java.util.Map;

public class Metadata {
  private List<Yhteystieto> yhteystiedot;
  private Map<String, String> hakutoimistonNimi;

  public List<Yhteystieto> getYhteystiedot() {
    return yhteystiedot;
  }

  public void setYhteystiedot(List<Yhteystieto> yhteystiedot) {
    this.yhteystiedot = yhteystiedot;
  }

  public Map<String, String> getHakutoimistonNimi() {
    return hakutoimistonNimi;
  }

  public void setHakutoimistonNimi(Map<String, String> hakutoimistonNimi) {
    this.hakutoimistonNimi = hakutoimistonNimi;
  }
}
