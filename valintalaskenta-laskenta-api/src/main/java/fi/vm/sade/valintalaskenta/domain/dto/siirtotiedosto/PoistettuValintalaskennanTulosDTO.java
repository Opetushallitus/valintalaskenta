package fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto;

import java.util.ArrayList;
import java.util.List;

public class PoistettuValintalaskennanTulosDTO {
  private List<ValinnanVaihe> poistetut = new ArrayList<>();

  public List<ValinnanVaihe> getPoistetut() {
    return poistetut;
  }

  public PoistettuValintalaskennanTulosDTO setPoistetut(List<ValinnanVaihe> poistetut) {
    this.poistetut = poistetut;
    return this;
  }

  public static class PoistettuItselfOrJustParent {
    protected Boolean poistettu = Boolean.TRUE;

    public Boolean isPoistettu() {
      return poistettu;
    }

    public void setPoistettuItself(boolean poistettuItself) {
      this.poistettu = poistettuItself ? Boolean.TRUE : null;
    }
  }

  public static class ValinnanVaihe extends PoistettuItselfOrJustParent {
    private String valinnanvaiheoid;
    private List<ValintatapaJono> valintatapajonot;

    public String getValinnanvaiheoid() {
      return valinnanvaiheoid;
    }

    public ValinnanVaihe setValinnanvaiheoid(String valinnanvaiheoid) {
      this.valinnanvaiheoid = valinnanvaiheoid;
      return this;
    }

    public List<ValintatapaJono> getValintatapajonot() {
      return valintatapajonot;
    }

    public ValinnanVaihe setValintatapajonot(List<ValintatapaJono> valintatapajonot) {
      this.valintatapajonot = valintatapajonot;
      return this;
    }
  }

  public static class ValintatapaJono extends PoistettuItselfOrJustParent {
    private String valintatapajonooid;
    private List<Jonosija> jonosijat;

    public String getValintatapajonooid() {
      return valintatapajonooid;
    }

    public ValintatapaJono setValintatapajonooid(String valintatapajonooid) {
      this.valintatapajonooid = valintatapajonooid;
      return this;
    }

    public List<Jonosija> getJonosijat() {
      return jonosijat;
    }

    public ValintatapaJono setJonosijat(List<Jonosija> jonosijat) {
      this.jonosijat = jonosijat;
      return this;
    }
  }

  public static class Jonosija extends PoistettuItselfOrJustParent {
    private String hakemusOid;

    public String getHakemusOid() {
      return hakemusOid;
    }

    public Jonosija setHakemusOid(String hakemusOid) {
      this.hakemusOid = hakemusOid;
      return this;
    }
  }
}
