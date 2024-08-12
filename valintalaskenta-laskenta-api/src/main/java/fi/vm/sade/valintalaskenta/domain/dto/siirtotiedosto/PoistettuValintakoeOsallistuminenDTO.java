package fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto;

import fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto.PoistettuValintalaskennanTulosDTO.PoistettuItselfOrJustParent;
import java.util.ArrayList;
import java.util.List;

public class PoistettuValintakoeOsallistuminenDTO {
  private List<ValintakoeOsallistuminen> poistetut = new ArrayList<>();

  public List<ValintakoeOsallistuminen> getPoistetut() {
    return poistetut;
  }

  public PoistettuValintakoeOsallistuminenDTO setPoistetut(
      List<ValintakoeOsallistuminen> poistetut) {
    this.poistetut = poistetut;
    return this;
  }

  public static class ValintakoeOsallistuminen extends PoistettuItselfOrJustParent {
    private String hakemusOid;
    private List<Hakutoive> hakutoiveet;

    public String getHakemusOid() {
      return hakemusOid;
    }

    public ValintakoeOsallistuminen setHakemusOid(String hakemusOid) {
      this.hakemusOid = hakemusOid;
      return this;
    }

    public List<Hakutoive> getHakutoiveet() {
      return hakutoiveet;
    }

    public ValintakoeOsallistuminen setHakutoiveet(List<Hakutoive> hakutoiveet) {
      this.hakutoiveet = hakutoiveet;
      return this;
    }
  }

  public static class Hakutoive extends PoistettuItselfOrJustParent {
    private String hakukohdeOid;
    private List<ValintakoeValinnanVaihe> valinnanVaiheet;

    public String getHakukohdeOid() {
      return hakukohdeOid;
    }

    public Hakutoive setHakukohdeOid(String hakukohdeOid) {
      this.hakukohdeOid = hakukohdeOid;
      return this;
    }

    public List<ValintakoeValinnanVaihe> getValinnanVaiheet() {
      return valinnanVaiheet;
    }

    public Hakutoive setValinnanVaiheet(List<ValintakoeValinnanVaihe> valinnanVaiheet) {
      this.valinnanVaiheet = valinnanVaiheet;
      return this;
    }
  }

  public static class ValintakoeValinnanVaihe extends PoistettuItselfOrJustParent {
    private String valinnanVaiheOid;
    private List<Valintakoe> valintakokeet;

    public String getValinnanVaiheOid() {
      return valinnanVaiheOid;
    }

    public ValintakoeValinnanVaihe setValinnanVaiheOid(String valinnanVaiheOid) {
      this.valinnanVaiheOid = valinnanVaiheOid;
      return this;
    }

    public List<Valintakoe> getValintakokeet() {
      return valintakokeet;
    }

    public ValintakoeValinnanVaihe setValintakokeet(List<Valintakoe> valintakokeet) {
      this.valintakokeet = valintakokeet;
      return this;
    }
  }

  public static class Valintakoe extends PoistettuItselfOrJustParent {
    private String valintakoeOid;

    public String getValintakoeOid() {
      return valintakoeOid;
    }

    public Valintakoe setValintakoeOid(String valintakoeOid) {
      this.valintakoeOid = valintakoeOid;
      return this;
    }
  }
}
