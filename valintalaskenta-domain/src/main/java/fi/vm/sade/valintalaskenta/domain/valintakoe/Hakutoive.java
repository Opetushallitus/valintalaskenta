package fi.vm.sade.valintalaskenta.domain.valintakoe;

import java.util.ArrayList;
import java.util.List;
import org.apache.commons.lang3.builder.ToStringBuilder;

public class Hakutoive {
  private String hakukohdeOid;

  private String laskettavaHakukohdeOid;

  private List<ValintakoeValinnanvaihe> valinnanVaiheet = new ArrayList<>();

  public String getHakukohdeOid() {
    return hakukohdeOid;
  }

  public void setHakukohdeOid(String hakukohdeOid) {
    this.hakukohdeOid = hakukohdeOid;
  }

  public List<ValintakoeValinnanvaihe> getValinnanVaiheet() {
    return valinnanVaiheet;
  }

  public void setValinnanVaiheet(List<ValintakoeValinnanvaihe> valinnanVaiheet) {
    this.valinnanVaiheet = valinnanVaiheet;
  }

  public String getLaskettavaHakukohdeOid() {
    return laskettavaHakukohdeOid;
  }

  public void setLaskettavaHakukohdeOid(String laskettavaHakukohdeOid) {
    this.laskettavaHakukohdeOid = laskettavaHakukohdeOid;
  }

  @Override
  public String toString() {
    return ToStringBuilder.reflectionToString(this);
  }
}
