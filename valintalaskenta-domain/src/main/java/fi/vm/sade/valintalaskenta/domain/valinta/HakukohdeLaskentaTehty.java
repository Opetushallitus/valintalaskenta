package fi.vm.sade.valintalaskenta.domain.valinta;

import java.util.Date;

public class HakukohdeLaskentaTehty {

  public String hakukohdeOid;

  public Date lastModified;

  public HakukohdeLaskentaTehty() {}

  public HakukohdeLaskentaTehty(String hakukohdeOid, Date lastModified) {
    this.lastModified = lastModified;
    this.hakukohdeOid = hakukohdeOid;
  }
}
