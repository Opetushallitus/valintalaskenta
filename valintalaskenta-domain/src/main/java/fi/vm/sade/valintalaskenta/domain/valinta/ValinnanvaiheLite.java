package fi.vm.sade.valintalaskenta.domain.valinta;

import java.util.Date;
import java.util.UUID;

public class ValinnanvaiheLite {

  private UUID id;

  private int jarjestysnumero;

  private Date createdAt;

  private String hakuOid;

  private String hakukohdeOid;

  private String valinnanvaiheOid;

  private String tarjoajaOid;

  private String nimi;
}
