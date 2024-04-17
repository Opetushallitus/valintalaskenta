package fi.vm.sade.valintalaskenta.domain.valinta.sijoittelu;

import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.Tasasijasaanto;
import java.util.Date;
import java.util.UUID;

public class SijoitteluValintatapajono {

  public int aloituspaikat;

  public Boolean eiVarasijatayttoa;

  public Boolean kaytetaanValintalaskentaa;

  public Boolean kaikkiEhdonTayttavatHyvaksytaan;

  public Boolean poissaOlevaTaytto;

  public String valintatapajonoOid;

  public UUID id;

  public int prioriteetti;

  public Boolean siirretaanSijoitteluun;

  public Tasasijasaanto tasasijasaanto;

  public Boolean valmisSijoiteltavaksi;

  public Long sijoitteluajoId;

  public Boolean kaytetaanKokonaisPisteita;

  public String nimi;

  // Valinnanvaihe fields
  public Date createdAt;

  public String valinnanvaiheOid;

  public String valinnanvaiheNimi;

  public int jarjestysnumero;

  public String hakukohdeOid;
}
