package fi.vm.sade.valintalaskenta.domain.dto.valintatieto;

import fi.vm.sade.valintalaskenta.domain.dto.OsallistuminenDTO;
import io.swagger.v3.oas.annotations.media.Schema;

@Schema(
    name = "valintalaskenta.valintatieto.ValintakoeOsallistuminenDTO",
    description = "Valintakoeosallistuminen")
public class ValintakoeOsallistuminenDTO {
  private OsallistuminenDTO osallistuminen;
  private String valintakoeOid;
  private String valintakoeTunniste;
  private String nimi;

  public void setOsallistuminen(OsallistuminenDTO osallistuminen) {
    this.osallistuminen = osallistuminen;
  }

  public OsallistuminenDTO getOsallistuminen() {
    return osallistuminen;
  }

  public void setValintakoeOid(String valintakoeOid) {
    this.valintakoeOid = valintakoeOid;
  }

  public String getValintakoeOid() {
    return valintakoeOid;
  }

  public void setValintakoeTunniste(String valintakoeTunniste) {
    this.valintakoeTunniste = valintakoeTunniste;
  }

  public String getValintakoeTunniste() {
    return valintakoeTunniste;
  }

  public String getNimi() {
    return nimi;
  }

  public void setNimi(String nimi) {
    this.nimi = nimi;
  }
}
