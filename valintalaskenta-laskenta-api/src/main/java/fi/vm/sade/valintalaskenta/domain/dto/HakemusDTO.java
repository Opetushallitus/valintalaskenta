package fi.vm.sade.valintalaskenta.domain.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

@Schema(name = "HakemusDTO", description = "Yhden hakemuksen tiedot")
public class HakemusDTO {

  @Schema(title = "Haku OID", required = true)
  private String hakuoid; // mika haku

  public void setHakuoid(String hakuoid) {
    this.hakuoid = hakuoid;
  }

  public void setHakemusoid(String hakemusoid) {
    this.hakemusoid = hakemusoid;
  }

  public void setHakukohteet(List<HakukohdeDTO> hakukohteet) {
    this.hakukohteet = hakukohteet;
  }

  @Schema(title = "Hakemus OID", required = true)
  private String hakemusoid;

  @Schema(title = "Hakutoiveet", required = true)
  private List<HakukohdeDTO> hakukohteet;

  @Schema(title = "Hakija OID", required = false)
  private String hakijaOid;

  @Schema(title = "Hakija etunimi", required = false)
  private String etunimi;

  @Schema(title = "Hakijan sukunimi", required = false)
  private String sukunimi;

  @Schema(title = "Hakijan Koski-opiskeluoikeudet", required = false)
  private String koskiOpiskeluoikeudetJson;

  /** @Deprecated Jatkossa käytetään avainSuoritusTiedot -kenttää */
  @Deprecated
  @Schema(title = "Hakemuksen avain/arvo map", required = false)
  private List<AvainArvoDTO> avaimet = new ArrayList<AvainArvoDTO>();

  @Schema(title = "Hakemuksen avain/arvo map", required = false)
  private List<AvainMetatiedotDTO> avainMetatiedotDTO = Collections.emptyList();

  public HakemusDTO(String hakuoid, String hakemusoid, List<HakukohdeDTO> hakukohteet) {
    this.hakuoid = hakuoid;
    this.hakemusoid = hakemusoid;
    this.hakukohteet = hakukohteet;
  }

  public List<AvainMetatiedotDTO> getAvainMetatiedotDTO() {
    return avainMetatiedotDTO;
  }

  public void setAvainMetatiedotDTO(List<AvainMetatiedotDTO> avainMetatiedotDTO) {
    this.avainMetatiedotDTO = avainMetatiedotDTO;
  }

  public HakemusDTO() {
    hakukohteet = new ArrayList<HakukohdeDTO>();
  }

  public List<HakukohdeDTO> getHakukohteet() {
    return hakukohteet;
  }

  public String getHakemusoid() {
    return hakemusoid;
  }

  public String getHakuoid() {
    return hakuoid;
  }

  public String getHakijaOid() {
    return hakijaOid;
  }

  public void setHakijaOid(String hakijaOid) {
    this.hakijaOid = hakijaOid;
  }

  public String getEtunimi() {
    return etunimi;
  }

  public void setEtunimi(String etunimi) {
    this.etunimi = etunimi;
  }

  public String getSukunimi() {
    return sukunimi;
  }

  public void setSukunimi(String sukunimi) {
    this.sukunimi = sukunimi;
  }

  public List<AvainArvoDTO> getAvaimet() {
    return avaimet;
  }

  public void setAvaimet(List<AvainArvoDTO> avaimet) {
    this.avaimet = avaimet;
  }

  public String getKoskiOpiskeluoikeudetJson() {
    return koskiOpiskeluoikeudetJson;
  }

  public void setKoskiOpiskeluoikeudetJson(String koskiOpiskeluoikeudetJson) {
    this.koskiOpiskeluoikeudetJson = koskiOpiskeluoikeudetJson;
  }
}
