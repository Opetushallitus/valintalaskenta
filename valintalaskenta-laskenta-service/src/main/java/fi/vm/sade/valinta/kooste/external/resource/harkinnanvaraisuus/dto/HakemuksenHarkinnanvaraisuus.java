package fi.vm.sade.valinta.kooste.external.resource.harkinnanvaraisuus.dto;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import fi.vm.sade.valinta.kooste.external.resource.harkinnanvaraisuus.HarkinnanvaraisuudenSyy;
import java.util.List;

@JsonSerialize(include = JsonSerialize.Inclusion.NON_EMPTY)
public class HakemuksenHarkinnanvaraisuus {

  private String hakemusOid;
  private String henkiloOid;
  private List<HakutoiveenHarkinnanvaraisuus> hakutoiveet;

  public HakemuksenHarkinnanvaraisuus() {}

  public HakemuksenHarkinnanvaraisuus(
      String hakemusOid, List<HakutoiveenHarkinnanvaraisuus> hakutoiveet) {
    this.hakemusOid = hakemusOid;
    this.hakutoiveet = hakutoiveet;
  }

  public String getHakemusOid() {
    return hakemusOid;
  }

  public void setHakemusOid(String hakemusOid) {
    this.hakemusOid = hakemusOid;
  }

  public String getHenkiloOid() {
    return henkiloOid;
  }

  public void setHenkiloOid(String henkiloOid) {
    this.henkiloOid = henkiloOid;
  }

  public List<HakutoiveenHarkinnanvaraisuus> getHakutoiveet() {
    return hakutoiveet;
  }

  public void setHakutoiveet(List<HakutoiveenHarkinnanvaraisuus> hakutoiveet) {
    this.hakutoiveet = hakutoiveet;
  }

  public boolean hasYksilollistettyMatAi() {
    return this.hakutoiveet.stream()
        .anyMatch(
            ht ->
                List.of(
                        HarkinnanvaraisuudenSyy.SURE_YKS_MAT_AI,
                        HarkinnanvaraisuudenSyy.ATARU_YKS_MAT_AI)
                    .contains(ht.getHarkinnanvaraisuudenSyy()));
  }
}
