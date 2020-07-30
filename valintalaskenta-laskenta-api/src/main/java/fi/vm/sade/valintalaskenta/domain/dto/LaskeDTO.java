package fi.vm.sade.valintalaskenta.domain.dto;

import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetHakijaryhmaDTO;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.ToStringBuilder;

public class LaskeDTO {
  private final String uuid; // <- Käynnistetyn laskennan tunniste, referenssi logeihin
  private final String hakukohdeOid;
  private final boolean erillishaku;
  private final List<HakemusDTO> hakemus;
  private final List<ValintaperusteetDTO> valintaperuste;
  private final List<ValintaperusteetHakijaryhmaDTO> hakijaryhmat;
  private final boolean korkeakouluhaku;

  public LaskeDTO() {
    this.uuid = null;
    this.hakemus = Collections.emptyList();
    this.valintaperuste = Collections.emptyList();
    this.hakukohdeOid = StringUtils.EMPTY;
    this.hakijaryhmat = Collections.emptyList();
    this.korkeakouluhaku = false;
    this.erillishaku = false;
  }

  public LaskeDTO(
      String uuid,
      boolean korkeakouluhaku,
      boolean erillishaku,
      String hakukohdeOid,
      List<HakemusDTO> hakemus,
      List<ValintaperusteetDTO> valintaperuste) {
    this.uuid = uuid;
    this.hakukohdeOid = hakukohdeOid;
    this.hakemus = hakemus != null ? hakemus : Collections.<HakemusDTO>emptyList();
    this.valintaperuste =
        valintaperuste != null ? valintaperuste : Collections.<ValintaperusteetDTO>emptyList();
    this.hakijaryhmat = Collections.emptyList();
    this.korkeakouluhaku = korkeakouluhaku;
    this.erillishaku = erillishaku;
  }

  public LaskeDTO(
      String uuid,
      boolean korkeakouluhaku,
      boolean erillishaku,
      String hakukohdeOid,
      List<HakemusDTO> hakemus,
      List<ValintaperusteetDTO> valintaperuste,
      List<ValintaperusteetHakijaryhmaDTO> hakijaryhmat) {
    this.uuid = uuid;
    this.hakukohdeOid = hakukohdeOid;
    this.hakemus = hakemus != null ? hakemus : Collections.<HakemusDTO>emptyList();
    this.valintaperuste =
        valintaperuste != null ? valintaperuste : Collections.<ValintaperusteetDTO>emptyList();
    this.hakijaryhmat =
        hakijaryhmat != null
            ? hakijaryhmat
            : Collections.<ValintaperusteetHakijaryhmaDTO>emptyList();
    this.korkeakouluhaku = korkeakouluhaku;
    this.erillishaku = erillishaku;
  }

  public String getUuid() {
    return uuid;
  }

  public String getHakukohdeOid() {
    return hakukohdeOid;
  }

  public List<HakemusDTO> getHakemus() {
    return hakemus;
  }

  public List<ValintaperusteetDTO> getValintaperuste() {
    return valintaperuste;
  }

  public List<ValintaperusteetHakijaryhmaDTO> getHakijaryhmat() {
    return hakijaryhmat;
  }

  public boolean isErillishaku() {
    return erillishaku;
  }

  public boolean isKorkeakouluhaku() {
    return korkeakouluhaku;
  }

  public LaskeDTOSizes logSerializedSizes(Function<Object, Integer> sizeCounter) {
    int hakemusSize = sizeCounter.apply(hakemus);
    int valintaperusteSize = sizeCounter.apply(valintaperuste);
    int hakijaryhmatSize = sizeCounter.apply(hakijaryhmat);
    int ownSize = sizeCounter.apply(this);
    return new LaskeDTOSizes(hakemusSize, valintaperusteSize, hakijaryhmatSize, ownSize);
  }

  public void populoiSuoritustiedotHakemuksille(SuoritustiedotDTO suoritustiedotDTO) {
    getHakemus()
        .forEach(
            h ->
                h.setKoskiOpiskeluoikeudetJson(
                    suoritustiedotDTO.haeKoskiOpiskeluoikeudetJson(h.getHakijaOid())));
  }

  public static class LaskeDTOSizes {
    public final int hakemusSize;
    public final int valintaperusteSize;
    public final int hakijaryhmatSize;
    public final int ownSize;

    public LaskeDTOSizes(
        int hakemusSize, int valintaperusteSize, int hakijaryhmatSize, int ownSize) {
      this.hakemusSize = hakemusSize;
      this.valintaperusteSize = valintaperusteSize;
      this.hakijaryhmatSize = hakijaryhmatSize;
      this.ownSize = ownSize;
    }

    @Override
    public String toString() {
      return ToStringBuilder.reflectionToString(this);
    }
  }
}
