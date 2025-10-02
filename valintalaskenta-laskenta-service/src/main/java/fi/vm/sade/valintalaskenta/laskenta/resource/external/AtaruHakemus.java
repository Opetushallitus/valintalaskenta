package fi.vm.sade.valintalaskenta.laskenta.resource.external;

import com.google.gson.annotations.SerializedName;
import fi.vm.sade.valintalaskenta.domain.dto.valintapiste.Pistetieto;
import fi.vm.sade.valintalaskenta.domain.dto.valintapiste.PistetietoWrapper;
import java.util.Arrays;

public record AtaruHakemus(
    @SerializedName("hakemus_oid") String hakemusOid,
    @SerializedName("henkilo_oid") String henkiloOid) {

  public PistetietoWrapper toPistetietoWrapper(Pistetieto... pisteet) {
    return new PistetietoWrapper(hakemusOid(), henkiloOid(), Arrays.stream(pisteet).toList());
  }
}
