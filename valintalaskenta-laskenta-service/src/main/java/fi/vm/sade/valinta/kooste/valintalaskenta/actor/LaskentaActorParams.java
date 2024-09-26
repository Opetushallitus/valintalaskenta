package fi.vm.sade.valinta.kooste.valintalaskenta.actor;

import fi.vm.sade.valinta.kooste.valintalaskenta.dto.HakukohdeJaOrganisaatio;
import fi.vm.sade.valinta.kooste.valintalaskenta.dto.LaskentaStartParams;
import java.util.Collection;

public class LaskentaActorParams {
  private final LaskentaStartParams laskentaStartParams;
  private final Collection<HakukohdeJaOrganisaatio> hakukohdeOids;

  public LaskentaActorParams(
      LaskentaStartParams laskentaStartParams,
      Collection<HakukohdeJaOrganisaatio> hakukohdeOids) {
    this.laskentaStartParams = laskentaStartParams;
    this.hakukohdeOids = hakukohdeOids;
  }

  public String getUuid() {
    return laskentaStartParams.getUuid();
  }

  public String getHakuOid() {
    return laskentaStartParams.getHakuOid();
  }

  public LaskentaStartParams getLaskentaStartParams() {
    return laskentaStartParams;
  }

  public boolean isErillishaku() {
    return laskentaStartParams.isErillishaku();
  }

  public Boolean isValintakoelaskenta() {
    return laskentaStartParams.getValintakoelaskenta();
  }

  public boolean isOsittainen() {
    return laskentaStartParams.isOsittainenLaskenta();
  }

  public Integer getValinnanvaihe() {
    return this.laskentaStartParams.getValinnanvaihe();
  }

  public Collection<HakukohdeJaOrganisaatio> getHakukohdeOids() {
    return hakukohdeOids;
  }

  /** Tilapainen workaround resurssin valinnanvaiheen normalisointiin. */
  public LaskentaTyyppi getLaskentaTyyppi() {
    if (fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaTyyppi.VALINTARYHMA.equals(
        laskentaStartParams.getTyyppi())) {
      return LaskentaTyyppi.VALINTARYHMALASKENTA;
    }
    if (Boolean.TRUE.equals(laskentaStartParams.getValintakoelaskenta())) {
      return LaskentaTyyppi.VALINTAKOELASKENTA;
    } else {
      if (laskentaStartParams.getValinnanvaihe() == null) {
        return LaskentaTyyppi.KAIKKI;
      } else {
        return LaskentaTyyppi.VALINTALASKENTA;
      }
    }
  }
}
