package fi.vm.sade.valinta.kooste.valintalaskenta.actor;

import fi.vm.sade.valinta.kooste.external.resource.ohjausparametrit.dto.ParametritDTO;
import fi.vm.sade.valinta.kooste.valintalaskenta.actor.dto.HakukohdeJaOrganisaatio;
import fi.vm.sade.valinta.kooste.valintalaskenta.dto.LaskentaStartParams;
import java.util.Collection;
import java.util.stream.Collectors;

public class LaskentaActorParams {
  private static final Integer HAE_KAIKKI_VALINNANVAIHEET = -1;

  private final LaskentaStartParams laskentaStartParams;
  private final ParametritDTO parametritDTO;
  private final Collection<HakukohdeJaOrganisaatio> hakukohdeOids;
  private boolean isValintaryhmalaskenta;

  public LaskentaActorParams(LaskentaStartParams laskentaStartParams, ParametritDTO parametritDTO) {
    this(
        laskentaStartParams,
        laskentaStartParams.getHakukohdeDtos().stream()
            .map(hk -> new HakukohdeJaOrganisaatio(hk.getHakukohdeOid(), hk.getOrganisaatioOid()))
            .collect(Collectors.toList()),
        parametritDTO);
  }

  public LaskentaActorParams(
      LaskentaStartParams laskentaStartParams,
      Collection<HakukohdeJaOrganisaatio> hakukohdeOids,
      ParametritDTO parametritDTO) {
    this.laskentaStartParams = laskentaStartParams;
    this.parametritDTO = parametritDTO;
    this.hakukohdeOids = hakukohdeOids;
    this.isValintaryhmalaskenta = false;
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

  public ParametritDTO getParametritDTO() {
    return parametritDTO;
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

  /** Tilapainen workaround resurssin valinnanvaiheen normalisointiin. */
  public Integer getValinnanvaihe() {
    return HAE_KAIKKI_VALINNANVAIHEET.equals(laskentaStartParams.getValinnanvaihe())
        ? null
        : laskentaStartParams.getValinnanvaihe();
  }

  public Collection<HakukohdeJaOrganisaatio> getHakukohdeOids() {
    return hakukohdeOids;
  }

  public void setValintaryhmalaskenta(boolean value) {
    this.isValintaryhmalaskenta = value;
  }

  public boolean isValintaryhmalaskenta() {
    return this.isValintaryhmalaskenta;
  }

  /** Tilapainen workaround resurssin valinnanvaiheen normalisointiin. */
  public LaskentaTyyppi getLaskentaTyyppi() {
    if (fi.vm.sade.valinta.seuranta.dto.LaskentaTyyppi.VALINTARYHMA.equals(
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
