package fi.vm.sade.valinta.kooste.external.resource.tarjonta;

import fi.vm.sade.tarjonta.service.resources.v1.dto.ValintakoeV1RDTO;

public class TarjontaValintakoe extends AbstractValintakoe {
  public TarjontaValintakoe(ValintakoeV1RDTO dto) {
    super(dto.getOid(), dto.getValintakoetyyppi());
  }
}
