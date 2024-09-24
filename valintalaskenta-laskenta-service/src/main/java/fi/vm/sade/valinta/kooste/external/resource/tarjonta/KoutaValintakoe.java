package fi.vm.sade.valinta.kooste.external.resource.tarjonta;

import fi.vm.sade.valinta.kooste.external.resource.tarjonta.dto.KoutaValintakoeDTO;

import java.math.BigDecimal;

public class KoutaValintakoe extends AbstractValintakoe {
  public final BigDecimal vahimmaispisteet;

  public KoutaValintakoe(String id, String tyyppi, BigDecimal vahimmaispisteet) {
    super(id, tyyppi);
    this.vahimmaispisteet = vahimmaispisteet;
  }

  public KoutaValintakoe(KoutaValintakoeDTO dto) {
    super(dto.getId(), dto.getTyyppi());
    this.vahimmaispisteet = dto.getVahimmaispisteet();
  }
}
