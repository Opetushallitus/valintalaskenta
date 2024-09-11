package fi.vm.sade.valinta.kooste.excel;

import com.google.common.collect.Lists;
import fi.vm.sade.valinta.kooste.excel.arvo.Arvo;
import fi.vm.sade.valinta.kooste.excel.arvo.MonivalintaArvo;
import java.util.Collection;
import java.util.List;

public class DataRivi extends Rivi {
  private final Collection<Collection<Arvo>> s;

  public DataRivi(Collection<Collection<Arvo>> s) {
    super();
    this.s = s;
  }

  @Override
  public boolean validoi(Rivi rivi) throws ExcelValidointiPoikkeus {
    return true;
  }

  private Rivi asRivi(Collection<Arvo> arvot) {
    RiviBuilder riviBuilder = new RiviBuilder();
    for (Arvo arvo : arvot) {
      if (ArvoTyyppi.MONIVALINTA.equals(arvo.getTyyppi())) {
        MonivalintaArvo monivalinta = arvo.asMonivalintaArvo();

        riviBuilder.addSolu(monivalinta.asMonivalinta());

      } else if (ArvoTyyppi.NUMERO.equals(arvo.getTyyppi())) {
        riviBuilder.addSolu(arvo.asNumeroArvo().asNumero());
      } else {
        riviBuilder.addSolu(arvo.asTekstiArvo().asTeksti());
      }
    }
    return riviBuilder.build();
  }

  @Override
  public List<Rivi> getToisteisetRivit() {
    List<Rivi> rivit = Lists.newArrayList();
    for (Collection<Arvo> arvot : s) {
      rivit.add(asRivi(arvot));
    }
    return rivit;
  }
}
