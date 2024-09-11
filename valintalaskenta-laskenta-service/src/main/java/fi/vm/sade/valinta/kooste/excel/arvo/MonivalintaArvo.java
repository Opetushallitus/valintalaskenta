package fi.vm.sade.valinta.kooste.excel.arvo;

import fi.vm.sade.valinta.kooste.excel.ArvoTyyppi;
import fi.vm.sade.valinta.kooste.excel.Excel;
import fi.vm.sade.valinta.kooste.excel.Monivalinta;
import java.util.Collection;

public class MonivalintaArvo extends Arvo {
  private final String arvo;
  private final Collection<String> vaihtoehdot;

  public MonivalintaArvo(String arvo, Collection<String> vaihtoehdot) {
    super(ArvoTyyppi.MONIVALINTA);
    this.arvo = arvo;
    this.vaihtoehdot = vaihtoehdot;
  }

  public Monivalinta asMonivalinta() {
    return new Monivalinta(arvo, vaihtoehdot, true, Excel.VAKIO_LEVEYS / 2);
  }

  public String getArvo() {
    return arvo;
  }

  @Override
  public String toString() {
    return arvo;
  }

  public Collection<String> getVaihtoehdot() {
    return vaihtoehdot;
  }
}
