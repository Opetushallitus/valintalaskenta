package fi.vm.sade.valintalaskenta.domain.valinta;

import com.fasterxml.jackson.annotation.JsonInclude;
import java.util.ArrayList;
import java.util.List;

@JsonInclude(JsonInclude.Include.NON_NULL)
public class SyotettyArvoContainer {

  public final List<SyotettyArvo> syotetytArvot = new ArrayList<>();

  public SyotettyArvoContainer() {}
  ;

  public SyotettyArvoContainer(List<SyotettyArvo> syotetytArvot) {
    this.syotetytArvot.addAll(syotetytArvot);
  }
}
