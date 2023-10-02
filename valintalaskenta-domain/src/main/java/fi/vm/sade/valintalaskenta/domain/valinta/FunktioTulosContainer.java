package fi.vm.sade.valintalaskenta.domain.valinta;

import com.fasterxml.jackson.annotation.JsonInclude;

import java.util.ArrayList;
import java.util.List;

@JsonInclude(JsonInclude.Include.NON_NULL)
public class FunktioTulosContainer {

  public final List<FunktioTulos> funktioTulokset = new ArrayList<>();

  public FunktioTulosContainer() {};

  public FunktioTulosContainer(List<FunktioTulos> funktioTulokset) {
    this.funktioTulokset.addAll(funktioTulokset);
  }
}
