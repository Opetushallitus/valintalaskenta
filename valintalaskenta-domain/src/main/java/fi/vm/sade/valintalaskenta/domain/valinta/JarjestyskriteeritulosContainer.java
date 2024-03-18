package fi.vm.sade.valintalaskenta.domain.valinta;

import com.fasterxml.jackson.annotation.JsonInclude;
import java.util.ArrayList;
import java.util.List;

@JsonInclude(JsonInclude.Include.NON_NULL)
public class JarjestyskriteeritulosContainer {

  public final List<Jarjestyskriteeritulos> jarjestyskriteeritulokset = new ArrayList<>();

  public JarjestyskriteeritulosContainer() {}
  ;

  public JarjestyskriteeritulosContainer(List<Jarjestyskriteeritulos> jarjestyskriteeritulokset) {
    this.jarjestyskriteeritulokset.addAll(jarjestyskriteeritulokset);
  }

  public List<Jarjestyskriteeritulos> getJarjestyskriteeritulokset() {
    return jarjestyskriteeritulokset;
  }
}
