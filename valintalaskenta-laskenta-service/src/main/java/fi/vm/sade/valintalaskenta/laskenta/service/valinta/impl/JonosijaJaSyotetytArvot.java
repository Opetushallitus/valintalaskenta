package fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl;

import fi.vm.sade.service.valintaperusteet.laskenta.api.FunktioTulos;
import fi.vm.sade.service.valintaperusteet.laskenta.api.SyotettyArvo;
import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

public class JonosijaJaSyotetytArvot {
  public JonosijaJaSyotetytArvot(
      Jonosija jonosija,
      Map<String, SyotettyArvo> syotetytArvot,
      Map<String, FunktioTulos> funktioTulokset) {
    this.jonosija = jonosija;
    this.syotetytArvot = syotetytArvot;
    this.funktioTulokset = funktioTulokset;
  }

  public JonosijaJaSyotetytArvot(Jonosija jonosija) {
    this(jonosija, new HashMap<>(), new HashMap<>());
  }

  private Jonosija jonosija;
  private Map<String, SyotettyArvo> syotetytArvot = new HashMap<String, SyotettyArvo>();
  private Map<String, FunktioTulos> funktioTulokset = new HashMap<String, FunktioTulos>();

  public Jonosija getJonosija() {
    return jonosija;
  }

  public Map<String, SyotettyArvo> getSyotetytArvot() {
    return Collections.unmodifiableMap(syotetytArvot);
  }

  public Map<String, FunktioTulos> getFunktioTulokset() {
    return funktioTulokset;
  }

  public void lisaaSyotetytArvot(Map<String, SyotettyArvo> syotetytArvot) {
    this.syotetytArvot.putAll(syotetytArvot);
  }

  public void lisaaFunktioTulokset(Map<String, FunktioTulos> funktioTulokset) {
    this.funktioTulokset.putAll(funktioTulokset);
  }
}
