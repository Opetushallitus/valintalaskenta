package fi.vm.sade.valinta.kooste.excel;

import static java.util.Arrays.asList;

import java.util.Optional;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class AikaleimaRivi extends Rivi {
  private static final Logger LOG = LoggerFactory.getLogger(AikaleimaRivi.class);
  public Optional<String> currentAikaleima;

  public static Solu teksti(String t, int ulottuvuus) {
    return new Teksti(t, ulottuvuus);
  }

  public AikaleimaRivi() {
    super(asList());
  }

  public AikaleimaRivi(Optional<String> aikaleima) {
    super(aikaleima.map(a -> asList(teksti(a, 1), teksti("Aikaleima", 4))).orElse(asList()));
    this.currentAikaleima = aikaleima;
  }

  public Optional<String> getCurrentAikaleima() {
    return currentAikaleima;
  }

  @Override
  public boolean validoi(Rivi rivi) throws ExcelValidointiPoikkeus {
    this.currentAikaleima =
        rivi.getSolut().stream()
            .findFirst()
            .map(s -> s.toTeksti().getTeksti())
            .filter(s -> !StringUtils.isBlank(s));
    return false;
  }
}
