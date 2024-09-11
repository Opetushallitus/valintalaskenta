package fi.vm.sade.valinta.kooste.excel;

import com.google.common.collect.Lists;
import java.util.Iterator;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** Koostaa useasta rivista yhden rivin */
public class Kooste extends Rivi {
  private static final Logger LOG = LoggerFactory.getLogger(Kooste.class);
  private final List<Rivi> rivit;
  private final boolean nakyvissa;

  public Kooste(Rivi... rivit) {
    this(Lists.newArrayList(rivit));
  }

  public Kooste(List<Rivi> rivit) {
    super(pura(rivit));
    this.rivit = rivit;
    this.nakyvissa = true;
  }

  public Kooste(List<Rivi> rivit, boolean nakyvissa) {
    super(pura(rivit));
    this.rivit = rivit;
    this.nakyvissa = nakyvissa;
  }

  @Override
  public boolean isNakyvissa() {
    return nakyvissa;
  }

  @Override
  public boolean validoi(Rivi rivi) throws ExcelValidointiPoikkeus {
    Iterator<Solu> soluIterator = rivi.getSolut().iterator();
    // LOG.error("KOOSTE VALIDOI");
    for (Rivi r : rivit) {
      int size = 0;
      for (Solu s : r.getSolut()) {
        size += s.ulottuvuus();
      }
      r.validoi(new Rivi(valuta(soluIterator, size)));
    }
    return false;
  }

  private static List<Solu> pura(List<Rivi> rivit) {
    List<Solu> solut = Lists.newArrayList();
    for (Rivi rivi : rivit) {
      solut.addAll(rivi.getSolut());
    }
    return solut;
  }
}
