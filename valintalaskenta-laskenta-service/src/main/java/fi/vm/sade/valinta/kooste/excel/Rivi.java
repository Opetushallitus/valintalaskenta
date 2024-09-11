package fi.vm.sade.valinta.kooste.excel;

import com.google.common.collect.Lists;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import org.apache.commons.lang.StringUtils;

public class Rivi {
  private final List<Solu> solut;

  public Rivi() {
    this.solut = Collections.emptyList();
  }

  public List<Rivi> getToisteisetRivit() {
    return Arrays.asList(this);
  }

  public Rivi(Solu solu) {
    this.solut = Arrays.asList(solu);
  }

  public Rivi(List<Solu> solut) {
    this.solut = solut;
  }

  public boolean isNakyvissa() {
    return true;
  }

  public boolean isTyhja() {
    if (solut.isEmpty()) {
      return true;
    } else {
      return solut.stream().allMatch(s -> StringUtils.isBlank(s.toTeksti().getTeksti()));
    }
  }

  public boolean validoi(Rivi rivi) throws ExcelValidointiPoikkeus {
    return false;
  }

  private static final Rivi TYHJA = new Rivi();

  public static Rivi tyhjaRivi() {
    return TYHJA;
  }

  public List<Solu> getSolut() {
    return solut;
  }

  public static Rivi tekstiRivi(List<String> tekstit) {
    List<Solu> solut = Lists.newArrayList();
    for (String teksti : tekstit) {
      solut.add(new Teksti(teksti));
    }
    return new Rivi(solut);
  }

  protected static List<Solu> valuta(Iterator<Solu> soluIterator, int maara) {
    if (soluIterator.hasNext()) {
      List<Solu> solut = Lists.newArrayList();
      for (int i = 0; i < maara; ++i) {
        solut.add(soluIterator.next());
        if (!soluIterator.hasNext()) {
          break;
        }
      }
      return solut;
    }
    return Collections.emptyList();
  }

  public String getArvoAt(int index) {
    return StringUtils.trimToEmpty(get(index).toTeksti().getTeksti());
  }

  public Solu get(int index) {
    if (solut.size() <= index) {
      return Teksti.tyhja();
    } else {
      Solu solu = solut.get(index);
      if (solu == null) {
        return Teksti.tyhja();
      }
      return solu;
    }
  }

  @Override
  public String toString() {
    StringBuilder b = new StringBuilder();
    for (Solu s : solut) {
      b.append(s).append(", ");
    }
    return b.toString();
  }
}
