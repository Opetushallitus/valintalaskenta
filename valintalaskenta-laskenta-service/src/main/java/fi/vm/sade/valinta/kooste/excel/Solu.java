package fi.vm.sade.valinta.kooste.excel;

import com.google.common.collect.Lists;
import java.util.Collection;

public abstract class Solu {
  private final String kommentti;

  public Solu() {
    this.kommentti = null;
  }

  public Solu(String kommentti) {
    this.kommentti = kommentti;
  }

  public boolean isMuokattava() {
    return false;
  }

  public boolean hasKommentti() {
    return this.kommentti != null;
  }

  public boolean isPiilotettu() {
    return false;
  }

  public abstract boolean isTyhja();

  public boolean isNumero() {
    return false;
  }

  public boolean isTeksti() {
    return false;
  }

  public boolean isMonivalinta() {
    return false;
  }

  public int ulottuvuus() {
    return 1;
  }

  public int preferoituLeveys() {
    return 0; // Excel.VAKIO_LEVEYS;
  }

  public abstract Monivalinta toMonivalinta();

  public abstract Teksti toTeksti();

  public abstract Numero toNumero();

  public static Collection<Solu> tekstiSolut(Collection<String> tekstit) {
    Collection<Solu> solut = Lists.newArrayList();
    for (String teksti : tekstit) {
      solut.add(new Teksti(teksti));
    }
    return solut;
  }

  public boolean isTasausOikealle() {
    return false;
  }

  public boolean isKeskitettyTasaus() {
    return false;
  }

  public boolean isLukittu() {
    return false;
  }
}
