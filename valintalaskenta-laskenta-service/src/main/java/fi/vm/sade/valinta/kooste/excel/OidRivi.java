package fi.vm.sade.valinta.kooste.excel;

import com.google.common.base.Function;
import com.google.common.base.Predicate;
import com.google.common.collect.FluentIterable;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import java.util.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class OidRivi extends Rivi {
  private static final Logger LOG = LoggerFactory.getLogger(OidRivi.class);
  private final Set<String> oidit;
  private final Collection<OidKuuntelija> kuuntelijat;
  private final int vali;
  private final boolean keskitetty;

  public OidRivi(String oid) {
    super(new Teksti(oid));
    if (oid == null) {
      throw new RuntimeException(
          "Exceliin yritettiin oid tarkistusta null oidille. Tarkista datan eheys.");
    }
    this.oidit = Sets.newHashSet(oid);
    this.kuuntelijat = Collections.emptyList();
    this.vali = 0;
    this.keskitetty = false;
  }

  public OidRivi(List<String> oidit) {
    this(oidit, Collections.<OidKuuntelija>emptyList(), 0, false);
  }

  public OidRivi(List<String> oidit, int vali) {
    this(oidit, Collections.<OidKuuntelija>emptyList(), vali, false);
  }

  public OidRivi(List<String> oidit, int vali, boolean keskitetty) {
    this(oidit, Collections.<OidKuuntelija>emptyList(), vali, keskitetty);
  }

  public static List<Solu> tekstiSolut(List<String> tekstit, int vali, boolean keskitetty) {
    List<Solu> solut = Lists.newArrayList();
    for (String teksti : tekstit) {
      solut.add(new Teksti(teksti, keskitetty, keskitetty, false, 0, vali, false));
      // for (int i = 0; i < vali; ++i) {
      // solut.add(Teksti.tyhja());
      // }
    }
    return solut;
  }

  public OidRivi(
      List<String> oidit, Collection<OidKuuntelija> kuuntelijat, int vali, boolean keskitetty) {
    super(tekstiSolut(oidit, vali, keskitetty));
    if (oidit == null || oidit.isEmpty()) {
      throw new RuntimeException(
          "Exceliin yritettiin oid tarkistusta null oidille. Tarkista datan eheys.");
    }
    this.oidit = Sets.newHashSet(oidit);
    if (this.oidit.size() != oidit.size()) {
      throw new RuntimeException("Oidien on oltava uniikkeja!");
    }
    this.kuuntelijat = kuuntelijat;
    this.vali = vali;
    this.keskitetty = keskitetty;
  }

  public Set<String> getOidit() {
    return oidit;
  }

  public boolean validoi(Rivi rivi) throws ExcelValidointiPoikkeus {
    List<Solu> soluja;
    if (vali != 0) {
      soluja = Lists.newArrayList();
      int i = 0;
      for (Solu s : rivi.getSolut()) {
        if (i % vali == 0) {
          soluja.add(s);
        }
        ++i;
      }
    } else {
      soluja = rivi.getSolut();
    }
    // valuta(rivi.getSolut().iterator(),
    // oidit.size() + (oidit.size() - 1) * vali);

    if (soluja.size() != oidit.size()) {
      String oidstr = Arrays.toString(oidit.toArray());
      throw new ExcelValidointiPoikkeus("Odotettiin oideja " + oidstr);
    }
    ImmutableList<String> tekstit =
        FluentIterable.from(soluja)
            .filter(
                new Predicate<Solu>() {
                  public boolean apply(Solu input) {
                    return !input.isTyhja();
                  }
                })
            //
            .transform(
                new Function<Solu, String>() {
                  @Override
                  public String apply(Solu input) {
                    return input.toTeksti().getTeksti();
                  }
                })
            .toList();

    if (!Sets.newHashSet(tekstit).equals(oidit)) {
      String oidstr = Arrays.toString(oidit.toArray());
      String tekstr = Arrays.toString(tekstit.toArray());
      LOG.error("Oid({} != {}) ei ole validi!", oidstr, tekstr);
      throw new ExcelValidointiPoikkeus("Odotettiin oidit " + oidstr + " mutta saatiin " + tekstr);
    }
    for (OidKuuntelija kuuntelija : kuuntelijat) {
      kuuntelija.oidienJarjestysTapahtuma(tekstit);
    }
    return false;
  }
}
