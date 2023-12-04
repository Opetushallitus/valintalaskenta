package fi.vm.sade.valintalaskenta.domain.valinta.sijoittelu;

import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila;
import fi.vm.sade.valintalaskenta.domain.valinta.SyotettyArvoContainer;
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

public class SijoitteluJarjestyskriteeritulos {

  // Jonosijan tiedot

  public UUID jonosijaId;

  public String hakemusOid;

  public String hakijaOid;

  public SyotettyArvoContainer syotetytArvot;

  public int hakutoiveprioriteetti;

  public Boolean harkinnanvarainen;

  public Boolean hylattyValisijoittelussa;

  public UUID valintatapajono;

  // Jarjestyskriteerin tiedot:
  public JarjestyskriteerituloksenTila tila;

  public BigDecimal arvo;

  public int prioriteetti;

  public String nimi;

  public String kuvausFi;

  public String kuvausSv;

  public String kuvausEn;

  public Map<String, String> getKuvaus() {
    Map<String, String> kuvaus = new HashMap<>();
    if (kuvausFi != null) {
      kuvaus.put("FI", kuvausFi);
    }
    if (kuvausSv != null) {
      kuvaus.put("SV", kuvausSv);
    }
    if (kuvausEn != null) {
      kuvaus.put("EN", kuvausEn);
    }
    return kuvaus;
  }
}
