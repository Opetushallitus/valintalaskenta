package fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto;

import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila;
import java.util.ArrayList;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

public class JonosijaSiirtotiedostoDTO {

  private String hakemusOid;

  private String hakijaOid;

  private SortedSet<JarjestyskriteeritulosSiirtotiedostoDTO> jarjestyskriteerit = new TreeSet<>();

  private int prioriteetti;

  private JarjestyskriteerituloksenTila tuloksenTila;

  private List<SyotettyArvoSiirtotiedostoDTO> syotetytArvot = new ArrayList<>();

  private List<FunktioTulosSiirtotiedostoDTO> funktioTulokset = new ArrayList<>();

  private boolean muokattu = false;

  private boolean hylattyValisijoittelussa = false;

  public String getHakemusOid() {
    return hakemusOid;
  }

  public void setHakemusOid(String hakemusOid) {
    this.hakemusOid = hakemusOid != null ? hakemusOid.intern() : null;
  }

  public String getHakijaOid() {
    return hakijaOid;
  }

  public void setHakijaOid(String hakijaOid) {
    this.hakijaOid = hakijaOid != null ? hakijaOid.intern() : null;
  }

  public SortedSet<JarjestyskriteeritulosSiirtotiedostoDTO> getJarjestyskriteerit() {
    return jarjestyskriteerit;
  }

  public void setJarjestyskriteerit(
      SortedSet<JarjestyskriteeritulosSiirtotiedostoDTO> jarjestyskriteerit) {
    this.jarjestyskriteerit = jarjestyskriteerit;
  }

  public int getPrioriteetti() {
    return prioriteetti;
  }

  public void setPrioriteetti(int prioriteetti) {
    this.prioriteetti = prioriteetti;
  }

  public JarjestyskriteerituloksenTila getTuloksenTila() {
    return tuloksenTila;
  }

  public void setTuloksenTila(JarjestyskriteerituloksenTila tuloksenTila) {
    this.tuloksenTila = tuloksenTila;
  }

  public List<SyotettyArvoSiirtotiedostoDTO> getSyotetytArvot() {
    return syotetytArvot;
  }

  public void setSyotetytArvot(List<SyotettyArvoSiirtotiedostoDTO> syotetytArvot) {
    this.syotetytArvot = syotetytArvot;
  }

  public List<FunktioTulosSiirtotiedostoDTO> getFunktioTulokset() {
    return funktioTulokset;
  }

  public void setFunktioTulokset(List<FunktioTulosSiirtotiedostoDTO> funktioTulokset) {
    this.funktioTulokset = funktioTulokset;
  }

  public boolean isMuokattu() {
    return muokattu;
  }

  public void setMuokattu(boolean muokattu) {
    this.muokattu = muokattu;
  }

  public boolean isHylattyValisijoittelussa() {
    return hylattyValisijoittelussa;
  }

  public void setHylattyValisijoittelussa(boolean hylattyValisijoittelussa) {
    this.hylattyValisijoittelussa = hylattyValisijoittelussa;
  }

  private String lastModified;

  public String getLastModified() {
    return lastModified;
  }

  public void setLastModified(String lastModified) {
    this.lastModified = lastModified;
  }
}
