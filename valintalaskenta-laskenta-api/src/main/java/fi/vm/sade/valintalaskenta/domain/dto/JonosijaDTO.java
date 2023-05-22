package fi.vm.sade.valintalaskenta.domain.dto;

import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.ArrayList;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

@Schema(name = "JonosijaDTO", description = "Jonosija")
public class JonosijaDTO {
  public JonosijaDTO() {}

  public JonosijaDTO(
      final int jonosija,
      final String hakemusOid,
      final String hakijaOid,
      final SortedSet<JarjestyskriteeritulosDTO> jarjestyskriteerit,
      final int prioriteetti,
      final String sukunimi,
      final String etunimi,
      final boolean harkinnanvarainen,
      final JarjestyskriteerituloksenTila tuloksenTila,
      final List<String> historiat,
      final List<SyotettyArvoDTO> syotetytArvot,
      final List<FunktioTulosDTO> funktioTulokset,
      final boolean muokattu,
      final boolean hylattyValisijoittelussa) {
    this.jonosija = jonosija;
    this.hakemusOid = hakemusOid != null ? hakemusOid.intern() : null;
    this.hakijaOid = hakijaOid != null ? hakijaOid.intern() : null;
    this.jarjestyskriteerit = jarjestyskriteerit;
    this.prioriteetti = prioriteetti;
    this.sukunimi = sukunimi != null ? sukunimi.intern() : null;
    this.etunimi = etunimi != null ? etunimi.intern() : null;
    this.harkinnanvarainen = harkinnanvarainen;
    this.tuloksenTila = tuloksenTila;
    this.historiat = historiat;
    this.syotetytArvot = syotetytArvot;
    this.funktioTulokset = funktioTulokset;
    this.muokattu = muokattu;
    this.hylattyValisijoittelussa = hylattyValisijoittelussa;
  }

  @Schema(name = "Jonosijanumero", required = true)
  private int jonosija;

  @Schema(name = "Hakemus OID", required = true)
  private String hakemusOid;

  @Schema(name = "Hakija OID")
  private String hakijaOid;

  @Schema(name = "Järjestyskriteerit", required = true)
  private SortedSet<JarjestyskriteeritulosDTO> jarjestyskriteerit =
      new TreeSet<JarjestyskriteeritulosDTO>();

  @Schema(name = "Prioriteetti", required = true)
  private int prioriteetti;

  @Schema(name = "Sukunimi")
  private String sukunimi;

  @Schema(name = "Etunimi")
  private String etunimi;

  @Schema(name = "Onko hakija hakenut harkinnanvaraisesti")
  private boolean harkinnanvarainen = false;

  @Schema(name = "Jonosijan tila", required = true)
  private JarjestyskriteerituloksenTila tuloksenTila;

  @Schema(name = "Jonosijan historiat")
  private List<String> historiat;

  @Schema(name = "Jonosijan syötetyt arvot")
  private List<SyotettyArvoDTO> syotetytArvot = new ArrayList<SyotettyArvoDTO>();

  @Schema(name = "Jonosijan tallennetut funktiotulokset")
  private List<FunktioTulosDTO> funktioTulokset = new ArrayList<FunktioTulosDTO>();

  @Schema(name = "Onko jonosijaa muokattu manuaalisesti")
  private boolean muokattu = false;

  @Schema(name = "Onko hakemus hylätty välisijoittelussa")
  private boolean hylattyValisijoittelussa = false;

  public List<String> getHistoriat() {
    return historiat;
  }

  public void setHistoriat(List<String> historiat) {
    this.historiat = historiat;
  }

  public SortedSet<JarjestyskriteeritulosDTO> getJarjestyskriteerit() {
    return jarjestyskriteerit;
  }

  public void setJarjestyskriteerit(SortedSet<JarjestyskriteeritulosDTO> jarjestyskriteerit) {
    this.jarjestyskriteerit = jarjestyskriteerit;
  }

  public String getHakijaOid() {
    return hakijaOid;
  }

  public void setHakijaOid(String hakijaOid) {
    this.hakijaOid = hakijaOid != null ? hakijaOid.intern() : null;
  }

  public String getHakemusOid() {
    return hakemusOid;
  }

  public void setHakemusOid(String hakemusOid) {
    this.hakemusOid = hakemusOid != null ? hakemusOid.intern() : null;
  }

  public int getJonosija() {
    return jonosija;
  }

  public void setJonosija(int jonosija) {
    this.jonosija = jonosija;
  }

  public void setEtunimi(String etunimi) {
    this.etunimi = etunimi != null ? etunimi.intern() : null;
  }

  public void setSukunimi(String sukunimi) {
    this.sukunimi = sukunimi != null ? sukunimi.intern() : null;
  }

  public void setPrioriteetti(int prioriteetti) {
    this.prioriteetti = prioriteetti;
  }

  public int getPrioriteetti() {
    return prioriteetti;
  }

  public String getSukunimi() {
    return sukunimi;
  }

  public String getEtunimi() {
    return etunimi;
  }

  public JarjestyskriteerituloksenTila getTuloksenTila() {
    return tuloksenTila;
  }

  public void setTuloksenTila(JarjestyskriteerituloksenTila tuloksenTila) {
    this.tuloksenTila = tuloksenTila;
  }

  public boolean isHarkinnanvarainen() {
    return harkinnanvarainen;
  }

  public void setHarkinnanvarainen(boolean harkinnanvarainen) {
    this.harkinnanvarainen = harkinnanvarainen;
  }

  public boolean isMuokattu() {
    return muokattu;
  }

  public void setMuokattu(boolean muokattu) {
    this.muokattu = muokattu;
  }

  public List<SyotettyArvoDTO> getSyotetytArvot() {
    return syotetytArvot;
  }

  public void setSyotetytArvot(List<SyotettyArvoDTO> syotetytArvot) {
    this.syotetytArvot = syotetytArvot;
  }

  public List<FunktioTulosDTO> getFunktioTulokset() {
    return funktioTulokset;
  }

  public void setFunktioTulokset(List<FunktioTulosDTO> funktioTulokset) {
    this.funktioTulokset = funktioTulokset;
  }

  public boolean isHylattyValisijoittelussa() {
    return hylattyValisijoittelussa;
  }

  public void setHylattyValisijoittelussa(boolean hylattyValisijoittelussa) {
    this.hylattyValisijoittelussa = hylattyValisijoittelussa;
  }
}
