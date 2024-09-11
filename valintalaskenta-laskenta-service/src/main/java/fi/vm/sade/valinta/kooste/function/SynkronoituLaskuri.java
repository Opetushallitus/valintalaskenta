package fi.vm.sade.valinta.kooste.function;

import java.util.concurrent.atomic.AtomicInteger;

public class SynkronoituLaskuri {

  private final SynkronoituToiminto synkronoituToiminto;
  private final AtomicInteger laskuri;
  private final boolean suoritaVainKerran;
  private final SynkronoituToiminto suoritaJokaKerta;

  private SynkronoituLaskuri() {
    this.synkronoituToiminto = null;
    this.laskuri = null;
    this.suoritaVainKerran = true;
    this.suoritaJokaKerta = null;
  }

  private SynkronoituLaskuri(
      int laskurinAlkuarvo,
      boolean suoritaVainKerran,
      SynkronoituToiminto suoritaJokaKerta,
      SynkronoituToiminto synkronoituToiminto) {
    this.synkronoituToiminto = synkronoituToiminto;
    this.laskuri = new AtomicInteger(laskurinAlkuarvo);
    this.suoritaVainKerran = suoritaVainKerran;
    this.suoritaJokaKerta = suoritaJokaKerta;
  }

  public void vahennaLaskuriaJaJosValmisNiinSuoritaToiminto() {
    int laskuriNyt = this.laskuri.decrementAndGet();
    suoritaJokaKerta.suorita();
    if (suoritaVainKerran && laskuriNyt == 0) {
      synkronoituToiminto.suorita();
    } else if (!suoritaVainKerran && laskuriNyt <= 0) {
      synkronoituToiminto.suorita();
    }
  }

  public static class SynkronoituLaskuriBuilder {
    private int laskurinAlkuarvo = 0;
    private SynkronoituToiminto synkronoituToiminto = () -> {};
    private SynkronoituToiminto suoritaJokaKerta = () -> {};
    private boolean suoritaVainKerran = true;

    public SynkronoituLaskuriBuilder setLaskurinAlkuarvo(int laskurinAlkuarvo) {
      this.laskurinAlkuarvo = laskurinAlkuarvo;
      return this;
    }

    public SynkronoituLaskuriBuilder setSuoritaJokaKerta(SynkronoituToiminto suoritaJokaKerta) {
      this.suoritaJokaKerta = suoritaJokaKerta;
      return this;
    }

    public SynkronoituLaskuriBuilder setSynkronoituToiminto(
        SynkronoituToiminto synkronoituToiminto) {
      this.synkronoituToiminto = synkronoituToiminto;
      return this;
    }

    public SynkronoituLaskuriBuilder setSuoritaVainKerran(boolean suoritaVainKerran) {
      this.suoritaVainKerran = suoritaVainKerran;
      return this;
    }

    public SynkronoituLaskuri build() {
      return new SynkronoituLaskuri(
          laskurinAlkuarvo, suoritaVainKerran, suoritaJokaKerta, synkronoituToiminto);
    }
  }

  public static SynkronoituLaskuriBuilder builder() {
    return new SynkronoituLaskuriBuilder();
  }
}
