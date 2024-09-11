package fi.vm.sade.valinta.kooste.valintalaskenta.actor;

import java.util.concurrent.atomic.AtomicInteger;

public class Laskuri {
  private final AtomicInteger laskuri;
  private final int yhteensa;

  public Laskuri(int tehtavia) {
    this.yhteensa = tehtavia;
    this.laskuri = new AtomicInteger(tehtavia);
  }

  protected int tiputaLaskuria() {
    return laskuri.decrementAndGet();
  }

  public boolean isDone() {
    return laskuri.get() <= 0;
  }

  public boolean isOverDone() {
    return laskuri.get() < 0;
  }

  public int getYhteensa() {
    return yhteensa;
  }
}
