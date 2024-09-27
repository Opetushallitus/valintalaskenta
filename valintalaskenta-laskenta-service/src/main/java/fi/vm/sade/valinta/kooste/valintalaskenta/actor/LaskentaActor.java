package fi.vm.sade.valinta.kooste.valintalaskenta.actor;

public interface LaskentaActor {
  String getHakuOid();

  boolean isValmis();

  void start();

  void lopeta();
}
