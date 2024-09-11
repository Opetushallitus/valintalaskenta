package fi.vm.sade.valinta.kooste.valintalaskenta.actor;

import akka.actor.TypedActor;

public interface LaskentaActor extends TypedActor.PostStop {
  String getHakuOid();

  boolean isValmis();

  void start();

  void lopeta();
}
