package fi.vm.sade.valinta.kooste.valintalaskenta.dto;

public interface LaskentaInfo {
  String getUuid();

  String getHakuOid();

  boolean isOsittainenLaskenta(); // eli maskilla aloitettu osajoukko koko laskennasta
}
