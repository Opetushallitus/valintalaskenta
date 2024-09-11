package fi.vm.sade.valinta.kooste.external.resource.ataru;

public class IsAtaruHakemusOid {

  public static boolean isAtaruHakemusOid(String oid) {
    return oid != null && 35 == oid.length();
  }
}
