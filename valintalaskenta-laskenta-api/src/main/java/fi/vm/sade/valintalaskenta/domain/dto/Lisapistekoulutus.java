package fi.vm.sade.valintalaskenta.domain.dto;

public enum Lisapistekoulutus {
  LISAKOULUTUS_KYMPPI("1.2.246.562.5.2013112814572435044876"),
  LISAKOULUTUS_VAMMAISTEN("1.2.246.562.5.2013112814572435755085"),
  LISAKOULUTUS_TALOUS("1.2.246.562.5.2013061010184614853416"),
  LISAKOULUTUS_AMMATTISTARTTI("1.2.246.562.5.2013112814572438136372"),
  LISAKOULUTUS_KANSANOPISTO("TODO kansanopisto oid"),
  LISAKOULUTUS_MAAHANMUUTTO("1.2.246.562.5.2013112814572441001730"),
  LISAKOULUTUS_MAAHANMUUTTO_LUKIO("1.2.246.562.5.2013112814572429142840"),
  LISAKOULUTUS_VALMA("valma"),
  LISAKOULUTUS_TELMA("telma"),
  LISAKOULUTUS_OPISTOVUOSI("vstoppivelvollisillesuunnattukoulutus");

  public String komoOid;

  Lisapistekoulutus(String komoOid) {
    this.komoOid = komoOid;
  }
}
