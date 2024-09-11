package fi.vm.sade.valinta.kooste.external.resource.valintatulosservice.dto;

import java.util.Date;
import java.util.Map;
import scala.Option;

public class IlmoittautumistilaDto {
  private String hakukohdeOid;
  private String tarjoajaOid;
  private String valintatila;
  private Option<String> vastaanottotila;
  private Option<IlmoittautumistilaDto> hakuToiveenIlmoittautumisTilaDto;
  private String vastaanotettavuustila;
  private Option<Date> vastaanottoDeadline;
  private Option<Integer> jonosija;
  private Option<Date> varasijojaKaytetaanAlkaen;
  private Option<Date> varasijojaTaytetaanAsti;
  private Option<Integer> varasijanumero;

  public IlmoittautumistilaDto(
      String hakukohdeOid,
      String tarjoajaOid,
      String valintatila,
      Option<String> vastaanottotila,
      Option<IlmoittautumistilaDto> hakuToiveenIlmoittautumisTilaDto,
      String vastaanotettavuustila,
      Option<Date> vastaanottoDeadline,
      Option<Integer> jonosija,
      Option<Date> varasijojaKaytetaanAlkaen,
      Option<Date> varasijojaTaytetaanAsti,
      Option<Integer> varasijanumero,
      Map<String, String> tilanKuvaukset) {
    this.hakukohdeOid = hakukohdeOid;
    this.tarjoajaOid = tarjoajaOid;
    this.valintatila = valintatila;
    this.vastaanottotila = vastaanottotila;
    this.hakuToiveenIlmoittautumisTilaDto = hakuToiveenIlmoittautumisTilaDto;
    this.vastaanotettavuustila = vastaanotettavuustila;
    this.vastaanottoDeadline = vastaanottoDeadline;
    this.jonosija = jonosija;
    this.varasijojaKaytetaanAlkaen = varasijojaKaytetaanAlkaen;
    this.varasijojaTaytetaanAsti = varasijojaTaytetaanAsti;
    this.varasijanumero = varasijanumero;
    this.tilanKuvaukset = tilanKuvaukset;
  }

  private Map<String, String> tilanKuvaukset;
}
