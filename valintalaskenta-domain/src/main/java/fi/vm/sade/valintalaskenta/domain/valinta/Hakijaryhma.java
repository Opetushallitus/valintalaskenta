package fi.vm.sade.valintalaskenta.domain.valinta;

import org.springframework.data.annotation.Id;

import java.util.Date;
import java.util.UUID;


public class Hakijaryhma {

  @Id
  public UUID id;

  public String hakijaryhmaOid;

  public int prioriteetti;

  public Date createdAt;

  public String hakukohdeOid;

  public String nimi;

  public String kuvaus;

  public int kiintio;

  public boolean kaytaKaikki;


  public boolean tarkkaKiintio;


  public boolean kaytetaanRyhmaanKuuluvia;

  public String hakijaryhmatyyppiKoodiuri;

  public String valintatapajonoOid;
}
