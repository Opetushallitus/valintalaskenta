package fi.vm.sade.valintalaskenta.laskenta.dao;

import fi.vm.sade.valintalaskenta.domain.valinta.Hakijaryhma;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;

import java.util.Date;
import java.util.UUID;

@Table("hakijaryhma_history")
public class HakijaryhmaHistory {

  @Id
  public UUID id;

  public String hakijaryhmaOid;

  public int prioriteetti;

  public Date createdAt = new Date();

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
