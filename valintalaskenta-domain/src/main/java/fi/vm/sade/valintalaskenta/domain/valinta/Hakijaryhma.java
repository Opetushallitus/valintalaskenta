package fi.vm.sade.valintalaskenta.domain.valinta;

import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.PersistenceCreator;

import java.util.*;


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

  public final Set<Jonosija> jonosija = new HashSet<>();

  @PersistenceCreator
  public Hakijaryhma(Collection <Jonosija> jonosija) {
    this.jonosija.addAll(jonosija);
  }

  public Hakijaryhma() {}

  public void setJonosija(Collection<Jonosija> jonosija) {
    this.jonosija.clear();
    this.jonosija.addAll(jonosija);
  }
}
