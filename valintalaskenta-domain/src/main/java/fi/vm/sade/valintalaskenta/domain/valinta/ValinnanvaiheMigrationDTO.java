package fi.vm.sade.valintalaskenta.domain.valinta;

import dev.morphia.annotations.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import org.bson.types.ObjectId;

@Entity(value = "Valinnanvaihe")
@Indexes(
    @Index(
        fields = {@Field("hakuOid"), @Field("valinnanvaiheOid")},
        options = @IndexOptions(name = "idx_hakuoid_valinnanvaihe_oid", unique = true)))
public class ValinnanvaiheMigrationDTO {
  @Id private ObjectId id;

  private int jarjestysnumero;

  private Date createdAt;

  @Indexed private String hakuOid;

  @Indexed private String hakukohdeOid;

  @Indexed private String valinnanvaiheOid;

  private String tarjoajaOid;

  private String nimi;

  @Reference
  private List<ValintatapajonoMigrationDTO> valintatapajonot =
      new ArrayList<ValintatapajonoMigrationDTO>();

  @PrePersist
  private void prePersist() {
    createdAt = new Date();
  }

  @PostLoad
  private void jarjestaValintatapajonot() {
    Collections.sort(valintatapajonot, (o1, o2) -> o1.getPrioriteetti() - o2.getPrioriteetti());
  }

  public ObjectId getId() {
    return this.id;
  }

  public int getJarjestysnumero() {
    return jarjestysnumero;
  }

  public void setJarjestysnumero(int jarjestysnumero) {
    this.jarjestysnumero = jarjestysnumero;
  }

  public Date getCreatedAt() {
    return createdAt;
  }

  public void setCreatedAt(Date createdAt) {
    this.createdAt = createdAt;
  }

  public String getHakuOid() {
    return hakuOid;
  }

  public void setHakuOid(String hakuOid) {
    this.hakuOid = hakuOid;
  }

  public String getHakukohdeOid() {
    return hakukohdeOid;
  }

  public void setHakukohdeOid(String hakukohdeOid) {
    this.hakukohdeOid = hakukohdeOid;
  }

  public String getValinnanvaiheOid() {
    return valinnanvaiheOid;
  }

  public void setValinnanvaiheOid(String valinnanvaiheOid) {
    this.valinnanvaiheOid = valinnanvaiheOid;
  }

  public String getTarjoajaOid() {
    return tarjoajaOid;
  }

  public void setTarjoajaOid(String tarjoajaOid) {
    this.tarjoajaOid = tarjoajaOid;
  }

  public List<ValintatapajonoMigrationDTO> getValintatapajonot() {
    return valintatapajonot;
  }

  public void setValintatapajonot(List<ValintatapajonoMigrationDTO> valintatapajonot) {
    this.valintatapajonot = valintatapajonot;
  }

  public String getNimi() {
    return nimi;
  }

  public void setNimi(String nimi) {
    this.nimi = nimi;
  }
}
