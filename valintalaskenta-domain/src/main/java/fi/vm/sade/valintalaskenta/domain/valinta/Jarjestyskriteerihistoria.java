package fi.vm.sade.valintalaskenta.domain.valinta;

import org.bson.types.ObjectId;
import org.mongodb.morphia.annotations.Entity;
import org.mongodb.morphia.annotations.Id;
import org.mongodb.morphia.annotations.PrePersist;

@Entity("Jarjestyskriteerihistoria")
public class Jarjestyskriteerihistoria {
  @Id private ObjectId id;

  private String historia;

  private byte[] historiaGzip;

  public ObjectId getId() {
    return id;
  }

  public void setId(ObjectId id) {
    this.id = id;
  }

  public byte[] getHistoriaGzip() {
    return historiaGzip;
  }

  public void setHistoriaGzip(byte[] historiaGzip) {
    this.historiaGzip = historiaGzip;
  }

  public String getHistoria() {
    return historia;
  }

  public void setHistoria(String historia) {
    this.historia = historia;
  }

  @PrePersist
  public void preventBothZipAndHistoryToBeSaved() {
    if (historia != null && historiaGzip != null) {
      historia = null;
    }
  }
}
