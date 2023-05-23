package fi.vm.sade.valintalaskenta.domain.valinta;

import dev.morphia.annotations.Entity;
import dev.morphia.annotations.Id;
import dev.morphia.annotations.PrePersist;
import org.bson.types.ObjectId;

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
