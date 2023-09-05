package fi.vm.sade.valintalaskenta.domain.valinta;


import javax.persistence.*;

public class Jarjestyskriteerihistoria {

  private String id;

  private String historia;

  private byte[] historiaGzip;

  public String getId() {
    return id;
  }

  public void setId(String id) {
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

  public void preventBothZipAndHistoryToBeSaved() {
    if (historia != null && historiaGzip != null) {
      historia = null;
    }
  }
}
