package fi.vm.sade.valintalaskenta.domain.valinta;

public class Jarjestyskriteerihistoria {

  public static String FILENAME_PREFIX = "JARHIS_", FILENAME_SUFFIX = ".zip";

  private String id;

  private String historia;

  private byte[] historiaGzip;

  public String getId() {
    return id;
  }

  public String getFilename() {
    return String.join("", FILENAME_PREFIX, this.getId(), FILENAME_SUFFIX);
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
