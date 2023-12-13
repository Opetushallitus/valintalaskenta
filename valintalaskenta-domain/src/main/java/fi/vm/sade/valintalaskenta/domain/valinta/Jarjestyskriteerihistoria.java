package fi.vm.sade.valintalaskenta.domain.valinta;

import java.util.List;
import java.util.UUID;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.Transient;

public class Jarjestyskriteerihistoria {

  public static final String FILENAME_PREFIX = "JARHIS_", FILENAME_SUFFIX = ".zip";

  @Id private UUID id;

  private String historia;

  @Transient private byte[] historiaGzip;

  public static final List<String> TAGS = List.of("valintalaskenta", "jarjestyskriteerihistoria");

  public UUID getId() {
    return id;
  }

  public String getFilename() {
    return String.join("", FILENAME_PREFIX, this.getId().toString(), FILENAME_SUFFIX);
  }

  public void setId(UUID id) {
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
}
