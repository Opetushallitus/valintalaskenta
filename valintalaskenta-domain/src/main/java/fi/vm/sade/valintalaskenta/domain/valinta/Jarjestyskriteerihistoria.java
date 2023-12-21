package fi.vm.sade.valintalaskenta.domain.valinta;

import java.util.Date;
import java.util.List;
import java.util.UUID;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.Transient;

public class Jarjestyskriteerihistoria {

  public static final String FILENAME_PREFIX = "JARHIS_", FILENAME_SUFFIX = ".zip";

  @Id private Long id;

  private UUID tunniste;

  private String historia;

  @Transient private byte[] historiaGzip;

  private boolean laskettuUudelleen = false;

  private Date createdAt = new Date();

  public static final List<String> TAGS = List.of("valintalaskenta", "jarjestyskriteerihistoria");

  public Long getId() {
    return id;
  }

  public String getFilename() {
    return String.join("", FILENAME_PREFIX, this.getTunniste().toString(), FILENAME_SUFFIX);
  }

  public void setId(Long id) {
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

  public boolean isLaskettuUudelleen() {
    return laskettuUudelleen;
  }

  public void setLaskettuUudelleen(boolean laskettuUudelleen) {
    this.laskettuUudelleen = laskettuUudelleen;
  }

  public Date getCreatedAt() {
    return createdAt;
  }

  public void setCreatedAt(Date createdAt) {
    this.createdAt = createdAt;
  }

  public UUID getTunniste() {
    return tunniste;
  }

  public void setTunniste(UUID tunniste) {
    this.tunniste = tunniste;
  }
}
