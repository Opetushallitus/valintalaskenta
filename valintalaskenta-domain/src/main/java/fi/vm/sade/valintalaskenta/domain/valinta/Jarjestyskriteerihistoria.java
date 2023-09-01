package fi.vm.sade.valintalaskenta.domain.valinta;


import javax.persistence.*;

@Entity
@Table(name = "jarjestyskriteerihistoria")
public class Jarjestyskriteerihistoria {
  @Id
  @GeneratedValue(strategy = GenerationType.AUTO)
  private Long id;

  private String historia;

  private byte[] historiaGzip;

  public Long getId() {
    return id;
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

  @PrePersist
  public void preventBothZipAndHistoryToBeSaved() {
    if (historia != null && historiaGzip != null) {
      historia = null;
    }
  }
}
