package fi.vm.sade.valintalaskenta.domain.dto.seuranta;

public class TunnisteDto {
  private String uuid;
  private boolean luotiinkoUusiLaskenta;

  public TunnisteDto(String uuid, boolean luotiinkoUusiLaskenta) {
    this.uuid = uuid;
    this.luotiinkoUusiLaskenta = luotiinkoUusiLaskenta;
  }

  public String getUuid() {
    return uuid;
  }

  public boolean getLuotiinkoUusiLaskenta() {
    return luotiinkoUusiLaskenta;
  }

  public boolean isLuotiinkoUusiLaskenta() {
    return luotiinkoUusiLaskenta;
  }
}
