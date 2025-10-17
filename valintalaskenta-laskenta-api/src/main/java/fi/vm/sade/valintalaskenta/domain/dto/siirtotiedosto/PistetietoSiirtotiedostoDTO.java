package fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto;

import com.google.gson.annotations.SerializedName;
import fi.vm.sade.valintalaskenta.domain.dto.valintapiste.Osallistumistieto;
import java.util.Date;

public record PistetietoSiirtotiedostoDTO(
    String tunniste,
    String arvo,
    Osallistumistieto osallistuminen,
    String tallettaja,
    @SerializedName("last_modified") Date lastModified) {}
