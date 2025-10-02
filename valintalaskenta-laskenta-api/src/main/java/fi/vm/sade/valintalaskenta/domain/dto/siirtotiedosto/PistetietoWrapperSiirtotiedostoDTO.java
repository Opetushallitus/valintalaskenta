package fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto;

import java.util.List;

public record PistetietoWrapperSiirtotiedostoDTO(
    String hakemusOID, List<PistetietoSiirtotiedostoDTO> pisteet) {}
