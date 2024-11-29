package fi.vm.sade.valintalaskenta.runner.service;

import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaDto;
import java.util.*;
import org.springframework.stereotype.Service;

@Service
public interface SuoritaLaskentaService {

  void suoritaLaskentaHakukohteille(LaskentaDto laskenta, Collection<String> hakukohdeOids);
}
