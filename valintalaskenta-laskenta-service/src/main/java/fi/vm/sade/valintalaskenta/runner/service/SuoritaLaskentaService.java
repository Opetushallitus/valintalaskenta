package fi.vm.sade.valintalaskenta.runner.service;

import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaDto;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import org.springframework.stereotype.Service;

@Service
public interface SuoritaLaskentaService {

  CompletableFuture<String> suoritaLaskentaHakukohteille(
      LaskentaDto laskenta, Collection<String> hakukohdeOids);
}
