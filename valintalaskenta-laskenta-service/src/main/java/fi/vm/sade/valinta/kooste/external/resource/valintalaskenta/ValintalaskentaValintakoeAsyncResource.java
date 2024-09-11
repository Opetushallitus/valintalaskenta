package fi.vm.sade.valinta.kooste.external.resource.valintalaskenta;

import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeOsallistuminenDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.HakemusOsallistuminenDTO;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface ValintalaskentaValintakoeAsyncResource {
  CompletableFuture<List<ValintakoeOsallistuminenDTO>> haeHakutoiveelle(String hakukohdeOid);

  CompletableFuture<List<ValintakoeOsallistuminenDTO>> haeHakutoiveille(
      Collection<String> hakukohdeOids);

  CompletableFuture<ValintakoeOsallistuminenDTO> haeHakemukselle(String hakemusOid);

  CompletableFuture<List<HakemusOsallistuminenDTO>> haeValintatiedotHakukohteelle(
      String hakukohdeOid, List<String> valintakoeTunnisteet);
}
