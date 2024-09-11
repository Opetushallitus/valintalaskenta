package fi.vm.sade.valinta.kooste.external.resource.valintalaskenta.impl;

import com.google.gson.reflect.TypeToken;
import fi.vm.sade.valinta.kooste.external.resource.valintalaskenta.ValintalaskentaValintakoeAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.viestintapalvelu.RestCasClient;
import fi.vm.sade.valinta.kooste.url.UrlConfiguration;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeOsallistuminenDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.HakemusOsallistuminenDTO;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

@Service
public class ValintalaskentaValintakoeAsyncResourceImpl
    implements ValintalaskentaValintakoeAsyncResource {
  private final RestCasClient httpClient;

  private final UrlConfiguration urlConfiguration;

  @Autowired
  public ValintalaskentaValintakoeAsyncResourceImpl(
      @Qualifier("ValintalaskentaCasClient") RestCasClient httpClient) {
    this.httpClient = httpClient;
    this.urlConfiguration = UrlConfiguration.getInstance();
  }

  @Override
  public CompletableFuture<List<ValintakoeOsallistuminenDTO>> haeHakutoiveelle(
      String hakukohdeOid) {
    return httpClient.get(
        this.urlConfiguration.url(
            "valintalaskenta-laskenta-service.valintakoe.hakutoive.hakukohdeoid", hakukohdeOid),
        new TypeToken<List<ValintakoeOsallistuminenDTO>>() {},
        Collections.emptyMap(),
        60 * 1000);
  }

  @Override
  public CompletableFuture<List<ValintakoeOsallistuminenDTO>> haeHakutoiveille(
      Collection<String> hakukohdeOids) {
    return httpClient.post(
        this.urlConfiguration.url("valintalaskenta-laskenta-service.valintakoe.hakutoive"),
        new TypeToken<List<ValintakoeOsallistuminenDTO>>() {},
        hakukohdeOids,
        Collections.emptyMap(),
        5 * 60 * 1000);
  }

  @Override
  public CompletableFuture<ValintakoeOsallistuminenDTO> haeHakemukselle(String hakemusOid) {
    return httpClient.get(
        this.urlConfiguration.url(
            "valintalaskenta-laskenta-service.valintakoe.hakemus", hakemusOid),
        new TypeToken<ValintakoeOsallistuminenDTO>() {},
        Collections.emptyMap(),
        60 * 1000);
  }

  @Override
  public CompletableFuture<List<HakemusOsallistuminenDTO>> haeValintatiedotHakukohteelle(
      String hakukohdeOid, List<String> valintakoeTunnisteet) {
    return httpClient.post(
        this.urlConfiguration.url(
            "valintalaskenta-laskenta-service.valintatieto.hakukohde", hakukohdeOid),
        new TypeToken<List<HakemusOsallistuminenDTO>>() {},
        valintakoeTunnisteet,
        Collections.emptyMap(),
        5 * 60 * 1000);
  }
}
