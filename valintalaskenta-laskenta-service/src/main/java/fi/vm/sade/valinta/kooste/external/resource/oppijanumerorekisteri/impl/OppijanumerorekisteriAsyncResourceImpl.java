package fi.vm.sade.valinta.kooste.external.resource.oppijanumerorekisteri.impl;

import com.google.common.collect.Lists;
import com.google.gson.reflect.TypeToken;
import fi.vm.sade.valinta.kooste.external.resource.oppijanumerorekisteri.OppijanumerorekisteriAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.oppijanumerorekisteri.dto.HenkiloCreateDTO;
import fi.vm.sade.valinta.kooste.external.resource.oppijanumerorekisteri.dto.HenkiloPerustietoDto;
import fi.vm.sade.valinta.kooste.external.resource.oppijanumerorekisteri.dto.HenkiloViiteDto;
import fi.vm.sade.valinta.kooste.external.resource.viestintapalvelu.RestCasClient;
import fi.vm.sade.valinta.kooste.url.UrlConfiguration;
import fi.vm.sade.valinta.kooste.util.CompletableFutureUtil;
import io.reactivex.Observable;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

@Service
public class OppijanumerorekisteriAsyncResourceImpl implements OppijanumerorekisteriAsyncResource {
  private final RestCasClient client;

  private final UrlConfiguration urlConfiguration;

  @Autowired
  public OppijanumerorekisteriAsyncResourceImpl(
      @Qualifier("OppijanumerorekisteriCasClient") RestCasClient client) {
    this.client = client;
    this.urlConfiguration = UrlConfiguration.getInstance();
  }

  public Observable<List<HenkiloPerustietoDto>> haeTaiLuoHenkilot(
      List<HenkiloCreateDTO> henkiloPrototyypit) {
    return Observable.fromFuture(
        this.client.post(
            this.urlConfiguration.url(
                "oppijanumerorekisteri-service.s2s.henkilo.findOrCreateMultiple"),
            new TypeToken<List<HenkiloPerustietoDto>>() {},
            henkiloPrototyypit,
            Collections.emptyMap(),
            10 * 60 * 1000));
  }

  public CompletableFuture<List<HenkiloViiteDto>> haeHenkiloOidDuplikaatit(Set<String> personOids) {
    String url =
        this.urlConfiguration.url("oppijanumerorekisteri-service.s2s.duplicatesByPersonOids");
    Map<String, Set<String>> henkiloSearchParams = new HashMap<>();
    henkiloSearchParams.put("henkiloOids", personOids);
    CompletableFuture<List<HenkiloViiteDto>> fut =
        this.client.post(
            url,
            new TypeToken<List<HenkiloViiteDto>>() {},
            henkiloSearchParams,
            Collections.emptyMap(),
            60 * 60 * 1000);
    return fut;
  }

  public CompletableFuture<Map<String, HenkiloPerustietoDto>> haeHenkilot(List<String> personOids) {
    String url =
        this.urlConfiguration.url("oppijanumerorekisteri-service.henkilo.masterHenkilosByOidList");
    return CompletableFutureUtil.sequence(
            Lists.partition(personOids, 5000).stream()
                .map(
                    chunk ->
                        this.client.post(
                            url,
                            new TypeToken<Map<String, HenkiloPerustietoDto>>() {},
                            chunk,
                            Map.of("Content-Type", "application/json"),
                            60 * 60 * 1000))
                .collect(Collectors.toList()))
        .thenApplyAsync(
            chunks ->
                chunks.stream()
                    .flatMap(m -> m.entrySet().stream())
                    .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue)));
  }
}
