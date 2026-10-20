package fi.vm.sade.valintalaskenta.runner.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTimeoutPreemptively;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyCollection;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetValinnanVaiheDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintatapajonoJarjestyskriteereillaDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.domain.dto.LaskeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.SuorituspalveluValintadataDTO;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaDto;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaTila;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaTyyppi;
import fi.vm.sade.valintalaskenta.laskenta.dao.SureHakuOidDao;
import fi.vm.sade.valintalaskenta.laskenta.resource.ValintalaskentaResourceImpl;
import fi.vm.sade.valintalaskenta.laskenta.service.valintapiste.ValintapisteService;
import fi.vm.sade.valintalaskenta.runner.resource.external.koostepalvelu.KoostepalveluAsyncResource;
import fi.vm.sade.valintalaskenta.runner.resource.external.koostepalvelu.impl.LahtotiedotException;
import fi.vm.sade.valintalaskenta.runner.resource.external.suorituspalvelu.impl.SuorituspalveluAsyncResourceImpl;
import fi.vm.sade.valintalaskenta.runner.resource.external.valintaperusteet.ValintaperusteetAsyncResource;
import fi.vm.sade.valintalaskenta.runner.service.EcsTaskManager;
import java.time.Duration;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import software.amazon.awssdk.services.cloudwatch.CloudWatchClient;

public class SuoritaLaskentaServiceImplTest {
  private static final String SUPA_HAKU_OID = "1.2.246.562.29.00000000001";
  private static final String SURE_HAKU_OID = "1.2.246.562.29.00000000002";
  private static final String HAKUKOHDE_OID = "1.2.246.562.20.00000000001";
  private static final String HAKUKOHDE_OID_2 = "1.2.246.562.20.00000000002";

  private final ValintalaskentaResourceImpl valintalaskentaResource =
      mock(ValintalaskentaResourceImpl.class);
  private final KoostepalveluAsyncResource koostepalveluAsyncResource =
      mock(KoostepalveluAsyncResource.class);
  private final ValintaperusteetAsyncResource valintaperusteetAsyncResource =
      mock(ValintaperusteetAsyncResource.class);
  private final SuorituspalveluAsyncResourceImpl suorituspalveluAsyncResource =
      mock(SuorituspalveluAsyncResourceImpl.class);
  private final ValintapisteService valintapisteService = mock(ValintapisteService.class);
  private final EcsTaskManager ecsTaskManager = mock(EcsTaskManager.class);
  private final SureHakuOidDao sureHakuOidDao = mock(SureHakuOidDao.class);

  private final SuoritaLaskentaServiceImpl service =
      new SuoritaLaskentaServiceImpl(
          valintalaskentaResource,
          koostepalveluAsyncResource,
          valintaperusteetAsyncResource,
          suorituspalveluAsyncResource,
          valintapisteService,
          mock(CloudWatchClient.class),
          ecsTaskManager,
          sureHakuOidDao,
          "test");

  @BeforeEach
  public void setUp() {
    when(sureHakuOidDao.haeSureHakuOidit()).thenReturn(Set.of(SURE_HAKU_OID));
    when(valintaperusteetAsyncResource.haeValintaperusteet(anyString(), any()))
        .thenReturn(CompletableFuture.completedFuture(List.of(valintalaskentaaKayttavaPeruste())));
    when(valintapisteService.findValintapisteetForHakemukset(anyCollection()))
        .thenReturn(Pair.of(null, Collections.emptyList()));
    doAnswer(
            invocation -> {
              ((Runnable) invocation.getArgument(0)).run();
              return null;
            })
        .when(ecsTaskManager)
        .withTaskProtection(any());
    for (String hakukohdeOid : List.of(HAKUKOHDE_OID, HAKUKOHDE_OID_2)) {
      when(suorituspalveluAsyncResource.haeValintaData(SUPA_HAKU_OID, hakukohdeOid))
          .thenReturn(CompletableFuture.completedFuture(supanValintadata(hakukohdeOid)));
      when(koostepalveluAsyncResource.haeLahtotiedot(
              any(), eq(hakukohdeOid), anyBoolean(), anyBoolean()))
          .thenReturn(CompletableFuture.completedFuture(koostepalvelunLahtotiedot(hakukohdeOid)));
    }
  }

  @Test
  public void supaHaunLaskentaKayttaaSupanTietoja() {
    service.suoritaLaskentaHakukohteille(
        laskenta(SUPA_HAKU_OID, LaskentaTyyppi.HAKU), List.of(HAKUKOHDE_OID), 1);

    assertEquals(List.of(supaHakemusOid(HAKUKOHDE_OID)), laskettujenHakemustenOidit());
  }

  @Test
  public void lahtotiedotHaetaanSupastaJaKoostepalvelustaRinnakkain() {
    // Supan vastaus valmistuu vasta, kun Koostepalvelua on kutsuttu. Jos Supan vastausta
    // odotettaisiin ennen Koostepalvelun kutsua, laskenta jäisi jumiin.
    CompletableFuture<Void> koostepalveluaKutsuttu = new CompletableFuture<>();
    when(suorituspalveluAsyncResource.haeValintaData(SUPA_HAKU_OID, HAKUKOHDE_OID))
        .thenReturn(koostepalveluaKutsuttu.thenApply(v -> supanValintadata(HAKUKOHDE_OID)));
    when(koostepalveluAsyncResource.haeLahtotiedot(
            any(), eq(HAKUKOHDE_OID), anyBoolean(), anyBoolean()))
        .thenAnswer(
            invocation -> {
              koostepalveluaKutsuttu.complete(null);
              return CompletableFuture.completedFuture(koostepalvelunLahtotiedot(HAKUKOHDE_OID));
            });

    assertTimeoutPreemptively(
        Duration.ofSeconds(5),
        () ->
            service.suoritaLaskentaHakukohteille(
                laskenta(SUPA_HAKU_OID, LaskentaTyyppi.HAKU), List.of(HAKUKOHDE_OID), 1));

    assertEquals(List.of(supaHakemusOid(HAKUKOHDE_OID)), laskettujenHakemustenOidit());
  }

  @Test
  public void laskentaEpaonnistuuKunSupanHakuEpaonnistuu() {
    when(suorituspalveluAsyncResource.haeValintaData(SUPA_HAKU_OID, HAKUKOHDE_OID))
        .thenReturn(CompletableFuture.failedFuture(new RuntimeException("Supa ei vastaa")));

    LahtotiedotException e =
        assertThrows(
            LahtotiedotException.class,
            () ->
                service.suoritaLaskentaHakukohteille(
                    laskenta(SUPA_HAKU_OID, LaskentaTyyppi.HAKU), List.of(HAKUKOHDE_OID), 1));

    assertEquals("Supa ei vastaa", e.getMessage());
    verify(valintalaskentaResource, never()).laskeKaikki(any());
  }

  @Test
  public void sureHaunLaskentaKayttaaKoostepalvelunTietoja() {
    service.suoritaLaskentaHakukohteille(
        laskenta(SURE_HAKU_OID, LaskentaTyyppi.HAKU), List.of(HAKUKOHDE_OID), 1);

    assertEquals(List.of(koosteHakemusOid(HAKUKOHDE_OID)), laskettujenHakemustenOidit());
    verify(suorituspalveluAsyncResource, never()).haeValintaData(any(), any());
  }

  @Test
  public void valintaryhmalaskentaKayttaaSupanTietoja() {
    service.suoritaLaskentaHakukohteille(
        laskenta(SUPA_HAKU_OID, LaskentaTyyppi.VALINTARYHMA),
        List.of(HAKUKOHDE_OID, HAKUKOHDE_OID_2),
        2);

    assertEquals(
        Map.of(
            HAKUKOHDE_OID, List.of(supaHakemusOid(HAKUKOHDE_OID)),
            HAKUKOHDE_OID_2, List.of(supaHakemusOid(HAKUKOHDE_OID_2))),
        valintaryhmalaskennanHakemustenOidit());
    verify(suorituspalveluAsyncResource).haeValintaData(SUPA_HAKU_OID, HAKUKOHDE_OID);
    verify(suorituspalveluAsyncResource).haeValintaData(SUPA_HAKU_OID, HAKUKOHDE_OID_2);
  }

  @Test
  public void sureHaunValintaryhmalaskentaKayttaaKoostepalvelunTietoja() {
    service.suoritaLaskentaHakukohteille(
        laskenta(SURE_HAKU_OID, LaskentaTyyppi.VALINTARYHMA),
        List.of(HAKUKOHDE_OID, HAKUKOHDE_OID_2),
        2);

    assertEquals(
        Map.of(
            HAKUKOHDE_OID, List.of(koosteHakemusOid(HAKUKOHDE_OID)),
            HAKUKOHDE_OID_2, List.of(koosteHakemusOid(HAKUKOHDE_OID_2))),
        valintaryhmalaskennanHakemustenOidit());
    verify(suorituspalveluAsyncResource, never()).haeValintaData(any(), any());
  }

  private List<String> laskettujenHakemustenOidit() {
    ArgumentCaptor<LaskeDTO> captor = ArgumentCaptor.forClass(LaskeDTO.class);
    verify(valintalaskentaResource).laskeKaikki(captor.capture());
    return captor.getValue().getHakemus().stream().map(HakemusDTO::getHakemusoid).toList();
  }

  private Map<String, List<String>> valintaryhmalaskennanHakemustenOidit() {
    @SuppressWarnings("unchecked")
    ArgumentCaptor<List<LaskeDTO>> captor = ArgumentCaptor.forClass(List.class);
    verify(valintalaskentaResource).valintaryhmaLaskenta(any(), captor.capture());
    return captor.getValue().stream()
        .collect(
            Collectors.toMap(
                LaskeDTO::getHakukohdeOid,
                laskeDTO ->
                    laskeDTO.getHakemus().stream().map(HakemusDTO::getHakemusoid).toList()));
  }

  private static String supaHakemusOid(String hakukohdeOid) {
    return "supa-" + hakukohdeOid;
  }

  private static String koosteHakemusOid(String hakukohdeOid) {
    return "kooste-" + hakukohdeOid;
  }

  private static SuorituspalveluValintadataDTO supanValintadata(String hakukohdeOid) {
    return new SuorituspalveluValintadataDTO(List.of(hakemus(supaHakemusOid(hakukohdeOid))));
  }

  private static LaskeDTO koostepalvelunLahtotiedot(String hakukohdeOid) {
    return new LaskeDTO(
        "uuid",
        false,
        false,
        hakukohdeOid,
        List.of(hakemus(koosteHakemusOid(hakukohdeOid))),
        List.of(valintalaskentaaKayttavaPeruste()),
        Collections.emptyList());
  }

  private static HakemusDTO hakemus(String hakemusOid) {
    HakemusDTO hakemus = new HakemusDTO();
    hakemus.setHakemusoid(hakemusOid);
    hakemus.setHakijaOid("1.2.246.562.24.00000000001");
    return hakemus;
  }

  private static ValintaperusteetDTO valintalaskentaaKayttavaPeruste() {
    ValintatapajonoJarjestyskriteereillaDTO jono = new ValintatapajonoJarjestyskriteereillaDTO();
    jono.setKaytetaanValintalaskentaa(true);
    ValintaperusteetValinnanVaiheDTO valinnanVaihe = new ValintaperusteetValinnanVaiheDTO();
    valinnanVaihe.setValintatapajono(List.of(jono));
    ValintaperusteetDTO peruste = new ValintaperusteetDTO();
    peruste.setValinnanVaihe(valinnanVaihe);
    return peruste;
  }

  private static LaskentaDto laskenta(String hakuOid, LaskentaTyyppi tyyppi) {
    return new LaskentaDto(
        "uuid",
        "1.2.246.562.24.00000000099",
        "Testihaku",
        "Testilaskenta",
        hakuOid,
        System.currentTimeMillis(),
        LaskentaTila.MENEILLAAN,
        tyyppi,
        null,
        Collections.emptyList(),
        false,
        Optional.empty(),
        false,
        Optional.empty(),
        false);
  }
}
