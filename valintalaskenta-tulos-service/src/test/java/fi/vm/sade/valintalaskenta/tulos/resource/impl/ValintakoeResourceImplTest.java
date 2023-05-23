package fi.vm.sade.valintalaskenta.tulos.resource.impl;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeOsallistuminenDTO;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import java.util.Arrays;
import java.util.List;
import org.junit.jupiter.api.Test;

public class ValintakoeResourceImplTest {
  private final ValintalaskentaTulosService serviceMock = mock(ValintalaskentaTulosService.class);
  private ValintakoeResourceImpl resource =
      new ValintakoeResourceImpl(serviceMock, new ValintalaskentaModelMapper());
  private final String hakukohdeOid = "1.2.this.is.hakukohde";
  private ValintakoeOsallistuminen osallistuminen1 =
      mock(ValintakoeOsallistuminen.class, "MOCK-osallistuminen 1");
  private ValintakoeOsallistuminen osallistuminen2 =
      mock(ValintakoeOsallistuminen.class, "MOCK-osallistuminen 2");

  @Test
  public void resourceRetrievesOsallistumisetByHakukohde() {
    when(osallistuminen1.getHakemusOid()).thenReturn("hakemus1Oid");
    when(osallistuminen2.getHakemusOid()).thenReturn("hakemus2Oid");
    when(serviceMock.haeValintakoeOsallistumisetByHakutoive(hakukohdeOid))
        .thenReturn(Arrays.asList(osallistuminen1, osallistuminen2));

    List<ValintakoeOsallistuminenDTO> osallistumiset = resource.hakuByHakutoive(hakukohdeOid);
    assertThat(osallistumiset, hasSize(2));
    assertEquals("hakemus1Oid", osallistumiset.get(0).getHakemusOid());
    assertEquals("hakemus2Oid", osallistumiset.get(1).getHakemusOid());

    verify(serviceMock).haeValintakoeOsallistumisetByHakutoive(hakukohdeOid);
    verifyNoMoreInteractions(serviceMock);
  }
}
