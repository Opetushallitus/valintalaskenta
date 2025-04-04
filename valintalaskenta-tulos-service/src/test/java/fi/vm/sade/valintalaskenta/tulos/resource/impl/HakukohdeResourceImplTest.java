package fi.vm.sade.valintalaskenta.tulos.resource.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.google.common.collect.Lists;
import fi.vm.sade.javautils.opintopolku_spring_security.Authorizer;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetValinnanVaiheDTO;
import fi.vm.sade.valinta.sharedutils.FakeAuthenticationInitialiser;
import fi.vm.sade.valintalaskenta.domain.dto.JonosijaDTO;
import fi.vm.sade.valintalaskenta.domain.dto.ValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.ValintatietoValintatapajonoDTO;
import fi.vm.sade.valintalaskenta.tulos.logging.LaskentaAuditLogMock;
import fi.vm.sade.valintalaskenta.tulos.resource.external.ValintaperusteetResource;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;
import java.util.Collections;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockHttpServletRequest;

public class HakukohdeResourceImplTest {
  private final ValintalaskentaTulosService tulosService = mock(ValintalaskentaTulosService.class);

  private final Authorizer authorizer = mock(Authorizer.class);

  private final ValintaperusteetResource valintaperusteetResource =
      mock(ValintaperusteetResource.class);

  private final HakukohdeResourceImpl hakukohdeResource =
      new HakukohdeResourceImpl(
          new LaskentaAuditLogMock(), tulosService, authorizer, valintaperusteetResource);
  ;

  private final ValinnanvaiheDTO valinnanvaiheFromUi = new ValinnanvaiheDTO();

  @BeforeEach
  public void setup() {
    valinnanvaiheFromUi.setValintatapajonot(
        Collections.singletonList(new ValintatietoValintatapajonoDTO()));
    valinnanvaiheFromUi
        .getValintatapajonot()
        .get(0)
        .setJonosijat(Collections.singletonList(new JonosijaDTO()));
    valinnanvaiheFromUi.setValinnanvaiheoid("valinnanVaiheFoundId");

    ValintaperusteetValinnanVaiheDTO valinnanVaihe = new ValintaperusteetValinnanVaiheDTO();
    valinnanVaihe.setValinnanVaiheOid("valinnanVaiheFoundId");

    ValintaperusteetDTO valintaperusteet = new ValintaperusteetDTO();
    valintaperusteet.setValinnanVaihe(valinnanVaihe);

    when(valintaperusteetResource.haeValintaperusteet("hakukohdeoid", null))
        .thenReturn(Lists.newArrayList(valintaperusteet));
    FakeAuthenticationInitialiser.fakeAuthentication();
  }

  @Test
  public void whenValinnanvaiheIsFoundFromValintaperusteet200IsReturned() {
    HttpServletRequest request = mock(HttpServletRequest.class);
    HttpSession session = mock(HttpSession.class);
    Mockito.when(request.getSession(false)).thenReturn(session);

    valinnanvaiheFromUi.setValinnanvaiheoid("valinnanVaiheFoundId");
    ResponseEntity<Object> response =
        hakukohdeResource.lisaaTuloksia(
            "hakukohdeoid", "tarjoajaoid", valinnanvaiheFromUi, request);
    assertEquals(202, response.getStatusCode().value());
  }

  @Test
  public void whenValinnanvaiheIsNotFoundFromValintaperusteet500IsReturned() {
    valinnanvaiheFromUi.setValinnanvaiheoid("valinnanVaiheNotFoundId");
    ResponseEntity<Object> response =
        hakukohdeResource.lisaaTuloksia(
            "hakukohdeoid", "tarjoajaoid", valinnanvaiheFromUi, new MockHttpServletRequest());
    assertEquals(500, response.getStatusCode().value());
  }

  @Test
  public void emptyInputIsNotAllowed() {
    HttpServletRequest request = mock(HttpServletRequest.class);
    HttpSession session = mock(HttpSession.class);
    Mockito.when(request.getSession(false)).thenReturn(session);
    valinnanvaiheFromUi.setValintatapajonot(null);
    assertEquals(
        400,
        hakukohdeResource
            .lisaaTuloksia("hakukohdeoid", "tarjoajaoid", valinnanvaiheFromUi, request)
            .getStatusCode()
            .value());

    valinnanvaiheFromUi.setValintatapajonot(Collections.emptyList());
    assertEquals(
        400,
        hakukohdeResource
            .lisaaTuloksia("hakukohdeoid", "tarjoajaoid", valinnanvaiheFromUi, request)
            .getStatusCode()
            .value());

    valinnanvaiheFromUi.setValintatapajonot(
        Collections.singletonList(new ValintatietoValintatapajonoDTO()));
    assertEquals(
        400,
        hakukohdeResource
            .lisaaTuloksia("hakukohdeoid", "tarjoajaoid", valinnanvaiheFromUi, request)
            .getStatusCode()
            .value());

    valinnanvaiheFromUi
        .getValintatapajonot()
        .get(0)
        .setJonosijat(Collections.singletonList(new JonosijaDTO()));
    assertEquals(
        202,
        hakukohdeResource
            .lisaaTuloksia("hakukohdeoid", "tarjoajaoid", valinnanvaiheFromUi, request)
            .getStatusCode()
            .value());
  }
}
