package fi.vm.sade.valintalaskenta.tulos.resource.impl;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.when;

import com.google.common.collect.Lists;
import fi.vm.sade.javautils.opintopolku_spring_security.Authorizer;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetValinnanVaiheDTO;
import fi.vm.sade.service.valintaperusteet.resource.ValintaperusteetResource;
import fi.vm.sade.valinta.sharedutils.FakeAuthenticationInitialiser;
import fi.vm.sade.valintalaskenta.domain.dto.JonosijaDTO;
import fi.vm.sade.valintalaskenta.domain.dto.ValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.ValintatietoValintatapajonoDTO;
import fi.vm.sade.valintalaskenta.tulos.logging.LaskentaAuditLogMock;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import java.util.Collections;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import javax.ws.rs.core.Response;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.runners.MockitoJUnitRunner;
import org.springframework.mock.web.MockHttpServletRequest;

@RunWith(MockitoJUnitRunner.class)
public class HakukohdeResourceImplTest {
  @Mock private ValintalaskentaTulosService tulosService;

  @Mock private Authorizer authorizer;

  @Mock private ValintaperusteetResource valintaperusteetResource;

  @InjectMocks
  HakukohdeResourceImpl hakukohdeResource = new HakukohdeResourceImpl(new LaskentaAuditLogMock());

  private final ValinnanvaiheDTO valinnanvaiheFromUi = new ValinnanvaiheDTO();

  @Before
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
    HttpServletRequest request = Mockito.mock(HttpServletRequest.class);
    HttpSession session = Mockito.mock(HttpSession.class);
    Mockito.when(request.getSession(false)).thenReturn(session);

    valinnanvaiheFromUi.setValinnanvaiheoid("valinnanVaiheFoundId");
    Response response =
        hakukohdeResource.lisaaTuloksia(
            "hakukohdeoid", "tarjoajaoid", valinnanvaiheFromUi, request);
    assertEquals(202, response.getStatus());
  }

  @Test
  public void whenValinnanvaiheIsNotFoundFromValintaperusteet500IsReturned() {
    valinnanvaiheFromUi.setValinnanvaiheoid("valinnanVaiheNotFoundId");
    Response response =
        hakukohdeResource.lisaaTuloksia(
            "hakukohdeoid", "tarjoajaoid", valinnanvaiheFromUi, new MockHttpServletRequest());
    assertEquals(500, response.getStatus());
  }

  @Test
  public void emptyInputIsNotAllowed() {
    HttpServletRequest request = Mockito.mock(HttpServletRequest.class);
    HttpSession session = Mockito.mock(HttpSession.class);
    Mockito.when(request.getSession(false)).thenReturn(session);
    valinnanvaiheFromUi.setValintatapajonot(null);
    assertEquals(
        400,
        hakukohdeResource
            .lisaaTuloksia("hakukohdeoid", "tarjoajaoid", valinnanvaiheFromUi, request)
            .getStatus());

    valinnanvaiheFromUi.setValintatapajonot(Collections.emptyList());
    assertEquals(
        400,
        hakukohdeResource
            .lisaaTuloksia("hakukohdeoid", "tarjoajaoid", valinnanvaiheFromUi, request)
            .getStatus());

    valinnanvaiheFromUi.setValintatapajonot(
        Collections.singletonList(new ValintatietoValintatapajonoDTO()));
    assertEquals(
        400,
        hakukohdeResource
            .lisaaTuloksia("hakukohdeoid", "tarjoajaoid", valinnanvaiheFromUi, request)
            .getStatus());

    valinnanvaiheFromUi
        .getValintatapajonot()
        .get(0)
        .setJonosijat(Collections.singletonList(new JonosijaDTO()));
    assertEquals(
        202,
        hakukohdeResource
            .lisaaTuloksia("hakukohdeoid", "tarjoajaoid", valinnanvaiheFromUi, request)
            .getStatus());
  }
}
