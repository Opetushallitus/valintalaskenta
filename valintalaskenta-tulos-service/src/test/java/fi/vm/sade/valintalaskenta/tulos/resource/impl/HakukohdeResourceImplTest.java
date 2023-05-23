package fi.vm.sade.valintalaskenta.tulos.resource.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
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
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockHttpServletRequest;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class HakukohdeResourceImplTest {
  private ValintalaskentaTulosService tulosService;
  private ValintaperusteetResource valintaperusteetResource;
  private Authorizer authorizer;
  private HakukohdeResourceImpl hakukohdeResource;

  private final ValinnanvaiheDTO valinnanvaiheFromUi = new ValinnanvaiheDTO();

  @BeforeEach
  public void setup() {
    tulosService = Mockito.mock(ValintalaskentaTulosService.class);
    valintaperusteetResource = Mockito.mock(ValintaperusteetResource.class);
    authorizer = Mockito.mock(Authorizer.class);
    hakukohdeResource =
        new HakukohdeResourceImpl(
            new LaskentaAuditLogMock(), tulosService, authorizer, valintaperusteetResource);
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
    ResponseEntity<Object> response =
        hakukohdeResource.lisaaTuloksia(
            "hakukohdeoid", "tarjoajaoid", valinnanvaiheFromUi, request);
    assertEquals(202, response.getStatusCodeValue());
  }

  @Test
  public void whenValinnanvaiheIsNotFoundFromValintaperusteet500IsReturned() {
    valinnanvaiheFromUi.setValinnanvaiheoid("valinnanVaiheNotFoundId");
    ResponseEntity<Object> response =
        hakukohdeResource.lisaaTuloksia(
            "hakukohdeoid", "tarjoajaoid", valinnanvaiheFromUi, new MockHttpServletRequest());
    assertEquals(500, response.getStatusCodeValue());
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
            .getStatusCodeValue());

    valinnanvaiheFromUi.setValintatapajonot(Collections.emptyList());
    assertEquals(
        400,
        hakukohdeResource
            .lisaaTuloksia("hakukohdeoid", "tarjoajaoid", valinnanvaiheFromUi, request)
            .getStatusCodeValue());

    valinnanvaiheFromUi.setValintatapajonot(
        Collections.singletonList(new ValintatietoValintatapajonoDTO()));
    assertEquals(
        400,
        hakukohdeResource
            .lisaaTuloksia("hakukohdeoid", "tarjoajaoid", valinnanvaiheFromUi, request)
            .getStatusCodeValue());

    valinnanvaiheFromUi
        .getValintatapajonot()
        .get(0)
        .setJonosijat(Collections.singletonList(new JonosijaDTO()));
    assertEquals(
        202,
        hakukohdeResource
            .lisaaTuloksia("hakukohdeoid", "tarjoajaoid", valinnanvaiheFromUi, request)
            .getStatusCodeValue());
  }
}
