package fi.vm.sade.valintalaskenta.tulos.resource.impl;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.when;
import com.google.common.collect.Lists;

import fi.vm.sade.authentication.business.service.Authorizer;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetValinnanVaiheDTO;
import fi.vm.sade.service.valintaperusteet.resource.ValintaperusteetResource;
import fi.vm.sade.sharedutils.FakeAuthenticationInitialiser;
import fi.vm.sade.valintalaskenta.domain.dto.JonosijaDTO;
import fi.vm.sade.valintalaskenta.domain.dto.ValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.ValintatietoValintatapajonoDTO;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.springframework.mock.web.MockHttpServletRequest;

import javax.ws.rs.core.Response;
import java.util.Collections;

@RunWith(MockitoJUnitRunner.class)
public class HakukohdeResourceImplTest {
    @Mock
    private ValintalaskentaTulosService tulosService;

    @Mock
    private Authorizer authorizer;

    @Mock
    private ValintaperusteetResource valintaperusteetResource;

    @InjectMocks
    HakukohdeResourceImpl hakukohdeResource = new HakukohdeResourceImpl();
    private final ValinnanvaiheDTO valinnanvaiheFromUi = new ValinnanvaiheDTO();

    @Before
    public void setup() {
        valinnanvaiheFromUi.setValintatapajonot(Collections.singletonList(new ValintatietoValintatapajonoDTO()));
        valinnanvaiheFromUi.getValintatapajonot().get(0).setJonosijat(Collections.singletonList(new JonosijaDTO()));
        valinnanvaiheFromUi.setValinnanvaiheoid("valinnanVaiheFoundId");

        ValintaperusteetValinnanVaiheDTO valinnanVaihe = new ValintaperusteetValinnanVaiheDTO();
        valinnanVaihe.setValinnanVaiheOid("valinnanVaiheFoundId");

        ValintaperusteetDTO valintaperusteet = new ValintaperusteetDTO();
        valintaperusteet.setValinnanVaihe(valinnanVaihe);

        when(valintaperusteetResource.haeValintaperusteet("hakukohdeoid", null)).thenReturn(Lists.newArrayList(valintaperusteet));
        FakeAuthenticationInitialiser.fakeAuthentication();
    }

    @Test
    public void whenValinnanvaiheIsFoundFromValintaperusteet200IsReturned() {
        valinnanvaiheFromUi.setValinnanvaiheoid("valinnanVaiheFoundId");
        Response response = hakukohdeResource.lisaaTuloksia("hakukohdeoid", "tarjoajaoid", valinnanvaiheFromUi, new MockHttpServletRequest());
        assertEquals(202, response.getStatus());
    }

    @Test
    public void whenValinnanvaiheIsNotFoundFromValintaperusteet500IsReturned() {
        valinnanvaiheFromUi.setValinnanvaiheoid("valinnanVaiheNotFoundId");
        Response response = hakukohdeResource.lisaaTuloksia("hakukohdeoid", "tarjoajaoid", valinnanvaiheFromUi, new MockHttpServletRequest());
        assertEquals(500, response.getStatus());
    }

    @Test
    public void emptyInputIsNotAllowed() {
        valinnanvaiheFromUi.setValintatapajonot(null);
        assertEquals(400, hakukohdeResource.lisaaTuloksia("hakukohdeoid", "tarjoajaoid", valinnanvaiheFromUi, new MockHttpServletRequest()).getStatus());

        valinnanvaiheFromUi.setValintatapajonot(Collections.emptyList());
        assertEquals(400, hakukohdeResource.lisaaTuloksia("hakukohdeoid", "tarjoajaoid", valinnanvaiheFromUi, new MockHttpServletRequest()).getStatus());

        valinnanvaiheFromUi.setValintatapajonot(Collections.singletonList(new ValintatietoValintatapajonoDTO()));
        assertEquals(400, hakukohdeResource.lisaaTuloksia("hakukohdeoid", "tarjoajaoid", valinnanvaiheFromUi, new MockHttpServletRequest()).getStatus());

        valinnanvaiheFromUi.getValintatapajonot().get(0).setJonosijat(Collections.singletonList(new JonosijaDTO()));
        assertEquals(202, hakukohdeResource.lisaaTuloksia("hakukohdeoid", "tarjoajaoid", valinnanvaiheFromUi, new MockHttpServletRequest()).getStatus());
    }
}
