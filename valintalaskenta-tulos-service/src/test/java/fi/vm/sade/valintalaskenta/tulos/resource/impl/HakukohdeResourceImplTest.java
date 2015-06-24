package fi.vm.sade.valintalaskenta.tulos.resource.impl;

import com.google.common.collect.Lists;
import fi.vm.sade.authentication.business.service.Authorizer;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetValinnanVaiheDTO;
import fi.vm.sade.service.valintaperusteet.resource.ValintaperusteetResource;
import fi.vm.sade.valintalaskenta.domain.dto.ValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.slf4j.Logger;

import javax.ws.rs.core.Response;


import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.when;

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

    @Before
    public void setup() {
        ValintaperusteetValinnanVaiheDTO valinnanVaihe = new ValintaperusteetValinnanVaiheDTO();
        valinnanVaihe.setValinnanVaiheOid("valinnanVaiheFoundId");

        ValintaperusteetDTO valintaperusteet = new ValintaperusteetDTO();
        valintaperusteet.setValinnanVaihe(valinnanVaihe);

        when(valintaperusteetResource.haeValintaperusteet("hakukohdeoid", null)).thenReturn(Lists.newArrayList(valintaperusteet));
    }

    @Test
    public void whenValinnanvaiheIsFoundFromValintaperusteet200IsReturned() {
        ValinnanvaiheDTO valinnanVaihe = new ValinnanvaiheDTO();
        valinnanVaihe.setValinnanvaiheoid("valinnanVaiheFoundId");
        Response response = hakukohdeResource.lisaaTuloksia("hakukohdeoid", "tarjoajaoid", valinnanVaihe);
        assertEquals(202, response.getStatus());
    }

    @Test
    public void whenValinnanvaiheIsNotFoundFromValintaperusteet500IsReturned() {
        ValinnanvaiheDTO valinnanVaihe = new ValinnanvaiheDTO();
        valinnanVaihe.setValinnanvaiheoid("valinnanVaiheNotFoundId");
        Response response = hakukohdeResource.lisaaTuloksia("hakukohdeoid", "tarjoajaoid", valinnanVaihe);
        assertEquals(500, response.getStatus());
    }
}
