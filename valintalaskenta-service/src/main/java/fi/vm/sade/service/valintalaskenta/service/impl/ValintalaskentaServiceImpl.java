package fi.vm.sade.service.valintalaskenta.service.impl;

import java.util.List;

import javax.jws.WebMethod;
import javax.jws.WebResult;
import javax.jws.WebService;
import javax.xml.ws.RequestWrapper;
import javax.xml.ws.ResponseWrapper;

import org.springframework.beans.factory.annotation.Autowired;

import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.valintalaskenta.ValintalaskentaService;
import fi.vm.sade.service.valintalaskenta.service.ValintalaskentaSuorittajaService;
import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteetTyyppi;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@WebService(endpointInterface = "fi.vm.sade.service.valintalaskenta.ValintalaskentaService")
public class ValintalaskentaServiceImpl implements ValintalaskentaService {

    @Autowired
    private ValintalaskentaSuorittajaService valintalaskentaSuorittaja;

    @Override
    @WebResult(name = "status", targetNamespace = "")
    @RequestWrapper(localName = "laske", targetNamespace = "http://valintalaskenta.service.sade.vm.fi/messages", className = "fi.vm.sade.service.valintalaskenta.messages.LaskeTyyppi")
    @WebMethod
    @ResponseWrapper(localName = "laskeVastaus", targetNamespace = "http://valintalaskenta.service.sade.vm.fi/messages", className = "fi.vm.sade.service.valintalaskenta.messages.LaskeVastausTyyppi")
    public String laske(String hakukohdeOid, int valinnanVaihe, List<HakemusTyyppi> hakemus,
            List<ValintaperusteetTyyppi> valintaperusteet) {
        valintalaskentaSuorittaja.suoritaLaskenta(hakemus, valintaperusteet);
        return "Laskenta suoritettu onnistuneesti!";
    }

}
