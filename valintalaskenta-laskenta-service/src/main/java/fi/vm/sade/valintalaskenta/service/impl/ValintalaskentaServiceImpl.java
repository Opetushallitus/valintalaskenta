package fi.vm.sade.valintalaskenta.service.impl;

import java.util.List;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;
import javax.jws.WebService;
import javax.xml.ws.RequestWrapper;
import javax.xml.ws.ResponseWrapper;

import org.springframework.beans.factory.annotation.Autowired;

import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.valintalaskenta.ValintalaskentaService;
import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteetTyyppi;
import fi.vm.sade.valintalaskenta.service.ValintalaskentaSuorittajaService;

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
	@RequestWrapper(localName = "laske", targetNamespace = "http://valintalaskenta.service.sade.vm.fi/messages", className = "fi.vm.sade.service.valintalaskenta.messages.ValintalaskentaTyyppi")
	@WebMethod
	@ResponseWrapper(localName = "laskeVastaus", targetNamespace = "http://valintalaskenta.service.sade.vm.fi/messages", className = "fi.vm.sade.service.valintalaskenta.messages.LaskeVastausTyyppi")
	public String laske(@WebParam(name = "hakemus", targetNamespace = "") List<HakemusTyyppi> hakemus, @WebParam(name = "valintaperuste", targetNamespace = "") List<ValintaperusteetTyyppi> valintaperuste) {
		// TODO Auto-generated method stub
		return null;
	}

    /**
     * Metodi ottaa hakemuksen, valintaperusteet ja tallentaa kantaan yhden hakijan tiedot
     * @param hakemus
     * @param valintaperuste
     * @return
     */
	@Override
	@WebResult(name = "status", targetNamespace = "")
	@RequestWrapper(localName = "valintakokeet", targetNamespace = "http://valintalaskenta.service.sade.vm.fi/messages", className = "fi.vm.sade.service.valintalaskenta.messages.ValintalaskentaTyyppi")
	@WebMethod
	@ResponseWrapper(localName = "valintakokeetVastaus", targetNamespace = "http://valintalaskenta.service.sade.vm.fi/messages", className = "fi.vm.sade.service.valintalaskenta.messages.LaskeVastausTyyppi")
	public String valintakokeet(@WebParam(name = "hakemus", targetNamespace = "") HakemusTyyppi hakemus, @WebParam(name = "valintaperuste", targetNamespace = "") List<ValintaperusteetTyyppi> valintaperuste) {
		// TODO Auto-generated method stub
		return null;
	}


    // @Override
	// public String laske(@WebParam(name = "hakemus", targetNamespace = "")
	// List<HakemusTyyppi> hakemus, @WebParam(name = "valintaperuste",
	// targetNamespace = "") List<ValintaperusteetTyyppi> valintaperuste) {
	// valintalaskentaSuorittaja.suoritaLaskenta(hakemus, valintaperuste);
	// return "Laskenta suoritettu onnistuneesti!"; }
	//
	// @Override
	// public String laske(@WebParam(name = "hakukohdeOid", targetNamespace =
	// "") String s, @WebParam(name = "valinnanVaihe", targetNamespace = "") int
	// i, @WebParam(name = "hakemus", targetNamespace = "") List<HakemusTyyppi>
	// hakemusTyyppis, @WebParam(name = "valintaperusteet", targetNamespace =
	// "") List<ValintaperusteetTyyppi> valintaperusteetTyyppis) {
	// return null; //To change body of implemented methods use File | Settings
	// | File Templates.
	// }
}
