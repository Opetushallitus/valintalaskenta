package fi.vm.sade.valintalaskenta.laskenta.service.impl;

import java.util.List;

import javax.jws.WebParam;
import javax.jws.WebService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;

import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.valintalaskenta.LaskeFault_Exception;
import fi.vm.sade.service.valintalaskenta.ValintalaskentaService;
import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteetTyyppi;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.ValintalaskentaSuorittajaService;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.ValintakoelaskentaSuorittajaService;

/**
 * @author Jussi Jartamo
 */
@WebService(endpointInterface = "fi.vm.sade.service.valintalaskenta.ValintalaskentaService")
@PreAuthorize("isAuthenticated()")
public class ValintalaskentaServiceImpl implements ValintalaskentaService {

    private static final Logger logger = LoggerFactory.getLogger(ValintalaskentaServiceImpl.class);

    @Autowired
    private ValintalaskentaSuorittajaService valintalaskentaSuorittaja;

    @Autowired
    private ValintakoelaskentaSuorittajaService valintakoelaskentaSuorittajaService;

    @Override
    // @Secured({ CRUD })
    public String laske(@WebParam(name = "hakemus", targetNamespace = "") List<HakemusTyyppi> hakemus,
            @WebParam(name = "valintaperuste", targetNamespace = "") List<ValintaperusteetTyyppi> valintaperuste)
            throws LaskeFault_Exception {
        try {
            valintalaskentaSuorittaja.suoritaLaskenta(hakemus, valintaperuste);
            return "Onnistui!";
        } catch (Exception e) {
            logger.error("Laskenta epäonnistui", e);
            throw new LaskeFault_Exception(e.getMessage(), e.getCause());
        }
    }

    /**
     * Metodi ottaa hakemuksen, valintaperusteet ja tallentaa kantaan yhden
     * hakijan tiedot
     * 
     * @param hakemus
     * @param valintaperuste
     * @return
     */
    @Override
    // @Secured({ CRUD })
    public String valintakokeet(@WebParam(name = "hakemus", targetNamespace = "") HakemusTyyppi hakemus,
            @WebParam(name = "valintaperuste", targetNamespace = "") List<ValintaperusteetTyyppi> valintaperuste)
            throws LaskeFault_Exception {
        try {
            valintakoelaskentaSuorittajaService.laske(hakemus, valintaperuste);
            return "Onnistui!";
        } catch (Exception e) {
            logger.error("Valintakoevaihe epäonnistui", e);
            throw new LaskeFault_Exception(e.getMessage(), e.getCause());
        }
    }

}
