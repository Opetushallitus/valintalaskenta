package fi.vm.sade.valintalaskenta.laskenta.service.impl;

import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.valintalaskenta.ValintalaskentaService;
import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteetTyyppi;
import fi.vm.sade.valintalaskenta.laskenta.service.ValintakoelaskentaSuorittajaService;
import fi.vm.sade.valintalaskenta.laskenta.service.ValintalaskentaSuorittajaService;
import org.springframework.beans.factory.annotation.Autowired;

import javax.jws.WebParam;
import javax.jws.WebService;
import java.util.List;

/**
 * @author Jussi Jartamo
 */
@WebService(endpointInterface = "fi.vm.sade.service.valintalaskenta.ValintalaskentaService")
public class ValintalaskentaServiceImpl implements ValintalaskentaService {

    @Autowired
    private ValintalaskentaSuorittajaService valintalaskentaSuorittaja;

    @Autowired
    private ValintakoelaskentaSuorittajaService valintakoelaskentaSuorittajaService;

    @Override
    public String laske(@WebParam(name = "hakemus", targetNamespace = "") List<HakemusTyyppi> hakemus,
                        @WebParam(name = "valintaperuste", targetNamespace = "") List<ValintaperusteetTyyppi> valintaperuste) {
        valintalaskentaSuorittaja.suoritaLaskenta(hakemus, valintaperuste);
        return "Onnistui!";
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
    public String valintakokeet(@WebParam(name = "hakemus", targetNamespace = "") HakemusTyyppi hakemus,
                                @WebParam(name = "valintaperuste", targetNamespace = "") List<ValintaperusteetTyyppi> valintaperuste) {
        valintakoelaskentaSuorittajaService.laske(hakemus, valintaperuste);
        return "Onnistui!";
    }

}
