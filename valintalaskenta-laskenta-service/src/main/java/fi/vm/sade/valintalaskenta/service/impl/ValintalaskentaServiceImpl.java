package fi.vm.sade.valintalaskenta.service.impl;

import java.util.List;

import javax.jws.WebService;

import org.springframework.beans.factory.annotation.Autowired;

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
    public String laske(String hakukohdeOid, int valinnanVaihe, List<HakemusTyyppi> hakemus,
            List<ValintaperusteetTyyppi> valintaperusteet) {
        valintalaskentaSuorittaja.suoritaLaskenta(hakemus, valintaperusteet);
        return "Laskenta suoritettu onnistuneesti!";
    }

}
