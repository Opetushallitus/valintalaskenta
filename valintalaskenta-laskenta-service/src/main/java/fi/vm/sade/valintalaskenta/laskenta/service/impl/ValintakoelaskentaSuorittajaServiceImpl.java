package fi.vm.sade.valintalaskenta.laskenta.service.impl;

import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.valintaperusteet.laskenta.api.LaskentaService;
import fi.vm.sade.service.valintaperusteet.schema.ValintakoeTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintakoeValinnanVaiheTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteetTyyppi;
import fi.vm.sade.valintalaskenta.laskenta.service.ValintakoelaskentaSuorittajaService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * User: wuoti
 * Date: 2.5.2013
 * Time: 9.16
 */
@Service
public class ValintakoelaskentaSuorittajaServiceImpl implements ValintakoelaskentaSuorittajaService {

    @Autowired
    private LaskentaService laskentaService;

    @Override
    public String laske(HakemusTyyppi hakemus, List<ValintaperusteetTyyppi> valintaperusteet) {

        for (ValintaperusteetTyyppi vp : valintaperusteet) {
            if(vp.getValinnanVaihe() instanceof ValintakoeValinnanVaiheTyyppi) {
                ValintakoeValinnanVaiheTyyppi vaihe = (ValintakoeValinnanVaiheTyyppi) vp.getValinnanVaihe();

                for(ValintakoeTyyppi koe : vaihe.getValintakoe()) {

                }
            }
        }

        return null;
    }
}
