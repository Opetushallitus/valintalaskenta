package fi.vm.sade.valintalaskenta.tulos.service.impl.converters;

import fi.vm.sade.service.valintaperusteet.model.Valintatapajono;
import fi.vm.sade.service.valintatiedot.schema.HakukohdeTyyppi;
import fi.vm.sade.service.valintatiedot.schema.ValintatapajonoTyyppi;
import fi.vm.sade.valintalaskenta.domain.Hakukohde;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

/**
 * @author Kari Kammonen
 */
@Component("valintatapajonoValintatapajonoTyyppiConverter")
public class ValintatapajonoValintatapajonoTyyppiConverter implements Converter<Valintatapajono, ValintatapajonoTyyppi> {


    @Override
    public ValintatapajonoTyyppi convert(Valintatapajono valintatapajono) {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }
}
