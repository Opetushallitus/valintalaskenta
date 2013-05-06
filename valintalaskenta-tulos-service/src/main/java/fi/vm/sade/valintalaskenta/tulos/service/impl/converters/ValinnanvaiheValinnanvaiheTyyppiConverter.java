package fi.vm.sade.valintalaskenta.tulos.service.impl.converters;

import fi.vm.sade.service.valintatiedot.schema.HakukohdeTyyppi;
import fi.vm.sade.service.valintatiedot.schema.ValinnanvaiheTyyppi;
import fi.vm.sade.valintalaskenta.domain.Hakukohde;
import fi.vm.sade.valintalaskenta.domain.Valinnanvaihe;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

/**
 * @author Kari Kammonen
 */
@Component("valinnanvaiheValinnanvaiheTyyppiConverter")
public class ValinnanvaiheValinnanvaiheTyyppiConverter implements Converter<Valinnanvaihe, ValinnanvaiheTyyppi> {


    @Override
    public ValinnanvaiheTyyppi convert(Valinnanvaihe valinnanvaihe) {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }
}
