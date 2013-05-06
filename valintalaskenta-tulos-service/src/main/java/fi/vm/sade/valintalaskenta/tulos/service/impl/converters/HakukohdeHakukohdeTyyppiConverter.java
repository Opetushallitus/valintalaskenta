package fi.vm.sade.valintalaskenta.tulos.service.impl.converters;

import fi.vm.sade.service.valintaperusteet.model.Funktioargumentti;
import fi.vm.sade.service.valintaperusteet.schema.FunktioargumenttiTyyppi;
import fi.vm.sade.service.valintatiedot.schema.HakukohdeTyyppi;
import fi.vm.sade.valintalaskenta.domain.Hakukohde;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

/**
 * @author Kari Kammonen
 */
@Component("hakukohdeHakukohdeTyyppiConverter")
public class HakukohdeHakukohdeTyyppiConverter implements Converter<Hakukohde, HakukohdeTyyppi> {


    @Override
    public HakukohdeTyyppi convert(Hakukohde hakukohde) {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }
}
