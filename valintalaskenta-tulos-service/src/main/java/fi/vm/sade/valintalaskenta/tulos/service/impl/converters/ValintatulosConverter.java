package fi.vm.sade.valintalaskenta.tulos.service.impl.converters;

import fi.vm.sade.valintalaskenta.domain.Hakukohde;
import fi.vm.sade.valintalaskenta.domain.Jonosija;
import fi.vm.sade.valintalaskenta.domain.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.Valintatapajono;
import fi.vm.sade.valintalaskenta.domain.dto.HakukohdeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.JonosijaDTO;
import fi.vm.sade.valintalaskenta.domain.dto.ValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.dto.ValintatapajonoDTO;

import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: kkammone
 * Date: 7.8.2013
 * Time: 10:34
 * To change this template use File | Settings | File Templates.
 */
public interface ValintatulosConverter {


    void sort(List<JonosijaDTO> list);

    List<JonosijaDTO> convertJonosija(List<Jonosija> jonosijat);

    List<ValintatapajonoDTO> convertValintatapajono(List<Valintatapajono> valintapajonoList);

    List<HakukohdeDTO> convertHakukohde(List<Hakukohde> a);

    List<ValinnanvaiheDTO> convertValinnanvaiheList(List<Valinnanvaihe> valinnanVaiheList);
}