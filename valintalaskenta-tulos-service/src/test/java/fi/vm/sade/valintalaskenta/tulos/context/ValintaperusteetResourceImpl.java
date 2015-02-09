package fi.vm.sade.valintalaskenta.tulos.context;

import fi.vm.sade.service.valintaperusteet.dto.HakukohdeImportDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetHakijaryhmaDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintatapajonoDTO;
import fi.vm.sade.service.valintaperusteet.resource.ValintaperusteetResource;

import javax.ws.rs.core.Response;
import java.util.ArrayList;
import java.util.List;

/**
 * Created by teemu on 18.12.2014.
 */
public class ValintaperusteetResourceImpl implements ValintaperusteetResource {

    @Override
    public List<ValintatapajonoDTO> haeValintatapajonotSijoittelulle(String hakukohdeOid) {
        return new ArrayList<>();
    }

    @Override
    public List<ValintatapajonoDTO> haeValintatapajonotSijoittelulle(List<String> hakukohdeOids) {
        return new ArrayList<>();
    }

    @Override
    public List<ValintaperusteetDTO> haeValintaperusteet(String hakukohdeOid, Integer valinnanVaiheJarjestysluku) {
        return new ArrayList<>();
    }

    @Override
    public List<ValintaperusteetHakijaryhmaDTO> haeHakijaryhmat(String hakukohdeOid) {
        return new ArrayList<>();
    }

    @Override
    public Response tuoHakukohde(HakukohdeImportDTO hakukohde) {
        return null;
    }

    @Override
    public Boolean readAutomaattinenSijoitteluunSiirto(String oid) {
        return false;
    }

    @Override
    public Boolean updateAutomaattinenSijoitteluunSiirto(String oid, Boolean arvo) {
        return false;
    }
}
