package fi.vm.sade.valintalaskenta.tulos.context;

import fi.vm.sade.service.valintaperusteet.dto.HakukohdeImportDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetHakijaryhmaDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintatapajonoDTO;
import fi.vm.sade.service.valintaperusteet.resource.ValintaperusteetResourceV2;
import io.swagger.annotations.ApiParam;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ValintaperusteetResourceV2Impl implements ValintaperusteetResourceV2 {

    @Override
    public List<ValintatapajonoDTO> haeValintatapajonotSijoittelulle(String hakukohdeOid) {
        return new ArrayList<>();
    }

    @Override
    public Map<String, List<ValintatapajonoDTO>> haeValintatapajonotSijoittelulle(List<String> hakukohdeOids) {
        return new HashMap<>();
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
    @Path("/{valintatapajonoOid}/automaattinenSiirto")
    public ValintatapajonoDTO updateAutomaattinenSijoitteluunSiirto(@ApiParam(value = "Valintatapajonon OID", required = true) @PathParam("valintatapajonoOid") String valintatapajonoOid,
                                                                    @ApiParam(value = "Sijoittelustatus", required = true) @QueryParam("status") boolean status,
                                                                    @Context HttpServletRequest request) {
        return new ValintatapajonoDTO();
    }
}
