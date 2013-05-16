package fi.vm.sade.valintalaskenta.tulos.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.Valintatapajono;
import fi.vm.sade.valintalaskenta.tulos.dto.ValintatulosDTO;
import fi.vm.sade.valintalaskenta.tulos.service.ExcelExportService;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Service
public class ExcelExportServiceImpl implements ExcelExportService {

    @Autowired
    ValintalaskentaTulosService tulosService;

    @Override
    public String export(String hakukohdeoid) {

        List<ValintatulosDTO> tulokset = new ArrayList<ValintatulosDTO>();
        StringBuilder builder = new StringBuilder();
        builder.append("<table>");
        List<Valinnanvaihe> valinnanvaiheet = tulosService.haeValinnanvaiheetHakukohteelle(hakukohdeoid);
        if (!valinnanvaiheet.isEmpty()) {
            // viimeisen valinnanvaiheen...
            Valinnanvaihe vaihe = valinnanvaiheet.get(0);
            if (!vaihe.getValintatapajono().isEmpty()) {
                // viimeinen valintatapajono...
                Valintatapajono jono = vaihe.getValintatapajono().get(0);
                if (!jono.getJarjestyskriteeritulokset().isEmpty()) {
                    Jarjestyskriteeritulos tulos = jono.getJarjestyskriteeritulokset().get(0);
                    builder.append("<tr>");
                    builder.append("<td>").append(tulos.getEtunimi()).append("</td>");
                    builder.append("<td>").append(tulos.getSukunimi()).append("</td>");
                    builder.append("<td>").append(tulos.getTila()).append("</td>");
                    builder.append("</tr>");
                }
            }
        }
        builder.append("</table>");
        return builder.toString();
    }
}
