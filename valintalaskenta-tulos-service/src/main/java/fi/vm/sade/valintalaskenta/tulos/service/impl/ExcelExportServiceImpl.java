package fi.vm.sade.valintalaskenta.tulos.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.springframework.stereotype.Service;

import fi.vm.sade.valintalaskenta.tulos.dto.ValintatulosDTO;
import fi.vm.sade.valintalaskenta.tulos.service.ExcelExportService;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Service
public class ExcelExportServiceImpl implements ExcelExportService {

    @Override
    public String export() {
        List<ValintatulosDTO> tulokset = new ArrayList<ValintatulosDTO>();
        StringBuilder builder = new StringBuilder();
        builder.append("<table>");
        for (ValintatulosDTO tulos : tulokset) {
            builder.append("<tr>");
            builder.append("<td>").append(tulos.getNimi()).append("</td>");
            builder.append("<td>").append(tulos.getOsoite()).append("</td>");
            builder.append("</tr>");
        }
        builder.append("</table>");
        return builder.toString();
    }
}
