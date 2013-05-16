package fi.vm.sade.valintalaskenta.tulos.service.impl;

import java.util.ArrayList;
import java.util.List;

import fi.vm.sade.valintalaskenta.domain.Jonosija;
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

        for(Valinnanvaihe valinnanvaihe : valinnanvaiheet) {
            for(Valintatapajono jono : valinnanvaihe.getValintatapajono()){

                builder.append("<tr></tr><tr><td>");
                builder.append(jono.getNimi());
                builder.append("</td><td></td><td></td><td></td></tr>");

                ValintatapajonoHelper.sortJonosijat(jono.getJonosijat());
                for(Jonosija jonosija : jono.getJonosijat()) {
                    builder.append("<tr>");
                    builder.append("<td>").append(jonosija.getJonosija()).append("</td>");
                    builder.append("<td>").append(jonosija.getEtunimi()).append("</td>");
                    builder.append("<td>").append(jonosija.getSukunimi()).append("</td>");
                    builder.append("<td>").append(jonosija.getTuloksenTila()).append("</td>");
                    builder.append("</tr>");
                }
            }
        }
        builder.append("</table>");
        return builder.toString();
    }
}
