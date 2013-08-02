package fi.vm.sade.valintalaskenta.tulos.service.impl;

import java.text.SimpleDateFormat;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import fi.vm.sade.valintalaskenta.domain.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.Valintatapajono;
import fi.vm.sade.valintalaskenta.domain.converter.JonosijaToJonosijaDTOConverter;
import fi.vm.sade.valintalaskenta.domain.dto.JonosijaDTO;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Hakutoive;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValinnanVaihe;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Valintakoe;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
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
    public String exportValintalaskentatulos(String hakukohdeoid) {
        StringBuilder builder = new StringBuilder();
        builder.append("<table>");

        List<Valinnanvaihe> valinnanvaiheet = tulosService.haeValinnanvaiheetHakukohteelle(hakukohdeoid);

        for (Valinnanvaihe valinnanvaihe : valinnanvaiheet) {
            for (Valintatapajono jono : valinnanvaihe.getValintatapajono()) {

                builder.append("<tr></tr><tr><td>");
                builder.append(jono.getNimi());
                builder.append("</td> <td></td> <td></td> <td></td> <td></td> <td></td> <td></td> </tr>\n");

                JonosijaToJonosijaDTOConverter a = new JonosijaToJonosijaDTOConverter();

                for (JonosijaDTO jonosija : a.jarjestaJaLisaaJonosijaNumero(jono.getJonosijat())) {

                    builder.append("<tr>");
                    builder.append("<td>").append(jonosija.getJonosija()).append("</td>");
                    builder.append("<td>").append(jonosija.getEtunimi()).append("</td>");
                    builder.append("<td>").append(jonosija.getSukunimi()).append("</td>");
                    builder.append("<td>").append(jonosija.getHakemusOid()).append("</td>");
                    builder.append("<td>").append(jonosija.getTuloksenTila()).append("</td>");
                    builder.append("<td>").append(jonosija.getJarjestyskriteerit().firstEntry().getValue().getArvo())
                            .append("</td>");
                    builder.append("<td>").append(jonosija.isHarkinnanvarainen()).append("</td>");

                    builder.append("</tr>\n");
                }
            }
        }
        builder.append("</table>");
        return builder.toString();
    }

    @Override
    public String exportValintakoetulos(String hakukohdeOid, String valintakoeOid) {
        SimpleDateFormat formatter = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");
        StringBuilder builder = new StringBuilder();
        builder.append("<table>");

        List<ValintakoeOsallistuminen> valinnanvaiheet = tulosService
                .haeValintakoeOsallistumisetByHakutoive(hakukohdeOid);

        for (ValintakoeOsallistuminen koetulos : valinnanvaiheet) {
            for (Hakutoive hakutoive : koetulos.getHakutoiveet()) {
                for (ValinnanVaihe vaihe : hakutoive.getValinnanVaiheet()) {
                    for (Valintakoe valintakoe : vaihe.getValintakokeet()) {
                        if (valintakoeOid.equals(valintakoe.getValintakoeOid())) {
                            // String hakuOid = koetulos.getHakuOid();
                            String hakemusOid = koetulos.getHakemusOid();
                            // String hakijaOid = koetulos.getHakijaOid();
                            String createdAt = formatter.format(koetulos.getCreatedAt());
                            String valintakoeTunniste = valintakoe.getValintakoeTunniste();
                            String osallistuminen = valintakoe.getOsallistuminen().toString();
                            builder.append("<tr>");
                            builder.append("<td>").append(hakemusOid).append("</td>");
                            builder.append("<td>").append(valintakoeOid).append("</td>");
                            builder.append("<td>").append(valintakoeTunniste).append("</td>");
                            builder.append("<td>").append(osallistuminen).append("</td>");
                            builder.append("<td>").append(createdAt).append("</td>");
                            builder.append("</tr>");
                        }

                    }
                }
            }

        }
        builder.append("</table>");
        return builder.toString();
    }

}
