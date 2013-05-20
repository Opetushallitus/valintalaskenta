package fi.vm.sade.valintalaskenta.tulos.service.impl;

import java.text.SimpleDateFormat;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import fi.vm.sade.valintalaskenta.domain.Jonosija;
import fi.vm.sade.valintalaskenta.domain.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.Valintatapajono;
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
                builder.append("</td><td></td><td></td><td></td></tr>");

                for (Jonosija jonosija : jono.getJonosijat()) {

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

    // Hakemus OID Valintakoe OID Valintakoetunniste Osallistuminen Laskettu pvm
    // <td>{{koe.hakemusOid}}</td>
    // <td>{{koe.valintakoeOid}}</td>
    // <td>{{koe.valintakoeTunniste}}</td>
    // <td>{{koe.osallistuminen}}</td>
    // <td><span
    // ng-bind="koe.createdAt | date:'dd.MM.yyyy HH:mm:ss'">-</span></td>
    @Override
    public String exportValintakoetulos(String hakukohdeOid) {
        SimpleDateFormat formatter = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");
        StringBuilder builder = new StringBuilder();
        builder.append("<table>");

        List<ValintakoeOsallistuminen> valinnanvaiheet = tulosService
                .haeValintakoeOsallistumisetByHakutoive(hakukohdeOid);

        for (ValintakoeOsallistuminen koetulos : valinnanvaiheet) {
            for (Hakutoive hakutoive : koetulos.getHakutoiveet()) {
                for (ValinnanVaihe vaihe : hakutoive.getValinnanVaiheet()) {
                    for (Valintakoe valintakoe : vaihe.getValintakokeet()) {
                        // String hakuOid = koetulos.getHakuOid();
                        String hakemusOid = koetulos.getHakemusOid();
                        // String hakijaOid = koetulos.getHakijaOid();
                        String createdAt = formatter.format(koetulos.getCreatedAt());
                        String valintakoeOid = valintakoe.getValintakoeOid();
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
        builder.append("</table>");
        return builder.toString();
    }

}
