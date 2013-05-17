package fi.vm.sade.valintalaskenta.tulos.service;

/**
 * 
 * @author Jussi Jartamo
 * 
 *         Hakee valintatulokset REST-kutsuna valintalaskennalta
 */
public interface ExcelExportService {

    String exportTulokset(String hakukohdeoid);

    String exportKoeOsallistujat(String hakukohdeoid);

}
