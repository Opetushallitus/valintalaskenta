package fi.vm.sade.valintalaskenta.tulos.service;

/**
 * 
 * @author Jussi Jartamo
 * 
 *         Hakee valintatulokset REST-kutsuna valintalaskennalta
 */
public interface ExcelExportService {

    String exportValintalaskentatulos(String hakukohdeoid);

    String exportValintakoetulos(String hakukohdeOid, String valintakoeOid);

}
