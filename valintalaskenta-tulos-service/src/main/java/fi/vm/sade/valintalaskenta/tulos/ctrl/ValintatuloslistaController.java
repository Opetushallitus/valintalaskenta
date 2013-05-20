package fi.vm.sade.valintalaskenta.tulos.ctrl;

import static org.springframework.web.bind.annotation.RequestMethod.GET;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import fi.vm.sade.valintalaskenta.tulos.service.ExcelExportService;

/**
 * 
 * @author Jussi Jartamo
 * 
 *         Exporter for - valintakoetulos - valintalaskentatulos -
 *         sijoitteluntulos
 */
@Controller
public class ValintatuloslistaController {

    private static final Logger LOG = LoggerFactory.getLogger(ValintatuloslistaController.class);

    @Autowired
    ExcelExportService excelService;

    /**
     * @return LASKENNAN TULOKSET EXCEL
     */
    @RequestMapping(value = "valintalaskentatulos.xls", method = GET)
    public ResponseEntity<String> valintalaskentatulos(@RequestParam("hakukohdeOid") String hakukohdeOid) {
        assert (hakukohdeOid != null);
        LOG.debug("Excel hakukohteelle {}", hakukohdeOid);
        HttpHeaders headers = new HttpHeaders();
        headers.set("Content-Disposition", "attachment; filename*=UTF-8''valintalaskentatulos.xls;");
        return new ResponseEntity<String>(excelService.exportValintalaskentatulos(hakukohdeOid), headers, HttpStatus.OK);
    }

    /**
     * @return VALINTAKOE EXCEL
     */
    @RequestMapping(value = "valintakoetulos.xls", method = GET)
    public ResponseEntity<String> valintakoetulos(@RequestParam("hakukohdeOid") String hakukohdeOid) {
        assert (hakukohdeOid != null);
        LOG.debug("Excel hakukohteelle {}", hakukohdeOid);
        HttpHeaders headers = new HttpHeaders();
        headers.set("Content-Disposition", "attachment; filename*=UTF-8''valintakoetulos.xls;");
        return new ResponseEntity<String>(excelService.exportValintakoetulos(hakukohdeOid), headers, HttpStatus.OK);
    }

}
