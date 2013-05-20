package fi.vm.sade.valintalaskenta.tulos.ctrl;

import static org.springframework.web.bind.annotation.RequestMethod.GET;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import fi.vm.sade.valintalaskenta.tulos.service.ExcelExportService;
import org.springframework.web.bind.annotation.ResponseBody;

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
    @ResponseBody
    @RequestMapping(value = "valintalaskentatulos.xls", method = GET, produces = "application/vnd.ms-excel" )
    public String valintalaskentatulos(@RequestParam("hakukohdeOid") String hakukohdeOid) {
        assert (hakukohdeOid != null);
        LOG.debug("Excel hakukohteelle {}", hakukohdeOid);
        return excelService.exportValintalaskentatulos(hakukohdeOid);

    }

    /**
     * @return VALINTAKOE EXCEL
     */
    @ResponseBody
    @RequestMapping(value = "valintakoetulos.xls", method = GET, produces = "application/vnd.ms-excel" )
    public String valintakoetulos(@RequestParam("hakukohdeOid") String hakukohdeOid) {
        assert (hakukohdeOid != null);
        LOG.debug("Excel hakukohteelle {}", hakukohdeOid);
        return excelService.exportValintakoetulos(hakukohdeOid);
    }

}
