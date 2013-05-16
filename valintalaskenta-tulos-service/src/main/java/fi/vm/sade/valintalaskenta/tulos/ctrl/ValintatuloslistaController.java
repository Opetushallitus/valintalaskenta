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
 *         Exporter for valintatuloslista
 */
@Controller
public class ValintatuloslistaController {

    private static final Logger LOG = LoggerFactory.getLogger(ValintatuloslistaController.class);

    @Autowired
    ExcelExportService excelService;

    /**
     * @return Excel Jiran formaatissa
     */
    @RequestMapping(value = "valintatuloslista.xls", method = GET)
    public ResponseEntity<String> valintatuloslista(@RequestParam("hakukohdeOid") String hakukohdeOid) {
        assert (hakukohdeOid != null);
        LOG.debug("Excel hakukohteelle {}", hakukohdeOid);
        HttpHeaders headers = new HttpHeaders();
        headers.set("Content-Disposition", "attachment; filename*=UTF-8''valintatuloslista.xls;");
        return new ResponseEntity<String>(excelService.export(hakukohdeOid), headers, HttpStatus.OK);
    }

}
