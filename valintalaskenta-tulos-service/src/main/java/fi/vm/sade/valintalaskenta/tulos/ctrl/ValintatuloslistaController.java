package fi.vm.sade.valintalaskenta.tulos.ctrl;

import static org.springframework.web.bind.annotation.RequestMethod.GET;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;

import fi.vm.sade.valintalaskenta.tulos.service.ExcelExportService;

/**
 * 
 * @author Jussi Jartamo
 * 
 *         Exporter for valintatuloslista
 */
@Controller
public class ValintatuloslistaController {

    @Autowired
    ExcelExportService excelService;

    /**
     * @return Excel Jiran formaatissa
     */
    @RequestMapping(value = "valintatuloslista.xls", method = GET)
    public ResponseEntity<String> valintatuloslista() {

        HttpHeaders headers = new HttpHeaders();
        headers.set("Content-Disposition", "attachment; filename*=UTF-8''valintatuloslista.xls;");

        // tulokset.clear();
        // tulokset.addAll(valintatuloslistaService.haeValintatuloslista());
        return new ResponseEntity<String>(excelService.export(), headers, HttpStatus.OK);
    }

}
