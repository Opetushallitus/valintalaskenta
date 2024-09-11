package fi.vm.sade.valinta.kooste.valintalaskenta.resource;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import fi.vm.sade.valinta.kooste.external.resource.seuranta.LaskentaSeurantaAsyncResource;
import fi.vm.sade.valinta.kooste.util.ExcelExportUtil;
import fi.vm.sade.valinta.kooste.valintalaskenta.excel.LaskentaDtoAsExcel;
import java.util.List;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.context.request.async.DeferredResult;

@Service
public class ValintalaskentaStatusExcelHandler {
  private static final Logger LOG =
      LoggerFactory.getLogger(ValintalaskentaStatusExcelHandler.class);

  @Autowired private LaskentaSeurantaAsyncResource seurantaAsyncResource;

  public ResponseEntity<byte[]> createTimeoutErrorXls(final String uuid) {
    final List<Object[]> grid = Lists.newArrayList();
    grid.add(
        new Object[] {
          "Kysely seuranapalveluun (kohteelle /laksenta/"
              + uuid
              + ") aikakatkaistiin. Palvelu saattaa olla ylikuormittunut!"
        });

    final byte[] bytes = getExcelSheetAndGridBytes("Aikakatkaistu", grid);
    LOG.error("Aikakatkaisu Excelin luonnille (kohde /laskenta/{})", uuid);
    return excelResponse("yhteenveto_aikakatkaistu.xls", bytes);
  }

  public void getStatusXls(final String uuid, DeferredResult<ResponseEntity<byte[]>> result) {
    seurantaAsyncResource
        .laskenta(uuid)
        .subscribe(
            laskenta -> {
              try {
                byte[] bytes = LaskentaDtoAsExcel.laskentaDtoAsExcel(laskenta);
                result.setResult(excelResponse("yhteenveto.xls", bytes));
              } catch (Throwable e) {
                LOG.error(
                    "Excelin muodostuksessa(kohteelle /laskenta/" + uuid + ") tapahtui virhe", e);
                result.setResult(
                    luoVirheExcelVastaus(
                        "yhteenveto_virhe.xls", "Virhe Excelin muodostuksessa!", e));
                throw e;
              }
            },
            poikkeus -> {
              LOG.error(
                  "Excelin tietojen haussa seurantapalvelusta(/laskenta/"
                      + uuid
                      + ") tapahtui virhe",
                  poikkeus);
              result.setResult(
                  luoVirheExcelVastaus(
                      "yhteenveto_seurantavirhe.xls",
                      "Virhe seurantapavelun kutsumisessa!",
                      poikkeus));
            });
  }

  private ResponseEntity<byte[]> luoVirheExcelVastaus(
      final String tiedostonNimi, final String virheViesti, final Throwable poikkeus) {
    final List<Object[]> grid = Lists.newArrayList();
    grid.add(new Object[] {virheViesti});
    grid.add(new Object[] {poikkeus.getMessage()});

    for (StackTraceElement se : poikkeus.getStackTrace()) {
      grid.add(new Object[] {se});
    }

    final byte[] bytes = getExcelSheetAndGridBytes("Virhe", grid);
    return excelResponse(tiedostonNimi, bytes);
  }

  private ResponseEntity<byte[]> excelResponse(final String tiedostonnimi, byte[] bytes) {
    return ResponseEntity.status(HttpStatus.OK)
        .header("Content-Length", bytes.length + "")
        .header("Content-Type", "application/vnd.ms-excel")
        .header("Content-Disposition", "attachment; filename=\"" + tiedostonnimi + "\"")
        .body(bytes);
  }

  private byte[] getExcelSheetAndGridBytes(final String sheetName, final List<Object[]> grid) {
    final Map<String, Object[][]> sheetAndGrid = Maps.newHashMap();
    sheetAndGrid.put(sheetName, grid.toArray(new Object[][] {}));

    return ExcelExportUtil.exportGridSheetsAsXlsBytes(sheetAndGrid);
  }
}
