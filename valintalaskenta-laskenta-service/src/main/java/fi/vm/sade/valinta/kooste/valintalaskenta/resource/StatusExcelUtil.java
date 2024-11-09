package fi.vm.sade.valinta.kooste.valintalaskenta.resource;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import fi.vm.sade.valinta.kooste.util.excel.ExcelExportUtil;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import fi.vm.sade.valintalaskenta.domain.dto.seuranta.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

public class StatusExcelUtil {
  private static final Logger LOG =
      LoggerFactory.getLogger(StatusExcelUtil.class);

  public static ResponseEntity<byte[]> createTimeoutErrorXls(final String uuid) {
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

  public static ResponseEntity<byte[]> getStatusXls(LaskentaDto laskenta) {
    try {
        byte[] bytes = laskentaDtoAsExcel(laskenta);
        return excelResponse("yhteenveto.xls", bytes);
    } catch(Throwable e) {
      LOG.error(
          "Excelin muodostuksessa(kohteelle /laskenta/" + laskenta.getUuid() + ") tapahtui virhe", e);
      return luoVirheExcelVastaus("yhteenveto_virhe.xls", "Virhe Excelin muodostuksessa!", e);
    }
  }

  private static ResponseEntity<byte[]> luoVirheExcelVastaus(
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

  private static ResponseEntity<byte[]> excelResponse(final String tiedostonnimi, byte[] bytes) {
    return ResponseEntity.status(HttpStatus.OK)
        .header("Content-Length", bytes.length + "")
        .header("Content-Type", "application/vnd.ms-excel")
        .header("Content-Disposition", "attachment; filename=\"" + tiedostonnimi + "\"")
        .body(bytes);
  }

  private static byte[] getExcelSheetAndGridBytes(final String sheetName, final List<Object[]> grid) {
    final Map<String, Object[][]> sheetAndGrid = Maps.newHashMap();
    sheetAndGrid.put(sheetName, grid.toArray(new Object[][] {}));

    return ExcelExportUtil.exportGridSheetsAsXlsBytes(sheetAndGrid);
  }

  private static byte[] laskentaDtoAsExcel(LaskentaDto laskenta) {
    Map<String, Object[][]> sheetAndGrid = Maps.newHashMap();
    {
      List<Object[]> grid = Lists.newArrayList();
      if (laskenta.getIlmoitus() != null) {
        IlmoitusDto ilmoitus = laskenta.getIlmoitus();
        if (IlmoitusTyyppi.VIRHE.equals(ilmoitus.getTyyppi())) {
          grid.add(new Object[] {"Virheilmoitus:", ilmoitus.getOtsikko()});
        } else {
          grid.add(new Object[] {"Ilmoitus:", ilmoitus.getOtsikko()});
        }
      }
      grid.add(new Object[] {""});
      grid.add(new Object[] {"Suorittamattomat hakukohteet"});
      if (laskenta.getHakukohteet() != null) {
        for (HakukohdeDto hakukohde :
            laskenta.getHakukohteet().stream()
                .filter(h -> !HakukohdeTila.VALMIS.equals(h.getTila()))
                .collect(Collectors.toList())) {
          List<String> rivi = Lists.newArrayList();
          rivi.add(hakukohde.getHakukohdeOid());
          if (hakukohde.getIlmoitukset() != null) {
            rivi.addAll(
                hakukohde.getIlmoitukset().stream()
                    .map(i -> i.getOtsikko())
                    .collect(Collectors.toList()));
          }
          grid.add(rivi.toArray());
        }
      }
      sheetAndGrid.put("Kesken", grid.toArray(new Object[][] {}));
    }
    {
      List<Object[]> grid = Lists.newArrayList();
      grid.add(new Object[] {"Valmistuneet hakukohteet"});
      if (laskenta.getHakukohteet() != null) {
        for (HakukohdeDto hakukohde :
            laskenta.getHakukohteet().stream()
                .filter(h -> HakukohdeTila.VALMIS.equals(h.getTila()))
                .collect(Collectors.toList())) {
          grid.add(new Object[] {hakukohde.getHakukohdeOid()});
        }
      }
      sheetAndGrid.put("Valmiit", grid.toArray(new Object[][] {}));
    }
    return ExcelExportUtil.exportGridSheetsAsXlsBytes(sheetAndGrid);
  }
}
