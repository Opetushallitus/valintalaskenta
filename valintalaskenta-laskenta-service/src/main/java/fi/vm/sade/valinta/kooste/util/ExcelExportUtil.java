package fi.vm.sade.valinta.kooste.util;

import static org.apache.poi.ss.usermodel.HorizontalAlignment.CENTER;

import fi.vm.sade.javautils.poi.OphCellStyles.OphXssfCellStyles;
import fi.vm.sade.valinta.kooste.util.excel.Highlight;
import fi.vm.sade.valinta.kooste.util.excel.Span;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Date;
import java.util.Map;
import java.util.Map.Entry;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.time.FastDateFormat;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.xssf.usermodel.XSSFCell;
import org.apache.poi.xssf.usermodel.XSSFRow;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** Muuntaa Object[][]:n xls-tiedostoksi! */
public class ExcelExportUtil {
  private static final Logger LOG = LoggerFactory.getLogger(ExcelExportUtil.class);

  public static final FastDateFormat DATE_FORMAT = FastDateFormat.getInstance("dd.MM.yyyy hh.mm");

  public static byte[] exportGridSheetsAsXlsBytes(Map<String, Object[][]> grids) {
    assert (grids != null);
    XSSFWorkbook wb = new XSSFWorkbook();
    OphXssfCellStyles alignCenterStyles = new OphXssfCellStyles(wb);
    alignCenterStyles.visit(alignCenterStyle -> alignCenterStyle.setAlignment(CENTER));
    OphXssfCellStyles highlights = new OphXssfCellStyles(wb);
    OphXssfCellStyles spanhighlights = new OphXssfCellStyles(wb);
    spanhighlights.visit(spanhighlight -> spanhighlight.setAlignment(CENTER));
    for (Entry<String, Object[][]> sheetAndGrid : grids.entrySet()) {
      XSSFSheet sheet = wb.createSheet(sheetAndGrid.getKey());
      exportGridToSheet(
          sheetAndGrid.getValue(), sheet, alignCenterStyles, spanhighlights, highlights);
    }

    ByteArrayOutputStream bytesOut = new ByteArrayOutputStream();
    try {
      wb.write(bytesOut);
    } catch (IOException e) {
      LOG.error("XLS-bytes generointi epäonnistui, ei pitäisi tapahtua, koska ei oikea I/O?", e);
    }
    return bytesOut.toByteArray();
  }

  private static void exportGridToSheet(
      Object[][] grid,
      XSSFSheet sheet,
      OphXssfCellStyles spanStyles,
      OphXssfCellStyles highlightSpanStyles,
      OphXssfCellStyles highlightStyles) {
    int numberOfcolumns = 0;
    // Create rows!
    short rowIndex = 0;
    for (Object[] dataRow : grid) {
      assert (dataRow != null);
      XSSFRow excelRow = sheet.createRow(rowIndex);
      // Create columns!
      short cellIndex = 0;
      for (Object dataCell : dataRow) {
        if (dataCell == null) {
          dataCell = StringUtils.EMPTY;
        }
        if (dataCell instanceof Span) {
          // Span over multiple columns
          Span span = (Span) dataCell;
          XSSFCell excelCell = excelRow.createCell(cellIndex);
          excelCell.setCellValue(span.getText());
          if (span.isAlsoHighlight()) {
            highlightSpanStyles.apply(excelCell);
          } else {
            spanStyles.apply(excelCell);
          }
          sheet.addMergedRegion(
              new CellRangeAddress(
                  rowIndex, // first
                  // row
                  // (0-based)
                  rowIndex, // last row (0-based)
                  cellIndex, // first column (0-based)
                  cellIndex + span.getSpanColumns() - 1 // last column
                  // (0-based)
                  ));
          cellIndex += span.getSpanColumns();
        } else {
          // Normal cell
          numberOfcolumns = Math.max(numberOfcolumns, cellIndex);
          XSSFCell excelCell = excelRow.createCell(cellIndex);
          String value = dataCell.toString();
          excelCell.setCellValue(value);
          if (dataCell instanceof Highlight) {
            highlightStyles.apply(excelCell);
          }
          ++cellIndex;
        }
      }
      ++rowIndex;
    }
    // Auto size used columns!
    for (int column = 0; column <= numberOfcolumns; ++column) {
      sheet.autoSizeColumn(column);
    }
  }

  private static byte[] exportGridAsXlsBytes(Object[][] grid) {
    assert (grid != null);
    XSSFWorkbook wb = new XSSFWorkbook();
    OphXssfCellStyles alignCenterStyles = new OphXssfCellStyles(wb);
    alignCenterStyles.visit(alignCenterStyle -> alignCenterStyle.setAlignment(CENTER));
    OphXssfCellStyles highlights = new OphXssfCellStyles(wb);
    OphXssfCellStyles spanhighlights = new OphXssfCellStyles(wb);
    spanhighlights.visit(spanhighlight -> spanhighlight.setAlignment(CENTER));
    XSSFSheet sheet = wb.createSheet(DATE_FORMAT.format(new Date()));
    exportGridToSheet(grid, sheet, alignCenterStyles, spanhighlights, highlights);

    ByteArrayOutputStream bytesOut = new ByteArrayOutputStream();
    try {
      wb.write(bytesOut);
    } catch (IOException e) {
      LOG.error("XLS-bytes generointi epäonnistui, ei pitäisi tapahtua, koska ei oikea I/O?", e);
    }
    return bytesOut.toByteArray();
  }

  public static InputStream exportGridAsXls(Object[][] grid) {
    return new ByteArrayInputStream(exportGridAsXlsBytes(grid)); // bytesOut.newInputStream();
  }
}
