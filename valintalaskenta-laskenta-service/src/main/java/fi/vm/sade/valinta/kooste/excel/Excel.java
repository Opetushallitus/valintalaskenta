package fi.vm.sade.valinta.kooste.excel;

import static org.apache.poi.ss.usermodel.CellType.NUMERIC;
import static org.apache.poi.ss.usermodel.CellType.STRING;
import static org.apache.poi.ss.usermodel.FillPatternType.SOLID_FOREGROUND;
import static org.apache.poi.ss.usermodel.HorizontalAlignment.CENTER;
import static org.apache.poi.ss.usermodel.HorizontalAlignment.LEFT;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import fi.vm.sade.javautils.poi.OphCellStyles.OphXssfCellStyles;
import java.awt.*;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import org.apache.poi.POIXMLException;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.xssf.usermodel.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Excel {
  private static final Logger LOG = LoggerFactory.getLogger(Excel.class);
  public static final int VAKIO_LEVEYS = 8500;
  private final List<Rivi> rivit;
  private final String nimi;
  private final int[] sarakkeetPysty;
  private final int[] sarakkeetVaaka;

  public Excel(String nimi, List<Rivi> rivit) {
    this.rivit = rivit;
    this.nimi = nimi;
    this.sarakkeetPysty = new int[] {};
    this.sarakkeetVaaka = new int[] {};
  }

  public Excel(String nimi, List<Rivi> rivit, int[] sarakkeetPysty, int[] sarakkeetVaaka) {
    this.rivit = rivit;
    this.nimi = nimi;
    this.sarakkeetPysty = sarakkeetPysty;
    this.sarakkeetVaaka = sarakkeetVaaka;
  }

  public List<Rivi> getRivit() {
    return rivit;
  }

  public String getNimi() {
    return nimi;
  }

  public void tuoXlsx(InputStream input) throws IOException, ExcelValidointiPoikkeus {
    XSSFWorkbook workbook;
    try {
      workbook = new XSSFWorkbook(input);
    } catch (POIXMLException e) {
      throw new RuntimeException(
          "Excelin lukemisessa tapahtui poikkeus ("
              + e.getMessage()
              + "). Onhan Excel Workbook -muodossa (.xlsx)?");
    }
    XSSFSheet sheet = workbook.getSheetAt(workbook.getActiveSheetIndex());
    int lastRowIndex = sheet.getLastRowNum();
    Iterator<Rivi> riviIterator = rivit.iterator();
    if (riviIterator.hasNext()) {
      Rivi riviImportteri = riviIterator.next();
      for (int i = 0; i <= lastRowIndex; ++i) {
        XSSFRow row = sheet.getRow(i);
        // LOG.error("rivi [{}]", i);
        Rivi rivi;
        if (row == null) {
          rivi = Rivi.tyhjaRivi();
        } else {
          rivi = XSSFRivi.asRivi(row);
        }
        // LOG.error("{}", rivi);
        // ottaako importteri viela vastaan dataa?
        if (!riviImportteri.validoi(rivi)) {
          if (!riviIterator.hasNext()) { // onko seuraavaa
            // importteria?
            return; // ei ole joten importointi on valmis
          } else {
            riviImportteri = riviIterator.next(); // Hidden
            // seuraava
            // importteri
          }
        }
      }
    }
  }

  public InputStream vieXlsx() {
    XSSFWorkbook workbook = new XSSFWorkbook();
    XSSFSheet sheet = workbook.createSheet(nimi);
    XSSFDataValidationHelper dvHelper = new XSSFDataValidationHelper(sheet);
    int hiddenSheetCount = 0;
    XSSFDataFormat fmt = workbook.createDataFormat();
    OphXssfCellStyles defaultStyles = new OphXssfCellStyles(workbook);
    defaultStyles.visit(
        s -> {
          s.setDataFormat(fmt.getFormat("@"));
          s.setAlignment(LEFT);
        });
    for (int i = 0; i < 22; ++i) {
      sheet.setDefaultColumnStyle(i, defaultStyles.getUnsafeStyle());
    }
    OphXssfCellStyles hiddenStyles = new OphXssfCellStyles(workbook);
    hiddenStyles.visit(s -> s.setHidden(true));

    OphXssfCellStyles alignRightStyles = new OphXssfCellStyles(workbook);
    alignRightStyles.visit(alignRightStyle -> alignRightStyle.setDataFormat(fmt.getFormat("@")));
    OphXssfCellStyles alignCenterStyles = new OphXssfCellStyles(workbook);
    alignCenterStyles.visit(
        alignCenterStyle -> {
          alignCenterStyle.setDataFormat(fmt.getFormat("@"));
          alignCenterStyle.setAlignment(CENTER);
        });
    OphXssfCellStyles lockedStyles = new OphXssfCellStyles(workbook);
    lockedStyles.visit(
        lockedStyle -> {
          lockedStyle.setFillForegroundColor(new XSSFColor(Color.GRAY));
          lockedStyle.setFillPattern(SOLID_FOREGROUND);
          lockedStyle.setDataFormat(fmt.getFormat("@"));
        });
    OphXssfCellStyles editableStyles = new OphXssfCellStyles(workbook);
    editableStyles.visit(
        editableStyle -> {
          editableStyle.setDataFormat(fmt.getFormat("@"));
          editableStyle.setFillForegroundColor(new XSSFColor(new Color(255, 204, 153)));
          editableStyle.setFillPattern(SOLID_FOREGROUND);
          editableStyle.setAlignment(LEFT);
          editableStyle.setLocked(false);
        });
    List<Integer> leveysPreferenssit = Lists.newArrayList();
    int rowIndex = 0;
    int maxCellNum = 0;
    Map<Collection<String>, MonivalintaJoukko> constraintSets = Maps.newHashMap();
    Map<Collection<Number>, ArvovaliJoukko> numberConstraintSets = Maps.newHashMap();

    for (Rivi toisteinenrivi : rivit) {
      for (Rivi rivi : toisteinenrivi.getToisteisetRivit()) {
        XSSFRow row = sheet.createRow(rowIndex);
        int cellNum = 0;
        for (Solu solu : rivi.getSolut()) {
          XSSFCell cell = null;
          if (solu.isTeksti()) {
            cell = row.createCell(cellNum, STRING);
            cell.setCellValue(solu.toTeksti().getTeksti());
            defaultStyles.apply(cell);
          } else if (solu.isNumero()) {
            cell = row.createCell(cellNum, NUMERIC);
            Numero numero = solu.toNumero();
            if (numero.hasArvovali()) {
              ArvovaliJoukko joukko;
              Collection<Number> numberSet = numero.asArvovali();
              if (!numberConstraintSets.containsKey(numberSet)) {
                numberConstraintSets.put(
                    numberSet,
                    joukko = new ArvovaliJoukko(numero.getMin(), numero.getMax(), sheet, dvHelper));
              } else {
                joukko = numberConstraintSets.get(numberSet);
              }
              joukko.addAddress(rowIndex, cellNum);
            }
            if (!numero.isTyhja()) {
              cell.setCellValue(numero.getNumero().doubleValue());
              alignRightStyles.apply(cell);
            }

          } else if (solu.isMonivalinta()) {
            cell = row.createCell(cellNum, STRING);
            cell.setCellValue(solu.toTeksti().getTeksti());
            defaultStyles.apply(cell);
            Monivalinta monivalinta = solu.toMonivalinta();
            MonivalintaJoukko joukko;

            // Pilkulla erotetun merkkijonon maksimipituus excel-solussa 255 (OY-190).
            int excelSolunMaxPituus = 250;

            if (monivalinta.getVaihtoehdot().toString().length() >= excelSolunMaxPituus
                && !constraintSets.containsKey(monivalinta.getVaihtoehdot())) {
              XSSFSheet hiddenSheet;
              String sheetName = String.valueOf(cellNum);
              try {
                hiddenSheet = workbook.createSheet(sheetName);
                int i = 0;
                for (String vaihtoehto : monivalinta.getVaihtoehdot()) {
                  XSSFRow hiddenRow = hiddenSheet.createRow(i);
                  XSSFCell hiddenCell = hiddenRow.createCell(0);
                  hiddenCell.setCellValue(vaihtoehto);
                  i++;
                }
                hiddenSheetCount++;
              } catch (IllegalArgumentException e) {
                // Should never happen since we have already created the hidden sheet for this
                // column.
              }
              workbook.setSheetHidden(hiddenSheetCount, true);
              constraintSets.put(
                  monivalinta.getVaihtoehdot(),
                  joukko =
                      new MonivalintaJoukko(
                          monivalinta.getVaihtoehdot(),
                          sheet,
                          dvHelper,
                          sheetName + "!$A$1:$A$" + monivalinta.getVaihtoehdot().size()));
            } else if (!constraintSets.containsKey(monivalinta.getVaihtoehdot())) {
              constraintSets.put(
                  monivalinta.getVaihtoehdot(),
                  joukko = new MonivalintaJoukko(monivalinta.getVaihtoehdot(), sheet, dvHelper));
            } else {
              joukko = constraintSets.get(monivalinta.getVaihtoehdot());
            }
            joukko.addAddress(rowIndex, cellNum);
          }
          if (cell != null) {
            if (solu.isKeskitettyTasaus()) {
              alignCenterStyles.apply(cell);
            } else if (solu.isTasausOikealle()) {
              alignRightStyles.apply(cell);
            }
          }
          if (cell != null && solu.isLukittu()) {
            lockedStyles.apply(cell);
          } else if (cell != null && solu.isMuokattava()) {
            editableStyles.apply(cell);
          }
          asetaPreferenssi(cellNum, solu.preferoituLeveys(), leveysPreferenssit);
          if (solu.ulottuvuus() != 1) {
            sheet.addMergedRegion(
                new CellRangeAddress(
                    rowIndex, // first
                    // row
                    // (0-based)
                    rowIndex, // last row (0-based)
                    cellNum, // first column (0-based)
                    cellNum + solu.ulottuvuus() - 1 // last column
                    // (0-based)
                    ));
            cellNum += solu.ulottuvuus();
          } else {
            ++cellNum;
          }
        }
        maxCellNum = Math.max(maxCellNum, cellNum);
        ++rowIndex;
      }
    }

    for (int sarake : sarakkeetPysty) {
      try {
        sheet.setColumnHidden(sarake, true);
      } catch (Exception e) {

      }
    }
    for (int sarake : sarakkeetVaaka) {
      try {
        XSSFRow r = sheet.getRow(sarake);
        if (r != null) {
          r.setZeroHeight(true);
          hiddenStyles.apply(r);
        }
      } catch (Exception e) {
        LOG.error("Excel throws", e);
      }
    }
    for (int i = 0; i < leveysPreferenssit.size(); ++i) {
      int preferenssi = leveysPreferenssit.get(i);
      if (preferenssi == 0) {
        preferenssi = Excel.VAKIO_LEVEYS;
      }
      sheet.autoSizeColumn(i);
      if (sheet.getColumnWidth(i) < preferenssi) {
        sheet.setColumnWidth(i, preferenssi);
      }
    }
    return export(workbook);
  }

  private void asetaPreferenssi(int column, int preferenssi, List<Integer> leveysPreferenssit) {

    while (leveysPreferenssit.size() <= column) {
      leveysPreferenssit.add(0);
    }
    leveysPreferenssit.set(column, preferenssi);
  }

  public static InputStream export(final XSSFWorkbook workbook) {
    ByteArrayOutputStream b;
    try {
      workbook.write(b = new ByteArrayOutputStream());
      return new ByteArrayInputStream(b.toByteArray());
    } catch (Exception e) {
      // tämä ei koskaan tapahdu I/O virheestä johtuen mutta Apache
      // Poi:ssa voi olla bugeja joten hyvä heittää eteenpäin ettei jää
      // huomaamatta
      throw new RuntimeException(e);
    }
  }
}
