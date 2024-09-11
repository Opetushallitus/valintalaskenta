package fi.vm.sade.valinta.kooste.excel;

import java.util.Collection;
import org.apache.poi.xssf.usermodel.XSSFDataValidation;
import org.apache.poi.xssf.usermodel.XSSFDataValidationConstraint;
import org.apache.poi.xssf.usermodel.XSSFDataValidationHelper;
import org.apache.poi.xssf.usermodel.XSSFSheet;

public class MonivalintaJoukko {
  private final Collection<String> joukko;
  private final XSSFDataValidationHelper dvHelper;
  private final XSSFSheet sheet;
  private final XSSFDataValidationConstraint dvConstraint;

  public MonivalintaJoukko(
      Collection<String> joukko, XSSFSheet sheet, XSSFDataValidationHelper dvHelper) {
    this.joukko = joukko;
    this.dvHelper = dvHelper;
    this.sheet = sheet;
    this.dvConstraint =
        (XSSFDataValidationConstraint)
            dvHelper.createExplicitListConstraint(joukko.toArray(new String[] {}));
  }

  public MonivalintaJoukko(
      Collection<String> joukko,
      XSSFSheet sheet,
      XSSFDataValidationHelper dvHelper,
      String formula) {
    this.joukko = joukko;
    this.dvHelper = dvHelper;
    this.sheet = sheet;
    this.dvConstraint =
        (XSSFDataValidationConstraint) dvHelper.createFormulaListConstraint(formula);
  }

  public void addAddress(int row, int col) {
    XSSFDataValidation validation =
        (XSSFDataValidation)
            dvHelper.createValidation(
                dvConstraint, new org.apache.poi.ss.util.CellRangeAddressList(row, row, col, col));
    sheet.addValidationData(validation);
    validation.setSuppressDropDownArrow(false);
  }

  public Collection<String> getJoukko() {
    return joukko;
  }
}
