package fi.vm.sade.valinta.kooste.excel;

import org.apache.poi.ss.usermodel.DataValidationConstraint.OperatorType;
import org.apache.poi.ss.usermodel.DataValidationConstraint.ValidationType;
import org.apache.poi.xssf.usermodel.XSSFDataValidation;
import org.apache.poi.xssf.usermodel.XSSFDataValidationConstraint;
import org.apache.poi.xssf.usermodel.XSSFDataValidationHelper;
import org.apache.poi.xssf.usermodel.XSSFSheet;

public class ArvovaliJoukko {
  private final XSSFDataValidationHelper dvHelper;
  private final XSSFSheet sheet;
  private final XSSFDataValidationConstraint dvConstraint;

  public ArvovaliJoukko(
      Number min, Number max, XSSFSheet sheet, XSSFDataValidationHelper dvHelper) {
    this.dvHelper = dvHelper;
    this.sheet = sheet;

    this.dvConstraint =
        (XSSFDataValidationConstraint)
            dvHelper.createNumericConstraint(
                ValidationType.DECIMAL, OperatorType.BETWEEN, min.toString(), max.toString());
  }

  public void addAddress(int row, int col) {
    XSSFDataValidation validation =
        (XSSFDataValidation)
            dvHelper.createValidation(
                dvConstraint, new org.apache.poi.ss.util.CellRangeAddressList(row, row, col, col));
    // validation.setShowErrorBox(true);
    sheet.addValidationData(validation);
    validation.setSuppressDropDownArrow(false);
  }
}
