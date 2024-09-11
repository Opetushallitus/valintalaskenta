package fi.vm.sade.valinta.kooste.excel;

import org.apache.commons.lang.StringUtils;
import org.apache.poi.hssf.usermodel.HSSFCell;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.DateUtil;

public class HSSFSolu {
  private HSSFSolu() {}

  public static Solu asSolu(HSSFCell cell) {
    if (CellType.NUMERIC.equals(cell.getCellTypeEnum())) {
      if (DateUtil.isCellDateFormatted(cell)) {
        return new Paivamaara(cell.getDateCellValue());
      } else {
        return new Numero(cell.getNumericCellValue());
      }
    } else {
      String rawValue;
      if (CellType.STRING.equals(cell.getCellTypeEnum())) {
        rawValue = cell.getStringCellValue();
      } else {
        rawValue = StringUtils.EMPTY;
      }

      try {
        String maybeNumber = rawValue.replace(",", ".");
        double d = Double.parseDouble(maybeNumber);
        return new Numero(d);
      } catch (Exception e) {
      }
      return new Teksti(rawValue);
    }
  }
}
