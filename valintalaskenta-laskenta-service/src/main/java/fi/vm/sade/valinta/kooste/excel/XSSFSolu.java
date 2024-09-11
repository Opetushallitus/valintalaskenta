package fi.vm.sade.valinta.kooste.excel;

import org.apache.commons.lang.StringUtils;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.DateUtil;
import org.apache.poi.xssf.usermodel.XSSFCell;

/** XSSFCell -> Solu */
public class XSSFSolu {

  private XSSFSolu() {}

  public static Solu asSolu(XSSFCell cell) {
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
      return new Teksti(rawValue);
    }
  }
}
