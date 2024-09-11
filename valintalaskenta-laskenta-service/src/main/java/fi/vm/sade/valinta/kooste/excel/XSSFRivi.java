package fi.vm.sade.valinta.kooste.excel;

import static org.apache.poi.ss.usermodel.Row.MissingCellPolicy.CREATE_NULL_AS_BLANK;

import com.google.common.collect.Lists;
import java.util.List;
import org.apache.poi.xssf.usermodel.XSSFCell;
import org.apache.poi.xssf.usermodel.XSSFRow;

/** XSSFRow -> Rivi */
public class XSSFRivi {

  private XSSFRivi() {}

  private static List<Solu> soluiksi(XSSFRow row) {
    List<Solu> solut = Lists.newArrayList();
    int lastCellIndex = row.getLastCellNum();
    for (int i = 0; i < lastCellIndex; ++i) {
      XSSFCell cell = row.getCell(i, CREATE_NULL_AS_BLANK);
      solut.add(XSSFSolu.asSolu(cell));
    }
    return solut;
  }

  public static Rivi asRivi(XSSFRow row) {
    return new Rivi(soluiksi(row));
  }
}
