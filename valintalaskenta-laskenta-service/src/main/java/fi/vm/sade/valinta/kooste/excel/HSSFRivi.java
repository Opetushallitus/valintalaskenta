package fi.vm.sade.valinta.kooste.excel;

import static org.apache.poi.ss.usermodel.Row.MissingCellPolicy.CREATE_NULL_AS_BLANK;

import com.google.common.collect.Lists;
import java.util.List;
import org.apache.poi.hssf.usermodel.HSSFCell;
import org.apache.poi.hssf.usermodel.HSSFRow;

public class HSSFRivi {

  private HSSFRivi() {}

  private static List<Solu> soluiksi(HSSFRow row) {
    List<Solu> solut = Lists.newArrayList();
    int lastCellIndex = row.getLastCellNum();
    for (int i = 0; i < lastCellIndex; ++i) {
      HSSFCell cell = row.getCell(i, CREATE_NULL_AS_BLANK);
      solut.add(HSSFSolu.asSolu(cell));
    }
    return solut;
  }

  public static Rivi asRivi(HSSFRow row) {
    return new Rivi(soluiksi(row));
  }
}
