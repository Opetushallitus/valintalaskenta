package fi.vm.sade.valinta.kooste.valintalaskenta.excel;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import fi.vm.sade.valinta.kooste.util.ExcelExportUtil;
import fi.vm.sade.valinta.seuranta.dto.*;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class LaskentaDtoAsExcel {

  public static byte[] laskentaDtoAsExcel(LaskentaDto laskenta) {
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
