package fi.vm.sade.valintalaskenta.tulos.service.impl.converters;

import fi.vm.sade.valintalaskenta.domain.dto.*;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.*;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.ValintatietoValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.ValintatietoValintatapajonoDTO;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;
import fi.vm.sade.valintalaskenta.domain.valintakoe.*;
import java.util.Collection;
import java.util.List;

public interface ValintatulosConverter {
  void sort(List<JonosijaDTO> list);

  List<JonosijaDTO> convertJonosija(Collection<Jonosija> jonosijat);

  List<ValintatietoValintatapajonoDTO> convertValintatapajono(
      List<Valintatapajono> valintapajonoList);

  List<ValintatietoValinnanvaiheDTO> convertValinnanvaiheList(
      List<Valinnanvaihe> valinnanVaiheList);

  List<ValintakoeOsallistuminenDTO> convertValintakoeOsallistuminen(
      List<ValintakoeOsallistuminen> osallistumiset);

  List<HakutoiveDTO> convertHakutoive(List<Hakutoive> hakutoiveet);

  List<ValintakoeValinnanvaiheDTO> convertValinnanVaihe(
      List<ValintakoeValinnanvaihe> valinnanVaiheet);

  List<ValintakoeDTO> convertValintakoe(List<Valintakoe> valintakokeet);

  OsallistuminenTulosDTO convertOsallistuminenTulos(Valintakoe valintakoe);

  List<JarjestyskriteeritulosDTO> convertJarjestyskriteeri(
      Collection<Jarjestyskriteeritulos> jktulos);

  JarjestyskriteeritulosDTO convertJarjestyskriteeri(Jarjestyskriteeritulos jktulos);

  ValintatietoValintatapajonoDTO convertValintatapajono(Valintatapajono jono);

  ValintatietoValinnanvaiheDTO convertValinnanvaihe(Valinnanvaihe valinnanvaihe);

  List<HakukohdeDTO> convertValinnanvaihe(Collection<Valinnanvaihe> valinnanvaiheet);
}
