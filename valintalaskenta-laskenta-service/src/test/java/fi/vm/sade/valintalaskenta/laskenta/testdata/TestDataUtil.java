package fi.vm.sade.valintalaskenta.laskenta.testdata;

import fi.vm.sade.auditlog.User;
import fi.vm.sade.service.valintaperusteet.dto.*;
import fi.vm.sade.service.valintaperusteet.dto.model.Funktionimi;
import fi.vm.sade.service.valintaperusteet.dto.model.Koekutsu;
import fi.vm.sade.service.valintaperusteet.dto.model.ValinnanVaiheTyyppi;
import fi.vm.sade.valintalaskenta.domain.dto.*;
import fi.vm.sade.valintalaskenta.domain.dto.AvainArvoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.Tasasijasaanto;
import fi.vm.sade.valintalaskenta.domain.valintakoe.*;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.impl.util.HakukohdeValintakoeData;
import java.net.InetAddress;
import java.util.*;
import org.mockito.Mockito;

/** User: wuoti Date: 6.5.2013 Time: 12.57 */
public abstract class TestDataUtil {

  public static final User TEST_AUDIT_USER =
      new User(Mockito.mock(InetAddress.class), "mock-session", "mock-user-agent");

  public static HakemusDTO luoHakemus(String hakuOid, String hakemusOid, String hakijaOid) {
    HakemusDTO hakemus = new HakemusDTO();
    hakemus.setHakemusoid(hakemusOid);
    hakemus.setHakijaOid(hakijaOid);
    hakemus.setHakuoid(hakuOid);
    List<AvainArvoDTO> avaimet = new LinkedList<>();
    AvainArvoDTO avain = new AvainArvoDTO();
    avain.setAvain("hakukohdeKutsunKohde2");
    avain.setArvo("hakukohdeOid2");
    avaimet.add(avain);
    hakemus.setAvaimet(avaimet);
    return hakemus;
  }

  public static HakemusDTO luoHakemus(
      String hakuOid, String hakemusOid, String hakijaOid, String... hakutoiveet) {
    HakemusDTO hakemus = luoHakemus(hakuOid, hakemusOid, hakijaOid);
    int i = 1;
    for (String hakutoive : hakutoiveet) {
      HakukohdeDTO toive = new HakukohdeDTO();
      toive.setOid(hakutoive);
      toive.setPrioriteetti(i);
      hakemus.getHakukohteet().add(toive);
      ++i;
    }
    return hakemus;
  }

  public static ValintaperusteetDTO luoValintaperusteet(String hakuOid, String hakukohdeOid) {
    ValintaperusteetDTO perusteet = new ValintaperusteetDTO();
    perusteet.setHakukohdeOid(hakukohdeOid);
    perusteet.setHakuOid(hakuOid);
    return perusteet;
  }

  public static ValintaperusteetDTO luoValintaperusteetJaValintakoeValinnanvaihe(
      String hakuOid,
      String hakukohdeOid,
      String valinnanVaiheOid,
      int valinnanVaiheJarjestysluku,
      String... valintakoeTunnisteet) {
    ValintaperusteetDTO perusteet = luoValintaperusteet(hakuOid, hakukohdeOid);
    perusteet.setValinnanVaihe(
        luoValintakoeValinnanVaihe(
            valinnanVaiheOid,
            valinnanVaiheJarjestysluku,
            Koekutsu.YLIN_TOIVE,
            "kutsunKohdeAvain",
            valintakoeTunnisteet));
    return perusteet;
  }

  public static ValintaperusteetDTO luoValintaperusteetJaTavallinenValinnanvaihe(
      String hakuOid,
      String hakukohdeOid,
      String valinnanVaiheOid,
      int valinnanVaiheJarjestysluku) {
    ValintaperusteetDTO perusteet = luoValintaperusteet(hakuOid, hakukohdeOid);
    perusteet.setTarjoajaOid("tarjoaja");
    perusteet.setValinnanVaihe(
        luoTavallinenValinnanvaihe(valinnanVaiheOid, valinnanVaiheJarjestysluku));
    return perusteet;
  }

  public static ValintaperusteetValinnanVaiheDTO luoTavallinenValinnanvaihe(
      String valinnanVaiheOid, int valinnanVaiheJarjestysluku) {
    ValintaperusteetValinnanVaiheDTO vaihe = new ValintaperusteetValinnanVaiheDTO();
    vaihe.setValinnanVaiheOid(valinnanVaiheOid);
    vaihe.setValinnanVaiheJarjestysluku(valinnanVaiheJarjestysluku);
    vaihe.setValinnanVaiheTyyppi(ValinnanVaiheTyyppi.TAVALLINEN);
    return vaihe;
  }

  public static ValintatapajonoJarjestyskriteereillaDTO luoValintatapajono(
      String valintatapajonoOid,
      int prioriteetti,
      int aloituspaikat,
      ValintaperusteetJarjestyskriteeriDTO... jarjestyskriteerit) {
    ValintatapajonoJarjestyskriteereillaDTO jono = new ValintatapajonoJarjestyskriteereillaDTO();
    jono.setOid(valintatapajonoOid);
    jono.setAloituspaikat(aloituspaikat);
    jono.setNimi(valintatapajonoOid);
    jono.setPrioriteetti(prioriteetti);
    jono.setSiirretaanSijoitteluun(true);
    jono.setTasasijasaanto(Tasasijasaanto.ARVONTA.name());
    jono.getJarjestyskriteerit().addAll(Arrays.asList(jarjestyskriteerit));
    return jono;
  }

  public static ValintaperusteetJarjestyskriteeriDTO luoJarjestyskriteeri(
      ValintaperusteetFunktiokutsuDTO funktiokutsu, int prioriteetti) {
    ValintaperusteetJarjestyskriteeriDTO jk = new ValintaperusteetJarjestyskriteeriDTO();
    jk.setFunktiokutsu(funktiokutsu);
    jk.setPrioriteetti(prioriteetti);
    return jk;
  }

  public static ValintaperusteetDTO luoValintaperusteetJaValintakoeValinnanVaihe(
      String hakuOid,
      String hakukohdeOid,
      String valinnanVaiheOid,
      int valinnanVaiheJarjestysluku,
      Map<String, FunktiokutsuDTO> valintakokeetJaKaavat,
      Koekutsu kutsunKohde,
      String kutsunKohdeAvain) {
    ValintaperusteetDTO perusteet = luoValintaperusteet(hakuOid, hakukohdeOid);
    perusteet.setValinnanVaihe(
        luoValinnanVaihe(
            valinnanVaiheOid,
            valinnanVaiheJarjestysluku,
            valintakokeetJaKaavat,
            kutsunKohde,
            kutsunKohdeAvain));
    return perusteet;
  }

  public static ValintaperusteetValinnanVaiheDTO luoValintakoeValinnanVaihe(
      String valinnanVaiheOid, int jarjestysluku) {
    ValintaperusteetValinnanVaiheDTO vaihe = new ValintaperusteetValinnanVaiheDTO();
    vaihe.setValinnanVaiheOid(valinnanVaiheOid);
    vaihe.setValinnanVaiheJarjestysluku(jarjestysluku);
    vaihe.setValinnanVaiheTyyppi(ValinnanVaiheTyyppi.VALINTAKOE);
    return vaihe;
  }

  private static ValintaperusteetValinnanVaiheDTO luoValinnanVaihe(
      String valinnanVaiheOid,
      int valinnanVaiheJarjestysluku,
      Map<String, FunktiokutsuDTO> valintakokeetJaKaavat,
      Koekutsu kutsunKohde,
      String kutsunKohdeAvain) {
    ValintaperusteetValinnanVaiheDTO vaihe =
        luoValintakoeValinnanVaihe(valinnanVaiheOid, valinnanVaiheJarjestysluku);
    for (Map.Entry<String, FunktiokutsuDTO> e : valintakokeetJaKaavat.entrySet()) {
      ValintakoeDTO koe = luoValintakoe(e.getKey(), e.getKey(), kutsunKohde, kutsunKohdeAvain);
      koe.setFunktiokutsu(e.getValue());
      vaihe.getValintakoe().add(koe);
    }
    return vaihe;
  }

  public static ValintaperusteetValinnanVaiheDTO luoValintakoeValinnanVaihe(
      String valinnanVaiheOid,
      int jarjestysluku,
      Koekutsu kutsunKohde,
      String kutsunKohdeAvain,
      String... valintakoeTunnisteet) {
    ValintaperusteetValinnanVaiheDTO vaihe =
        luoValintakoeValinnanVaihe(valinnanVaiheOid, jarjestysluku);
    for (String tunniste : valintakoeTunnisteet) {
      vaihe.getValintakoe().add(luoValintakoe(tunniste, tunniste, kutsunKohde, kutsunKohdeAvain));
    }
    return vaihe;
  }

  public static ValintakoeDTO luoValintakoe(
      String valintakoeOid, String tunniste, Koekutsu kutsunKohde, String kutsunKohdeAvain) {
    ValintakoeDTO koe = new ValintakoeDTO();
    koe.setFunktiokutsu(luoFunktioKutsu());
    koe.setTunniste(tunniste);
    koe.setOid(UUID.randomUUID().toString());
    koe.setLahetetaankoKoekutsut(true);
    koe.setAktiivinen(true);
    koe.setKutsutaankoKaikki(false);
    koe.setKutsunKohde(kutsunKohde);
    koe.setKutsunKohdeAvain(kutsunKohdeAvain);
    return koe;
  }

  public static FunktiokutsuDTO luoFunktioKutsu() {
    FunktiokutsuDTO kutsu = new FunktiokutsuDTO();
    kutsu.setFunktionimi(Funktionimi.LUKUARVO);
    SyoteparametriDTO param = new SyoteparametriDTO();
    param.setArvo("5");
    param.setAvain("luku");
    kutsu.setSyoteparametrit(Set.of(param));
    return kutsu;
  }

  public static HakukohdeDTO luoHakukohdeDTO(String hakukohdeOid, int prioriteetti) {
    HakukohdeDTO hakukohde = new HakukohdeDTO();
    hakukohde.setOid(hakukohdeOid);
    hakukohde.setPrioriteetti(prioriteetti);
    return hakukohde;
  }

  public static HakukohdeValintakoeData luoHakukohdeValintakoeData(
      String hakukohdeOid, Osallistuminen osallistuminen, String valintakoeTunniste) {
    HakukohdeValintakoeData koe = new HakukohdeValintakoeData();
    koe.setHakukohdeOid(hakukohdeOid);
    koe.setKutsunKohde(Koekutsu.YLIN_TOIVE);
    OsallistuminenTulos osallistuminenTulos = new OsallistuminenTulos();
    osallistuminenTulos.setOsallistuminen(osallistuminen);
    koe.setOsallistuminenTulos(osallistuminenTulos);
    koe.setValintakoeTunniste(valintakoeTunniste);
    return koe;
  }
}
