package fi.vm.sade.valintalaskenta.laskenta.testdata;

import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.hakemus.schema.HakukohdeTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.FunktiokutsuTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintakoeTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintakoeValinnanVaiheTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteetTyyppi;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.impl.util.HakukohdeValintakoeData;
import scala.actors.threadpool.Arrays;

import java.util.Map;

/**
 * User: wuoti
 * Date: 6.5.2013
 * Time: 12.57
 */
public abstract class TestDataUtil {

    public static HakemusTyyppi luoHakemus(String hakemusOid, String hakijaOid) {
        HakemusTyyppi hakemus = new HakemusTyyppi();
        hakemus.setHakemusOid(hakemusOid);
        hakemus.setHakijaOid(hakijaOid);

        return hakemus;
    }

    public static HakemusTyyppi luoHakemus(String hakemusOid, String hakijaOid, String... hakutoiveet) {
        HakemusTyyppi hakemus = luoHakemus(hakemusOid, hakijaOid);

        int i = 1;
        for (String hakutoive : hakutoiveet) {
            HakukohdeTyyppi toive = new HakukohdeTyyppi();
            toive.setHakukohdeOid(hakutoive);
            toive.setPrioriteetti(i);
            hakemus.getHakutoive().add(toive);
            ++i;
        }

        return hakemus;
    }

    public static HakemusTyyppi luoHakemus(String hakemusOid, String hakijaOid, HakukohdeTyyppi... hakutoiveet) {
        HakemusTyyppi hakemus = luoHakemus(hakemusOid, hakijaOid);
        hakemus.getHakutoive().addAll(Arrays.asList(hakutoiveet));

        return hakemus;
    }

    public static ValintaperusteetTyyppi luoValintaperusteet(String hakuOid, String hakukohdeOid) {
        ValintaperusteetTyyppi perusteet = new ValintaperusteetTyyppi();
        perusteet.setHakukohdeOid(hakukohdeOid);
        perusteet.setHakuOid(hakuOid);
        return perusteet;
    }

    public static ValintaperusteetTyyppi luoValintaperusteet(String hakuOid, String hakukohdeOid,
                                                             String valinnanVaiheOid,
                                                             int valinnanVaiheJarjestysluku,
                                                             String... valintakoeTunnisteet) {
        ValintaperusteetTyyppi perusteet = luoValintaperusteet(hakuOid, hakukohdeOid);
        perusteet.setValinnanVaihe(luoValinnanVaihe(valinnanVaiheOid,
                valinnanVaiheJarjestysluku,
                valintakoeTunnisteet));

        return perusteet;
    }

    public static ValintaperusteetTyyppi luoValintaperusteet(String hakuOid, String hakukohdeOid,
                                                             String valinnanVaiheOid,
                                                             int valinnanVaiheJarjestysluku,
                                                             Map<String, FunktiokutsuTyyppi> valintakokeetJaKaavat) {
        ValintaperusteetTyyppi perusteet = luoValintaperusteet(hakuOid, hakukohdeOid);
        perusteet.setValinnanVaihe(luoValinnanVaihe(valinnanVaiheOid, valinnanVaiheJarjestysluku, valintakokeetJaKaavat));

        return perusteet;
    }

    public static ValintakoeValinnanVaiheTyyppi luoValinnanVaihe(String valinnanVaiheOid, int jarjestysluku) {
        ValintakoeValinnanVaiheTyyppi vaihe = new ValintakoeValinnanVaiheTyyppi();
        vaihe.setValinnanVaiheOid(valinnanVaiheOid);
        vaihe.setValinnanVaiheJarjestysluku(jarjestysluku);

        return vaihe;
    }

    private static ValintakoeValinnanVaiheTyyppi luoValinnanVaihe(String valinnanVaiheOid,
                                                                  int valinnanVaiheJarjestysluku,
                                                                  Map<String, FunktiokutsuTyyppi> valintakokeetJaKaavat) {
        ValintakoeValinnanVaiheTyyppi vaihe = luoValinnanVaihe(valinnanVaiheOid, valinnanVaiheJarjestysluku);

        for(Map.Entry<String, FunktiokutsuTyyppi>e: valintakokeetJaKaavat.entrySet()) {
            ValintakoeTyyppi koe = luoValintakoe(e.getKey(), e.getKey());
            koe.setFunktiokutsu(e.getValue());
            vaihe.getValintakoe().add(koe);
        }

        return vaihe;
    }

    public static ValintakoeValinnanVaiheTyyppi luoValinnanVaihe(String valinnanVaiheOid, int jarjestysluku,
                                                                 String... valintakoeTunnisteet) {
        ValintakoeValinnanVaiheTyyppi vaihe = luoValinnanVaihe(valinnanVaiheOid, jarjestysluku);

        for (String tunniste : valintakoeTunnisteet) {
            vaihe.getValintakoe().add(luoValintakoe(tunniste, tunniste));
        }

        return vaihe;
    }

    public static ValintakoeTyyppi luoValintakoe(String valintakoeOid, String tunniste) {
        ValintakoeTyyppi koe = new ValintakoeTyyppi();
        koe.setFunktiokutsu(new FunktiokutsuTyyppi());
        koe.setTunniste(tunniste);
        koe.setOid(valintakoeOid);
        return koe;
    }

    public static HakukohdeTyyppi luoHakukohdeTyyppi(String hakukohdeOid, int prioriteetti) {
        HakukohdeTyyppi hakukohde = new HakukohdeTyyppi();
        hakukohde.setHakukohdeOid(hakukohdeOid);
        hakukohde.setPrioriteetti(prioriteetti);

        return hakukohde;
    }

    public static HakukohdeValintakoeData luoHakukohdeValintakoeData(String hakukohdeOid,
                                                                     Osallistuminen osallistuminen,
                                                                     String valintakoeTunniste) {

        HakukohdeValintakoeData koe = new HakukohdeValintakoeData();
        koe.setHakukohdeOid(hakukohdeOid);
        koe.setOsallistuminen(osallistuminen);
        koe.setValintakoeTunniste(valintakoeTunniste);

        return koe;
    }
}
