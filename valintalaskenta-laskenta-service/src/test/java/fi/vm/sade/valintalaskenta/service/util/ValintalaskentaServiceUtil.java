package fi.vm.sade.valintalaskenta.service.util;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import fi.vm.sade.service.hakemus.schema.AvainArvoTyyppi;
import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.hakemus.schema.HakukohdeTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.FunktioargumenttiTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.FunktiokutsuTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.JarjestyskriteeriTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.TasasijasaantoTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteetTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteviiteTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintatapajonoJarjestyskriteereillaTyyppi;

/**
 * 
 * @author Jussi Jartamo
 * 
 *         Yksikkötestien testiarvojen syöttämiseen
 * 
 */
public class ValintalaskentaServiceUtil {

    public static HakemusTyyppi createHakemusParillaDynaamisesti(String hakemusoid, String hakukohdeoid,
            List<String[]> avaimet) {
        return createHakemusTyyppi(hakemusoid, hakukohdeoid, createAvainArvo(avaimet.toArray(new String[][] {})));
    }

    public static HakemusTyyppi createHakemusParilla(String hakemusoid, String hakukohdeoid, String[]... avaimet) {
        return createHakemusTyyppi(hakemusoid, hakukohdeoid, createAvainArvo(avaimet));
    }

    public static HakemusTyyppi createHakemus(String hakemusoid, String hakukohdeoid, String... avaimet) {
        return createHakemusTyyppi(hakemusoid, hakukohdeoid, createAvainArvo(avaimet));
    }

    public static ValintaperusteetTyyppi createValintaperusteet(String hakukohdeoid, int jarjestysluku) {
        ValintaperusteetTyyppi valintaperusteet = new ValintaperusteetTyyppi();
        valintaperusteet.setHakukohdeOid(hakukohdeoid);
        valintaperusteet.setValinnanVaiheJarjestysluku(jarjestysluku);

        return valintaperusteet;
    }

    public static HakemusTyyppi createHakemusTyyppi(String hakemusoid, String hakukohdeoid,
            Map<String, String> avainArvo) {
        HakukohdeTyyppi hakukohde1 = new HakukohdeTyyppi();
        hakukohde1.setHakukohdeOid(hakukohdeoid);
        hakukohde1.setPrioriteetti(1);
        HakemusTyyppi hakemus = new HakemusTyyppi();
        hakemus.setHakemusOid(hakemusoid);
        hakemus.getHakukohde().add(hakukohde1);
        for (Entry<String, String> arvopari : avainArvo.entrySet()) {
            AvainArvoTyyppi uusiavainarvo = new AvainArvoTyyppi();
            uusiavainarvo.setAvain(arvopari.getKey());
            uusiavainarvo.setArvo(arvopari.getValue());
            hakemus.getAvainArvo().add(uusiavainarvo);
        }
        return hakemus;
    }

    public static FunktiokutsuTyyppi createSummaFunktio(String... tunniste) {
        FunktiokutsuTyyppi funktiokutsu = new FunktiokutsuTyyppi();
        funktiokutsu.setFunktionimi("SUMMA");
        for (int i = 0; i < tunniste.length; ++i) {
            FunktioargumenttiTyyppi arg = ValintalaskentaServiceUtil.createHaelukuarvoArgumentti(tunniste[i], i + 1);
            funktiokutsu.getFunktioargumentit().add(arg);
        }
        return funktiokutsu;
    }

    public static JarjestyskriteeriTyyppi createJarjestyskriteeri() {
        JarjestyskriteeriTyyppi jarjestyskriteeri0 = new JarjestyskriteeriTyyppi();
        jarjestyskriteeri0.setPrioriteetti(1);
        return jarjestyskriteeri0;
    }

    public static ValintatapajonoJarjestyskriteereillaTyyppi createValintatapajono(String oid) {
        ValintatapajonoJarjestyskriteereillaTyyppi jono = new ValintatapajonoJarjestyskriteereillaTyyppi();
        jono.setAloituspaikat(10);
        jono.setNimi(oid);
        jono.setOid(oid);
        jono.setSiirretaanSijoitteluun(true);
        jono.setPrioriteetti(1);
        jono.setTasasijasaanto(TasasijasaantoTyyppi.ARVONTA);
        return jono;
    }

    private static final String[] TESTI_ARVOJA = { "5.0", "9.0", "7.0", "10.0" };

    public static Map<String, String> createAvainArvo(String... avaimet) {
        Map<String, String> mapping = new HashMap<String, String>();
        int indeksi = 0;
        for (String avain : avaimet) {
            mapping.put(avain, TESTI_ARVOJA[indeksi % TESTI_ARVOJA.length]);
            ++indeksi;
        }
        return mapping;
    }

    public static Map<String, String> createAvainArvo(String[]... avaimet) {
        Map<String, String> mapping = new HashMap<String, String>();
        for (String[] avain : avaimet) {
            mapping.put(avain[0], avain[1]);
        }
        return mapping;
    }

    public static FunktioargumenttiTyyppi createHaelukuarvoArgumentti(String tunniste, int indeksi) {
        FunktioargumenttiTyyppi farg1 = new FunktioargumenttiTyyppi();
        FunktiokutsuTyyppi haearvo1 = new FunktiokutsuTyyppi();
        haearvo1.setFunktionimi("HAELUKUARVO");
        ValintaperusteviiteTyyppi viite1 = new ValintaperusteviiteTyyppi();
        viite1.setOnPakollinen(true);
        viite1.setTunniste(tunniste);
        haearvo1.getValintaperusteviite().add(viite1);
        farg1.setFunktiokutsu(haearvo1);
        farg1.setIndeksi(indeksi);
        return farg1;
    }

    public static ValintaperusteetTyyppi createValintaperusteetTyyppi(String oid) {
        ValintaperusteetTyyppi valintaperusteet = new ValintaperusteetTyyppi();
        valintaperusteet.setHakukohdeOid(oid);

        return valintaperusteet;
    }
}
