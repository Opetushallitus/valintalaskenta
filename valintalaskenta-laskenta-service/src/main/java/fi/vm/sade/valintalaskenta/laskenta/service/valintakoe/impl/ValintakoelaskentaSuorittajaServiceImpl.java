package fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.impl;

import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.hakemus.schema.HakukohdeTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintakoeTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintakoeValinnanVaiheTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteetTyyppi;
import fi.vm.sade.valintalaskenta.domain.valintakoe.*;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValintakoeOsallistuminenDAO;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.ValintakoelaskentaSuorittajaService;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.Valintakoeosallistumislaskin;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.impl.util.HakukohdeValintakoeData;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.*;

/**
 * User: wuoti
 * Date: 2.5.2013
 * Time: 9.16
 */
@Service
public class ValintakoelaskentaSuorittajaServiceImpl implements ValintakoelaskentaSuorittajaService {

    private static final Logger LOG = LoggerFactory.getLogger(ValintakoelaskentaSuorittajaServiceImpl.class);


    @Autowired
    private ValintakoeOsallistuminenDAO valintakoeOsallistuminenDAO;

    @Autowired
    private Valintakoeosallistumislaskin valintakoeosallistumislaskin;

    @Override
    public void laske(HakemusTyyppi hakemus, List<ValintaperusteetTyyppi> valintaperusteet) {

        LOG.info("Laskentaan valintakoeosallistumiset hakemukselle {}", hakemus.getHakemusOid());

        final Map<String, HakukohdeTyyppi> hakutoiveetByOid = luoHakutoiveMap(hakemus.getHakutoive());
        Map<String, List<HakukohdeValintakoeData>> valintakoeData = new HashMap<String, List<HakukohdeValintakoeData>>();

        for (ValintaperusteetTyyppi vp : valintaperusteet) {
            if (hakutoiveetByOid.containsKey(vp.getHakukohdeOid()) &&
                    vp.getValinnanVaihe() instanceof ValintakoeValinnanVaiheTyyppi) {
                ValintakoeValinnanVaiheTyyppi vaihe = (ValintakoeValinnanVaiheTyyppi) vp.getValinnanVaihe();

                for (ValintakoeTyyppi koe : vaihe.getValintakoe()) {
                    OsallistuminenTulos osallistuminen = valintakoeosallistumislaskin.laskeOsallistuminenYhdelleHakukohteelle(
                            vp.getHakukohdeOid(), hakemus, koe.getFunktiokutsu());

                    HakukohdeValintakoeData data = new HakukohdeValintakoeData();
                    data.setHakuOid(vp.getHakuOid());
                    data.setHakukohdeOid(vp.getHakukohdeOid());
                    data.setOsallistuminenTulos(osallistuminen);
                    data.setValinnanVaiheJarjestysNro(vaihe.getValinnanVaiheJarjestysluku());
                    data.setValinnanVaiheOid(vaihe.getValinnanVaiheOid());
                    data.setValintakoeTunniste(koe.getTunniste());
                    data.setValintakoeOid(koe.getOid());

                    if (!valintakoeData.containsKey(koe.getTunniste())) {
                        valintakoeData.put(koe.getTunniste(), new ArrayList<HakukohdeValintakoeData>());
                    }

                    valintakoeData.get(koe.getTunniste()).add(data);
                }
            }
        }


        Map<String, ValintakoeOsallistuminen> osallistumisetByHaku = new HashMap<String, ValintakoeOsallistuminen>();
        for (Map.Entry<String, List<HakukohdeValintakoeData>> entry : valintakoeData.entrySet()) {
            List<HakukohdeValintakoeData> kokeet = entry.getValue();

            asetaOsallistumisetKokeisiin(kokeet, hakutoiveetByOid);
            for (HakukohdeValintakoeData c : kokeet) {
                LOG.info("Hakukohde: {}, valintakoe: {}", new Object[]{c.getHakukohdeOid(), c.getValintakoeTunniste()});

                if (!osallistumisetByHaku.containsKey(c.getHakuOid())) {
                    osallistumisetByHaku.put(c.getHakuOid(), luoValintakoeOsallistuminen(c, hakemus));
                }

                ValintakoeOsallistuminen osallistuminen = osallistumisetByHaku.get(c.getHakuOid());
                haeTaiLuoHakutoive(osallistuminen, c);
            }
        }

        for (ValintakoeOsallistuminen osallistuminen : osallistumisetByHaku.values()) {
            valintakoeOsallistuminenDAO.createOrUpdate(osallistuminen);
        }
    }

    protected void asetaOsallistumisetKokeisiin(List<HakukohdeValintakoeData> kokeet,
                                                final Map<String, HakukohdeTyyppi> hakukohteetByOid) {

        // Käydään hakijan hakutoiveet läpi prioriteetin mukaan ja asetetaan kullekin hakukohteelle
        // valintakoekohtainen osallistumistieto

        Collections.sort(kokeet, new Comparator<HakukohdeValintakoeData>() {
            @Override
            public int compare(HakukohdeValintakoeData o1, HakukohdeValintakoeData o2) {
                return hakukohteetByOid.get(o1.getHakukohdeOid()).getPrioriteetti() - hakukohteetByOid
                        .get(o2.getHakukohdeOid()).getPrioriteetti();
            }
        });

        // Jos hakija osallistuu korkeamman prioriteetin hakuktoiveen valintakokeeseen, hakija ei osallistu
        // pienemmällä prioriteetilla oleviin samoihin valintakokeisiin.
        boolean osallistuminenLoydetty = false;
        for (HakukohdeValintakoeData d : kokeet) {
            if (!osallistuminenLoydetty && Osallistuminen.OSALLISTUU.equals(d.getOsallistuminenTulos().getOsallistuminen())) {
                osallistuminenLoydetty = true;
                continue;
            }

            if (!d.getOsallistuminenTulos().getOsallistuminen().equals(Osallistuminen.VIRHE)) {
                d.getOsallistuminenTulos().setOsallistuminen(Osallistuminen.EI_OSALLISTU);
            }
        }
    }


    protected ValintakoeOsallistuminen luoValintakoeOsallistuminen(HakukohdeValintakoeData data, HakemusTyyppi hakemus) {
        ValintakoeOsallistuminen osallistuminen =
                valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(data.getHakuOid(),
                        hakemus.getHakemusOid());

        if (osallistuminen == null) {
            osallistuminen = new ValintakoeOsallistuminen();
        }

//        System.out.println("here we are: " + osallistuminen.getEtunimi() + " " + osallistuminen.getSukunimi());

        osallistuminen.setHakuOid(data.getHakuOid());

        osallistuminen.setHakemusOid(hakemus.getHakemusOid());
        osallistuminen.setHakijaOid(hakemus.getHakijaOid());
        osallistuminen.setSukunimi(hakemus.getHakijanSukunimi());
        osallistuminen.setEtunimi(hakemus.getHakijanEtunimi());

        return osallistuminen;
    }

    protected void haeTaiLuoHakutoive(ValintakoeOsallistuminen osallistuminen, HakukohdeValintakoeData data) {
        Hakutoive toive = null;
        for (Hakutoive t : osallistuminen.getHakutoiveet()) {
            if (data.getHakukohdeOid().equals(t.getHakukohdeOid())) {
                toive = t;
                break;
            }
        }

        if (toive == null) {
            toive = new Hakutoive();
            osallistuminen.getHakutoiveet().add(toive);
        }
        toive.setHakukohdeOid(data.getHakukohdeOid());

        haeTaiLuoValinnanVaihe(toive, data);
    }

    protected void haeTaiLuoValinnanVaihe(Hakutoive hakutoive, HakukohdeValintakoeData data) {
        ValintakoeValinnanvaihe vaihe = null;

        for (ValintakoeValinnanvaihe v : hakutoive.getValinnanVaiheet()) {
            if (data.getValinnanVaiheOid().equals(v.getValinnanVaiheOid())) {
                vaihe = v;
                break;
            }
        }

        if (vaihe == null) {
            vaihe = new ValintakoeValinnanvaihe();
            hakutoive.getValinnanVaiheet().add(vaihe);
        }
        vaihe.setValinnanVaiheOid(data.getValinnanVaiheOid());
        vaihe.setValinnanVaiheJarjestysluku(data.getValinnanVaiheJarjestysNro());

        haeTaiLuoValintakoe(vaihe, data);
    }

    protected void haeTaiLuoValintakoe(ValintakoeValinnanvaihe valinnanVaihe, HakukohdeValintakoeData data) {
        Valintakoe koe = null;
        for (Valintakoe k : valinnanVaihe.getValintakokeet()) {
            if (data.getValintakoeTunniste().equals(k.getValintakoeTunniste())) {
                koe = k;
                break;
            }
        }

        if (koe == null) {
            koe = new Valintakoe();
            valinnanVaihe.getValintakokeet().add(koe);
        }

        koe.setOsallistuminenTulos(data.getOsallistuminenTulos());
        koe.setValintakoeOid(data.getValintakoeOid());
        koe.setValintakoeTunniste(data.getValintakoeTunniste());
    }


    protected Map<String, HakukohdeTyyppi> luoHakutoiveMap(List<HakukohdeTyyppi> hakutoiveet) {
        Map<String, HakukohdeTyyppi> toiveetMap = new HashMap<String, HakukohdeTyyppi>();
        for (HakukohdeTyyppi hk : hakutoiveet) {
            toiveetMap.put(hk.getHakukohdeOid(), hk);
        }

        return toiveetMap;
    }
}