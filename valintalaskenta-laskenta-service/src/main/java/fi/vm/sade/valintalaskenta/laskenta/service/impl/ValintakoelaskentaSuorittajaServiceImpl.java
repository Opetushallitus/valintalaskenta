package fi.vm.sade.valintalaskenta.laskenta.service.impl;

import fi.vm.sade.kaava.Laskentadomainkonvertteri;
import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.hakemus.schema.HakukohdeTyyppi;
import fi.vm.sade.service.valintaperusteet.laskenta.api.LaskentaService;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Laskentatulos;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Tila;
import fi.vm.sade.service.valintaperusteet.model.Funktiokutsu;
import fi.vm.sade.service.valintaperusteet.schema.ValintakoeTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintakoeValinnanVaiheTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteetTyyppi;
import fi.vm.sade.valintalaskenta.dao.ValintakoeOsallistuminenDAO;
import fi.vm.sade.valintalaskenta.domain.valintakoe.*;
import fi.vm.sade.valintalaskenta.laskenta.service.ValintakoelaskentaSuorittajaService;
import fi.vm.sade.valintalaskenta.laskenta.service.exception.LaskentaVaarantyyppisellaFunktiollaException;
import fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion.FunktioKutsuTyyppiToFunktioKutsuConverter;
import fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion.HakemusTyyppiToHakemusConverter;
import fi.vm.sade.valintalaskenta.laskenta.service.impl.util.HakukohdeValintakoeData;
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

    @Autowired
    private LaskentaService laskentaService;

    @Autowired
    private FunktioKutsuTyyppiToFunktioKutsuConverter funktiokutsuConverter;

    @Autowired
    private HakemusTyyppiToHakemusConverter hakemusConverter;

    @Autowired
    private ValintakoeOsallistuminenDAO valintakoeOsallistuminenDAO;

    @Override
    public void laske(HakemusTyyppi hakemus, List<ValintaperusteetTyyppi> valintaperusteet) {

        final Map<String, HakukohdeTyyppi> hakutoiveetByOid = luoHakutoiveMap(hakemus.getHakutoive());
        Map<String, List<HakukohdeValintakoeData>> valintakoeData = new HashMap<String, List<HakukohdeValintakoeData>>();

        for (ValintaperusteetTyyppi vp : valintaperusteet) {
            if (hakutoiveetByOid.containsKey(vp.getHakukohdeOid()) &&
                    vp.getValinnanVaihe() instanceof ValintakoeValinnanVaiheTyyppi) {
                ValintakoeValinnanVaiheTyyppi vaihe = (ValintakoeValinnanVaiheTyyppi) vp.getValinnanVaihe();

                for (ValintakoeTyyppi koe : vaihe.getValintakoe()) {
                    boolean tulos = laskeKaava(vp.getHakukohdeOid(), hakemus,
                            funktiokutsuConverter.convert(koe.getFunktiokutsu()));

                    HakukohdeValintakoeData container = new HakukohdeValintakoeData();
                    container.setHakuOid(vp.getHakuOid());
                    container.setHakukohdeOid(vp.getHakukohdeOid());
                    container.setOsallistuminen(tulos ? Osallistuminen.OSALLISTUU : Osallistuminen.EI_OSALLISTU);
                    container.setValinnanVaiheJarjestysNro(vaihe.getValinnanVaiheJarjestysluku());
                    container.setValinnanVaiheOid(vaihe.getValinnanVaiheOid());
                    container.setValintakoeTunniste(koe.getTunniste());
                    container.setValintakoeOid(koe.getOid());

                    if (!valintakoeData.containsKey(koe.getTunniste())) {
                        valintakoeData.put(koe.getTunniste(), new ArrayList<HakukohdeValintakoeData>());
                    }

                    valintakoeData.get(koe.getTunniste()).add(container);
                }
            }
        }

        Map<String, ValintakoeOsallistuminen> osallistumisetByHaku = new HashMap<String, ValintakoeOsallistuminen>();

        for (Map.Entry<String, List<HakukohdeValintakoeData>> entry : valintakoeData.entrySet()) {
            List<HakukohdeValintakoeData> kokeet = entry.getValue();

            Collections.sort(kokeet, new Comparator<HakukohdeValintakoeData>() {
                @Override
                public int compare(HakukohdeValintakoeData o1, HakukohdeValintakoeData o2) {
                    return hakutoiveetByOid.get(o1.getHakukohdeOid()).getPrioriteetti() - hakutoiveetByOid
                            .get(o2.getHakukohdeOid()).getPrioriteetti();
                }
            });

            boolean osallistuminenLoydetty = false;
            for (HakukohdeValintakoeData c : kokeet) {
                if (!osallistuminenLoydetty && Osallistuminen.OSALLISTUU.equals(c.getOsallistuminen())) {
                    osallistuminenLoydetty = true;
                    continue;
                }

                c.setOsallistuminen(Osallistuminen.EI_OSALLISTU);
            }

            for (HakukohdeValintakoeData c : kokeet) {
                if (!osallistumisetByHaku.containsKey(c.getHakuOid())) {
                    osallistumisetByHaku.put(c.getHakuOid(), luoValintakoeOsallistuminen(c, hakemus));
                }

                ValintakoeOsallistuminen osallistuminen = osallistumisetByHaku.get(c.getHakuOid());
                haeTaiLuoHakutoive(osallistuminen, c);
            }
        }

        for(ValintakoeOsallistuminen osallistuminen : osallistumisetByHaku.values()) {
            valintakoeOsallistuminenDAO.createOrUpdate(osallistuminen);
        }
    }

    protected ValintakoeOsallistuminen luoValintakoeOsallistuminen(HakukohdeValintakoeData data, HakemusTyyppi hakemus) {
        ValintakoeOsallistuminen osallistuminen =
                valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(data.getHakuOid(),
                        hakemus.getHakemusOid());

        if (osallistuminen == null) {
            osallistuminen = new ValintakoeOsallistuminen();
            osallistuminen.setHakemusOid(hakemus.getHakemusOid());
            osallistuminen.setHakijaOid(hakemus.getHakijaOid());
            osallistuminen.setHakuOid(data.getHakuOid());
        }

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
            toive.setHakukohdeOid(data.getHakukohdeOid());
            osallistuminen.getHakutoiveet().add(toive);
        }

        haeTaiLuoValinnanVaihe(toive, data);
    }

    protected void haeTaiLuoValinnanVaihe(Hakutoive hakutoive, HakukohdeValintakoeData data) {
        ValinnanVaihe vaihe = null;

        for (ValinnanVaihe v : hakutoive.getValinnanVaiheet()) {
            if (data.getValinnanVaiheOid().equals(v.getValinnanVaiheOid())) {
                vaihe = v;
                break;
            }
        }

        if (vaihe == null) {
            vaihe = new ValinnanVaihe();
            vaihe.setValinnanVaiheOid(data.getValinnanVaiheOid());
            vaihe.setValinnanVaiheJarjestysluku(data.getValinnanVaiheJarjestysNro());
            hakutoive.getValinnanVaiheet().add(vaihe);
        }

        haeTaiLuoValintakoe(vaihe, data);
    }

    protected void haeTaiLuoValintakoe(ValinnanVaihe valinnanVaihe, HakukohdeValintakoeData data) {
        Valintakoe koe = null;
        for (Valintakoe k : valinnanVaihe.getValintakokeet()) {
            if (data.getValintakoeTunniste().equals(k.getValintakoeTunniste())) {
                koe = k;
                break;
            }
        }

        if (koe == null) {
            koe = new Valintakoe();
            koe.setOsallistuminen(data.getOsallistuminen());
            koe.setValintakoeOid(data.getValintakoeOid());
            koe.setValintakoeTunniste(data.getValintakoeTunniste());
            valinnanVaihe.getValintakokeet().add(koe);
        }
    }


    protected Map<String, HakukohdeTyyppi> luoHakutoiveMap(List<HakukohdeTyyppi> hakutoiveet) {
        Map<String, HakukohdeTyyppi> toiveetMap = new HashMap<String, HakukohdeTyyppi>();
        for (HakukohdeTyyppi hk : hakutoiveet) {
            toiveetMap.put(hk.getHakukohdeOid(), hk);
        }

        return toiveetMap;
    }

    protected boolean laskeKaava(String hakukohdeOid, HakemusTyyppi hakemus, Funktiokutsu funktiokutsu) {
        switch (funktiokutsu.getFunktionimi().getTyyppi()) {
            case TOTUUSARVOFUNKTIO:
                Laskentatulos<Boolean> tulos = laskentaService.suoritaLasku(hakukohdeOid,
                        hakemusConverter.convert(hakemus),
                        Laskentadomainkonvertteri.muodostaTotuusarvolasku(funktiokutsu));

                // Jos tulosta ei ole saatu laskettua (ts. sitä ei ole) tai jos tuloksen tila on hylätty, voidaan
                // olettaa, että henkilön pitää osallistua valintakokeeseen
                if (tulos.getTulos() == null || Tila.Tilatyyppi.HYLATTY.equals(tulos.getTila().getTilatyyppi())) {
                    return true;
                } else {
                    // muussa tapauksessa palautetaan laskettu tulos
                    return tulos.getTulos();
                }

            default:
                throw new LaskentaVaarantyyppisellaFunktiollaException("Palvelu hyväksyy vain totuusarvofunktioita!");
        }
    }
}