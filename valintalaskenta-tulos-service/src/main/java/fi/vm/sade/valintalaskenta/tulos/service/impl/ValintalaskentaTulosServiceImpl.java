package fi.vm.sade.valintalaskenta.tulos.service.impl;

import com.google.common.base.Predicate;
import com.google.common.collect.Collections2;
import fi.vm.sade.security.service.authz.util.AuthorizationUtil;
import fi.vm.sade.valintalaskenta.domain.dto.*;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.HakutoiveDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeOsallistuminenDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.valinta.*;
import fi.vm.sade.valintalaskenta.domain.valintakoe.*;
import fi.vm.sade.valintalaskenta.tulos.dao.*;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import fi.vm.sade.valintalaskenta.tulos.service.impl.converters.ValintatulosConverter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.*;

/**
 * @author Jussi Jartamo
 */
@Service
public class ValintalaskentaTulosServiceImpl implements ValintalaskentaTulosService {

    private static final Logger LOGGER = LoggerFactory.getLogger(ValintalaskentaTulosServiceImpl.class);


    @Autowired
    private ValinnanvaiheDAO valinnanvaiheDAO;

    @Autowired
    private ValintakoeOsallistuminenDAO valintakoeOsallistuminenDAO;

    @Autowired
    private MuokattuJonosijaDAO muokattuJonosijaDAO;

    @Autowired
    private ValintatulosConverter valintatulosConverter;

    @Autowired
    private JarjestyskriteerihistoriaDAO jarjestyskriteerihistoriaDAO;

    @Autowired
    private HarkinnanvarainenHyvaksyminenDAO harkinnanvarainenHyvaksyminenDAO;

    @Autowired
    private ValintalaskentaModelMapper modelMapper;


    private Map<String, MuokattuJonosija> muokatutJonosijatJonoOidinMukaan(List<MuokattuJonosija> muokatutJonosijat) {
        Map<String, MuokattuJonosija> map = new HashMap<String, MuokattuJonosija>();
        for (MuokattuJonosija jonosija : muokatutJonosijat) {
            map.put(jonosija.getValintatapajonoOid(), jonosija);
        }

        return map;
    }

    public HakemusDTO haeTuloksetHakemukselle(final String hakuOid, final String hakemusOid) {
        List<Valinnanvaihe> valinnanVaiheet = valinnanvaiheDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid);
        Map<String, HakukohdeDTO> hakukohdeDTOtOidinMukaan = new HashMap<String, HakukohdeDTO>();
        List<MuokattuJonosija> muokatutJonosijat = muokattuJonosijaDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid);
        List<HarkinnanvarainenHyvaksyminen> harkinnanvaraiset = harkinnanvarainenHyvaksyminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid);
        ValintakoeOsallistuminenDTO kokeet = modelMapper.map(haeValintakoeOsallistumiset(hakemusOid), ValintakoeOsallistuminenDTO.class);

        for (Valinnanvaihe vv : valinnanVaiheet) {
            HakukohdeDTO hakukohdeDTO = null;

            if (hakukohdeDTOtOidinMukaan.containsKey(vv.getHakukohdeOid())) {
                hakukohdeDTO = hakukohdeDTOtOidinMukaan.get(vv.getHakukohdeOid());
            } else {
                hakukohdeDTO = new HakukohdeDTO();
                hakukohdeDTO.setHakuoid(vv.getHakuOid());
                hakukohdeDTO.setOid(vv.getHakukohdeOid());
                hakukohdeDTO.setTarjoajaoid(vv.getTarjoajaOid());
                hakukohdeDTOtOidinMukaan.put(vv.getHakukohdeOid(), hakukohdeDTO);
            }

            ValinnanvaiheDTO vvdto = new ValinnanvaiheDTO();
            vvdto.setCreatedAt(vv.getCreatedAt());
            vvdto.setJarjestysnumero(vv.getJarjestysnumero());
            vvdto.setValinnanvaiheoid(vv.getValinnanvaiheOid());
            for (Valintatapajono jono : vv.getValintatapajonot()) {
                jono.setJonosijat(new ArrayList<Jonosija>(Collections2.filter(jono.getJonosijat(), new Predicate<Jonosija>() {
                    @Override
                    public boolean apply(Jonosija jonosija) {
                        return hakemusOid.equals(jonosija.getHakemusOid());
                    }
                })));

                vvdto.getValintatapajonot().add(valintatulosConverter.convertValintatapajono(jono));
            }
            hakukohdeDTO.getValinnanvaihe().add(vvdto);
        }

        // Valintakokeet
        for(HakutoiveDTO toive : kokeet.getHakutoiveet()) {
            HakukohdeDTO hakukohdeDTO = null;
            if (hakukohdeDTOtOidinMukaan.containsKey(toive.getHakukohdeOid())) {
                hakukohdeDTO = hakukohdeDTOtOidinMukaan.get(toive.getHakukohdeOid());
            } else {
                hakukohdeDTO = new HakukohdeDTO();
                hakukohdeDTO.setHakuoid(kokeet.getHakuOid());
                hakukohdeDTO.setOid(toive.getHakukohdeOid());
                hakukohdeDTOtOidinMukaan.put(toive.getHakukohdeOid(), hakukohdeDTO);
            }

            for (ValintakoeValinnanvaiheDTO vv : toive.getValinnanVaiheet()) {
                ValinnanvaiheDTO vvdto = new ValinnanvaiheDTO();
                vvdto.setCreatedAt(kokeet.getCreatedAt());
                vvdto.setJarjestysnumero(vv.getValinnanVaiheJarjestysluku());
                vvdto.setValinnanvaiheoid(vv.getValinnanVaiheOid());
                vvdto.getValintakokeet().addAll(vv.getValintakokeet());
                hakukohdeDTO.getValinnanvaihe().add(vvdto);
            }

        }

        for (HakukohdeDTO hk : hakukohdeDTOtOidinMukaan.values()) {
            applyMuokatutJonosijat(hk.getOid(), hk.getValinnanvaihe(), muokatutJonosijat, harkinnanvaraiset);
        }

        return new HakemusDTO(hakuOid, hakemusOid, new ArrayList<HakukohdeDTO>(hakukohdeDTOtOidinMukaan.values()));
    }

    private void applyMuokatutJonosijatToValinnanvaihe(String hakukohdeoid, List<ValinnanvaiheDTO> b) {
        List<MuokattuJonosija> a = muokattuJonosijaDAO.readByhakukohdeOid(hakukohdeoid);
        List<HarkinnanvarainenHyvaksyminen> c = harkinnanvarainenHyvaksyminenDAO.haeHarkinnanvarainenHyvaksyminen(hakukohdeoid);
        applyMuokatutJonosijat(hakukohdeoid, b, a, c);
    }

    private void applyMuokatutJonosijatToHakukohde(String hakuOid, List<HakukohdeDTO> b) {
        LOGGER.error("Haetaan muokatut jonosijat {}!", hakuOid);
        List<MuokattuJonosija> a = muokattuJonosijaDAO.readByHakuOid(hakuOid);
        LOGGER.error("Muokatut jonosijat haettu, haetaan harkinnanvaraiset {}!", hakuOid);
        List<HarkinnanvarainenHyvaksyminen> c = harkinnanvarainenHyvaksyminenDAO.haeHarkinnanvaraisetHyvaksymisetHaulle(hakuOid);
        LOGGER.error("Harkinnavaraiset haettu, loopataan hakukohteet {}!", hakuOid);
        for (HakukohdeDTO hakukohde : b) {
            LOGGER.error("Laitetaan jonosijat hakukohteelle {}!", hakukohde.getOid());
            applyMuokatutJonosijat(hakukohde.getOid(), hakukohde.getValinnanvaihe(), a, c);
            LOGGER.error("Laitettu jonosijat hakukohteelle {}!", hakukohde.getOid());
        }
    }

    private void applyMuokatutJonosijat(String hakukohdeoid, List<ValinnanvaiheDTO> b, List<MuokattuJonosija> a, List<HarkinnanvarainenHyvaksyminen> c) {
        for (ValinnanvaiheDTO dto : b) {
            for (ValintatapajonoDTO valintatapajonoDTO : dto.getValintatapajonot()) {
                for (JonosijaDTO jonosija : valintatapajonoDTO.getJonosijat()) {
                    for (MuokattuJonosija muokattuJonosija : a) {
                        if (muokattuJonosija.getHakemusOid().equals(jonosija.getHakemusOid())
                                && valintatapajonoDTO.getOid().equals(muokattuJonosija.getValintatapajonoOid())) {
                            applyJonosija(jonosija, muokattuJonosija);
                        }
                    }
                    for (HarkinnanvarainenHyvaksyminen harkinnanvarainenHyvaksyminen : c) {
                        if (harkinnanvarainenHyvaksyminen.getHakemusOid().equals(jonosija.getHakemusOid()) &&
                                harkinnanvarainenHyvaksyminen.getHakukohdeOid().equals(hakukohdeoid)) {
                            applyHarkinnanvarainenHyvaksynta(jonosija, harkinnanvarainenHyvaksyminen);
                        }
                    }
                }
                valintatulosConverter.sort(valintatapajonoDTO.getJonosijat());
            }
        }
    }

    private void applyHarkinnanvarainenHyvaksynta(JonosijaDTO jonosija, HarkinnanvarainenHyvaksyminen hyvaksyminen) {
        if (hyvaksyminen.getHarkinnanvaraisuusTila() == HarkinnanvaraisuusTila.HYVAKSYTTY) {
            if (jonosija.getJarjestyskriteerit().first() != null) {
                jonosija.getJarjestyskriteerit().first().setTila(JarjestyskriteerituloksenTila.HYVAKSYTTY_HARKINNANVARAISESTI);
            }
            jonosija.setTuloksenTila(JarjestyskriteerituloksenTila.HYVAKSYTTY_HARKINNANVARAISESTI);
        }
    }

    private void applyJonosija(JonosijaDTO jonosijaDTO, MuokattuJonosija muokattuJonosija) {
        boolean jonosijaMuokattu = false;

        if (muokattuJonosija.getPrioriteetti() != null) {
            jonosijaDTO.setPrioriteetti(muokattuJonosija.getPrioriteetti());
            jonosijaMuokattu = true;
        }

        Map<Integer, Jarjestyskriteeritulos> muokatunJonosijanKriteerit = jarjestyskriteeritPrioriteetitMukaan(muokattuJonosija.getJarjestyskriteerit());
        Map<Integer, JarjestyskriteeritulosDTO> jonosijanKriteerit = jarjestyskriteeriDtotPrioriteetitMukaan(jonosijaDTO.getJarjestyskriteerit());

        for (Integer i : muokatunJonosijanKriteerit.keySet()) {
            Jarjestyskriteeritulos muokattuJarjestyskriteeritulos = muokatunJonosijanKriteerit.get(i);
            JarjestyskriteeritulosDTO alkuperainenJarjestyskriteeritulosDTO = jonosijanKriteerit.get(i);

            if (alkuperainenJarjestyskriteeritulosDTO == null) {
                alkuperainenJarjestyskriteeritulosDTO = new JarjestyskriteeritulosDTO();
                alkuperainenJarjestyskriteeritulosDTO.setPrioriteetti(i);
                jonosijaDTO.getJarjestyskriteerit().add(alkuperainenJarjestyskriteeritulosDTO);
            }
            if (muokattuJarjestyskriteeritulos.getArvo() != null) {
                alkuperainenJarjestyskriteeritulosDTO.setArvo(muokattuJarjestyskriteeritulos.getArvo());
                jonosijaMuokattu = true;
            }
            if (muokattuJarjestyskriteeritulos.getKuvaus() != null) {
                alkuperainenJarjestyskriteeritulosDTO.setKuvaus(muokattuJarjestyskriteeritulos.getKuvaus());
                jonosijaMuokattu = true;
            }
            if (muokattuJarjestyskriteeritulos.getTila() != null) {
                alkuperainenJarjestyskriteeritulosDTO.setTila(muokattuJarjestyskriteeritulos.getTila());
                jonosijaMuokattu = true;
            }
        }
        if (jonosijaMuokattu) {
            jonosijaDTO.setMuokattu(true);
        }
    }


    private Map<Integer, Jarjestyskriteeritulos> jarjestyskriteeritPrioriteetitMukaan(Collection<Jarjestyskriteeritulos> kriteerit) {
        Map<Integer, Jarjestyskriteeritulos> map = new HashMap<Integer, Jarjestyskriteeritulos>();

        for (Jarjestyskriteeritulos jktulos : kriteerit) {
            map.put(jktulos.getPrioriteetti(), jktulos);
        }

        return map;
    }

    private Map<Integer, JarjestyskriteeritulosDTO> jarjestyskriteeriDtotPrioriteetitMukaan(Collection<JarjestyskriteeritulosDTO> kriteerit) {
        Map<Integer, JarjestyskriteeritulosDTO> map = new HashMap<Integer, JarjestyskriteeritulosDTO>();

        for (JarjestyskriteeritulosDTO dto : kriteerit) {
            map.put(dto.getPrioriteetti(), dto);
        }

        return map;
    }


    @Override
    public List<ValinnanvaiheDTO> haeValinnanvaiheetHakukohteelle(String hakukohdeoid) {

        List<Valinnanvaihe> a = valinnanvaiheDAO.readByHakukohdeOid(hakukohdeoid);
        List<ValinnanvaiheDTO> b = valintatulosConverter.convertValinnanvaiheList(a);
        applyMuokatutJonosijatToValinnanvaihe(hakukohdeoid, b);
        return b;

    }

    @Override
    public List<ValintakoeOsallistuminenDTO> haeValintakoevirheetHaulle(String hakuOid) {
        final Osallistuminen virhe = Osallistuminen.VIRHE;
        List<ValintakoeOsallistuminen> osallistumiset = valintakoeOsallistuminenDAO.findByHakuAndOsallistuminen(hakuOid, virhe);

        Iterator<ValintakoeOsallistuminen> i = osallistumiset.iterator();
        while (i.hasNext()) {
            ValintakoeOsallistuminen vko = i.next();

            Iterator<Hakutoive> j = vko.getHakutoiveet().iterator();
            while (j.hasNext()) {
                Hakutoive ht = j.next();
                Iterator<ValintakoeValinnanvaihe> k = ht.getValinnanVaiheet().iterator();
                while (k.hasNext()) {
                    ValintakoeValinnanvaihe vv = k.next();
                    Iterator<Valintakoe> l = vv.getValintakokeet().iterator();
                    while (l.hasNext()) {
                        Valintakoe vk = l.next();

                        if (!Osallistuminen.VIRHE.equals(vk.getOsallistuminenTulos().getOsallistuminen())) {
                            l.remove();
                        }
                    }

                    if (vv.getValintakokeet().isEmpty()) {
                        k.remove();
                    }
                }

                if (ht.getValinnanVaiheet().isEmpty()) {
                    j.remove();
                }
            }

            if (vko.getHakutoiveet().isEmpty()) {
                i.remove();
            }
        }

        return valintatulosConverter.convertValintakoeOsallistuminen(osallistumiset);
    }


    @Override
    public List<HakukohdeDTO> haeVirheetHaulle(String hakuOid) {

        // FIXME: Suora mongo kysely tälle.
        List<Valinnanvaihe> valinnanvaiheet = valinnanvaiheDAO.readByHakuOid(hakuOid);

        Iterator<Valinnanvaihe> i = valinnanvaiheet.iterator();
        while (i.hasNext()) {
            Valinnanvaihe vv = i.next();
            Iterator<Valintatapajono> j = vv.getValintatapajonot().iterator();
            while (j.hasNext()) {
                Valintatapajono jono = j.next();
                Iterator<Jonosija> k = jono.getJonosijat().iterator();
                while (k.hasNext()) {
                    Jonosija jonosija = k.next();
                    Iterator<Jarjestyskriteeritulos> l = jonosija.getJarjestyskriteeritulokset().iterator();
                    while (l.hasNext()) {
                        Jarjestyskriteeritulos jktulos = l.next();

                        if (!JarjestyskriteerituloksenTila.VIRHE.equals(jktulos.getTila())) {
                            l.remove();
                        }
                    }

                    if (jonosija.getJarjestyskriteeritulokset().isEmpty()) {
                        k.remove();
                    }
                }

                if (jono.getJonosijat().isEmpty()) {
                    j.remove();
                }
            }

            if (vv.getValintatapajonot().isEmpty()) {
                i.remove();
            }
        }

        Map<String, HakukohdeDTO> hakukohdeDTOtOidinMukaan = new HashMap<String, HakukohdeDTO>();

        for (Valinnanvaihe vv : valinnanvaiheet) {
            HakukohdeDTO hakukohdeDTO = null;

            if (hakukohdeDTOtOidinMukaan.containsKey(vv.getHakukohdeOid())) {
                hakukohdeDTO = hakukohdeDTOtOidinMukaan.get(vv.getHakukohdeOid());
            } else {
                hakukohdeDTO = new HakukohdeDTO();
                hakukohdeDTO.setHakuoid(vv.getHakuOid());
                hakukohdeDTO.setOid(vv.getHakukohdeOid());
                hakukohdeDTO.setTarjoajaoid(vv.getTarjoajaOid());
                hakukohdeDTOtOidinMukaan.put(vv.getHakukohdeOid(), hakukohdeDTO);
            }

            hakukohdeDTO.getValinnanvaihe().add(valintatulosConverter.convertValinnanvaihe(vv));

        }

        return new ArrayList<HakukohdeDTO>(hakukohdeDTOtOidinMukaan.values());
    }

    @Override
    public List<HakukohdeDTO> haeLasketutValinnanvaiheetHaulle(String hakuOid) {
        LOGGER.error("Valintatietoja haetaan mongosta {}!", hakuOid);
        List<Valinnanvaihe> a = valinnanvaiheDAO.readByHakuOid(hakuOid);
        LOGGER.error("Valintatietoja haettu mongosta {}!", hakuOid);
        List<HakukohdeDTO> b = valintatulosConverter.convertValinnanvaihe(a);
        LOGGER.error("Valintatiedot kovertoitu DTO:iksi {}!", hakuOid);
        applyMuokatutJonosijatToHakukohde(hakuOid, b);
        LOGGER.error("Muokatut jonosijat liitetty {}!", hakuOid);
        return b;
    }

    @Override
    public ValintakoeOsallistuminen haeValintakoeOsallistumiset(String hakemusOid) {
        ValintakoeOsallistuminen byHakemusOid = valintakoeOsallistuminenDAO.findByHakemusOid(hakemusOid);
        if(byHakemusOid == null) {
            byHakemusOid = new ValintakoeOsallistuminen();
        }
        return byHakemusOid;
    }

    @Override
    public List<ValintakoeOsallistuminen> haeValintakoeOsallistumisetByHakutoive(String hakukohdeOid) {
        return valintakoeOsallistuminenDAO.findByHakutoive(hakukohdeOid);
    }

    @Override
    public List<Jarjestyskriteerihistoria> haeJonosijaHistoria(String valintatapajonoOid, String hakemusOid) {
        return jarjestyskriteerihistoriaDAO.findByValintatapajonoAndVersioAndHakemusOid(valintatapajonoOid, hakemusOid);
    }


    @Override
    public void asetaHarkinnanvaraisestiHyvaksymisenTila(String hakuoid, String hakukohdeoid, String hakemusoid, HarkinnanvaraisuusTila tila) {
        HarkinnanvarainenHyvaksyminen a = harkinnanvarainenHyvaksyminenDAO.haeHarkinnanvarainenHyvaksyminen(hakukohdeoid, hakemusoid);
        if (a == null) {
            a = new HarkinnanvarainenHyvaksyminen();
            a.setHakemusOid(hakemusoid);
            a.setHakukohdeOid(hakukohdeoid);
            a.setHakuOid(hakuoid);
        }
        a.setHarkinnanvaraisuusTila(tila);
        harkinnanvarainenHyvaksyminenDAO.tallennaHarkinnanvarainenHyvaksyminen(a);
    }

    @Override
    public List<HarkinnanvarainenHyvaksyminen> haeHarkinnanvaraisestiHyvaksymisenTila(String hakukohdeoid) {
        return harkinnanvarainenHyvaksyminenDAO.haeHarkinnanvarainenHyvaksyminen(hakukohdeoid);
    }

    @Override
    public List<HarkinnanvarainenHyvaksyminen> haeHakemuksenHarkinnanvaraisestiHyvaksymisenTilat(String hakuOid, String hakukohdeoid) {
        return harkinnanvarainenHyvaksyminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakukohdeoid);
    }

    @Override
    public ValinnanvaiheDTO lisaaTuloksia(ValinnanvaiheDTO vaihe, String hakukohdeoid) {
        Valinnanvaihe haettu = valinnanvaiheDAO.haeValinnanvaihe(vaihe.getValinnanvaiheoid());
        Valinnanvaihe annettu = modelMapper.map(vaihe, Valinnanvaihe.class);

        annettu.setHakukohdeOid(hakukohdeoid);
        if(haettu == null) {
            valinnanvaiheDAO.create(annettu);
        } else {
            List<Valintatapajono> vanhat = new ArrayList<Valintatapajono>();
            for (Valintatapajono jono : haettu.getValintatapajonot()) {
                boolean mukana = false;
                for (Valintatapajono uusi : annettu.getValintatapajonot()) {
                    if(uusi.getValintatapajonoOid().equals(jono.getValintatapajonoOid())) {
                        mukana = true;
                    }
                }
                if(!mukana) {
                    vanhat.add(jono);
                }
            }
            annettu.getValintatapajonot().addAll(vanhat);
            valinnanvaiheDAO.update(haettu, annettu.getValintatapajonot(), hakukohdeoid, vaihe.getHakuOid());
        }
        return vaihe;
    }


    /**
     * Muokattu jonosija works in mysterious ways.
     *
     * @param valintatapajonoOid
     * @param hakemusOid
     * @param jarjestyskriteeriPrioriteetti
     * @param jonosija
     * @param selite
     * @return
     */
    @Override
    public MuokattuJonosija muutaJarjestyskriteeri(String valintatapajonoOid, String hakemusOid,
                                                   Integer jarjestyskriteeriPrioriteetti, MuokattuJonosijaArvoDTO jonosija) {

        Valinnanvaihe valinnanvaihe = valinnanvaiheDAO.findByValintatapajonoOid(valintatapajonoOid);

        MuokattuJonosija muokattuJonosija;
        muokattuJonosija = muokattuJonosijaDAO.readByValintatapajonoOid(valintatapajonoOid, hakemusOid);
        if (muokattuJonosija == null) {
            muokattuJonosija = new MuokattuJonosija();
        }

        muokattuJonosija.setHakemusOid(hakemusOid);
        muokattuJonosija.setValintatapajonoOid(valintatapajonoOid);
        muokattuJonosija.setHakuOid(valinnanvaihe.getHakuOid());
        muokattuJonosija.setHakukohdeOid(valinnanvaihe.getHakukohdeOid());

        Jarjestyskriteeritulos jarjestyskriteeritulos = null;

        for (Jarjestyskriteeritulos tulos : muokattuJonosija.getJarjestyskriteerit()) {
            if (tulos.getPrioriteetti() == jarjestyskriteeriPrioriteetti) {
                jarjestyskriteeritulos = tulos;
            }
        }

        if (jarjestyskriteeritulos == null) {
            jarjestyskriteeritulos = new Jarjestyskriteeritulos();
            jarjestyskriteeritulos.setPrioriteetti(jarjestyskriteeriPrioriteetti);
            muokattuJonosija.getJarjestyskriteerit().add(jarjestyskriteeritulos);
        }

        Map<String,String> muokattuKasin = new HashMap<String,String>();
        muokattuKasin.put("FI", "Muokattu käsin");

        jarjestyskriteeritulos.setKuvaus(muokattuKasin);
        jarjestyskriteeritulos.setArvo(jonosija.getArvo());
        jarjestyskriteeritulos.setTila(jonosija.getTila());

        addLogEntry(jonosija.getSelite(), muokattuJonosija, "jarjestyskriteeriPrioriteetti: " + jarjestyskriteeriPrioriteetti
                + " arvo: " + jonosija.getArvo() + " tila: " + jonosija.getTila().name());

        muokattuJonosijaDAO.saveOrUpdate(muokattuJonosija);

        return muokattuJonosija;
    }

    private void addLogEntry(String selite, MuokattuJonosija muokattuJonosija, String muutos) {
        LogEntry logEntry = new LogEntry();

        logEntry.setLuotu(new Date());
        logEntry.setMuokkaaja(AuthorizationUtil.getCurrentUser());
        logEntry.setSelite(selite);
        logEntry.setMuutos(muutos);

        muokattuJonosija.getLogEntries().add(logEntry);
    }

}
