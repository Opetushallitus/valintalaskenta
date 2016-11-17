package fi.vm.sade.valintalaskenta.tulos.service.impl;

import com.google.common.collect.Collections2;
import fi.vm.sade.valintalaskenta.domain.dto.*;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.HakutoiveDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeOsallistuminenDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.ValintatietoValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.ValintatietoValintatapajonoDTO;
import fi.vm.sade.valintalaskenta.domain.valinta.*;
import fi.vm.sade.valintalaskenta.domain.valintakoe.*;
import fi.vm.sade.valintalaskenta.tulos.dao.*;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import fi.vm.sade.valintalaskenta.tulos.service.AuthorizationUtil;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import fi.vm.sade.valintalaskenta.tulos.service.exception.EiOikeuttaPoistaaValintatapajonoaSijoittelustaException;
import fi.vm.sade.valintalaskenta.tulos.service.impl.converters.ValintatulosConverter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.function.Consumer;
import java.util.stream.Collectors;

@Service
public class ValintalaskentaTulosServiceImpl implements ValintalaskentaTulosService {
    private static final Logger LOGGER = LoggerFactory.getLogger(ValintalaskentaTulosServiceImpl.class);

    @Autowired
    private ValinnanvaiheDAO valinnanvaiheDAO;

    @Autowired
    private HakijaryhmaDAO hakijaryhmaDAO;

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

    @Value("${root.organisaatio.oid}")
    private String rootOrgOid;

    public HakemusDTO haeTuloksetHakemukselle(final String hakuOid, final String hakemusOid) {
        List<Valinnanvaihe> valinnanVaiheet = valinnanvaiheDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid);
        Map<String, HakukohdeDTO> hakukohdeDTOtOidinMukaan = new HashMap<>();
        List<MuokattuJonosija> muokatutJonosijat = muokattuJonosijaDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid);
        List<HarkinnanvarainenHyvaksyminen> harkinnanvaraiset = harkinnanvarainenHyvaksyminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid);
        ValintakoeOsallistuminenDTO kokeet = modelMapper.map(haeValintakoeOsallistumiset(hakemusOid), ValintakoeOsallistuminenDTO.class);

        for (Valinnanvaihe vv : valinnanVaiheet) {
            HakukohdeDTO hakukohdeDTO = getOrCreateHakukohdeDTO(hakukohdeDTOtOidinMukaan, vv);
            ValintatietoValinnanvaiheDTO vvdto = createValintatietoValinnanvaiheDTO(hakuOid, vv.getCreatedAt(), vv.getValinnanvaiheOid(), vv.getJarjestysnumero());
            for (Valintatapajono jono : vv.getValintatapajonot()) {
                jono.setJonosijat(new ArrayList<>(
                                Collections2.filter(jono.getJonosijat(), jonosija -> hakemusOid.equals(jonosija.getHakemusOid())))
                );
                vvdto.getValintatapajonot().add(valintatulosConverter.convertValintatapajono(jono));
            }
            hakukohdeDTO.getValinnanvaihe().add(vvdto);
        }

        // Valintakokeet
        for (HakutoiveDTO toive : kokeet.getHakutoiveet()) {
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
                ValintatietoValinnanvaiheDTO vvdto = createValintatietoValinnanvaiheDTO(hakuOid, kokeet.getCreatedAt(), vv.getValinnanVaiheOid(), vv.getValinnanVaiheJarjestysluku());
                vvdto.getValintakokeet().addAll(vv.getValintakokeet());
                hakukohdeDTO.getValinnanvaihe().add(vvdto);
            }
        }
        for (HakukohdeDTO hk : hakukohdeDTOtOidinMukaan.values()) {
            applyMuokatutJonosijat(hk.getOid(), hk.getValinnanvaihe(), muokatutJonosijat, harkinnanvaraiset);
        }
        return new HakemusDTO(hakuOid, hakemusOid, new ArrayList<>(hakukohdeDTOtOidinMukaan.values()));
    }

    private ValintatietoValinnanvaiheDTO createValintatietoValinnanvaiheDTO(String hakuOid, Date createdAt, String valinnanvaiheOid, int jarjestysnumero) {
        ValintatietoValinnanvaiheDTO vvdto = new ValintatietoValinnanvaiheDTO();
        vvdto.setHakuOid(hakuOid);
        vvdto.setCreatedAt(createdAt);
        vvdto.setValinnanvaiheoid(valinnanvaiheOid);
        vvdto.setJarjestysnumero(jarjestysnumero);
        return vvdto;
    }

    private void applyMuokatutJonosijatToValinnanvaihe(String hakukohdeoid,
                                                       List<ValintatietoValinnanvaiheDTO> b) {
        List<MuokattuJonosija> a = muokattuJonosijaDAO.readByhakukohdeOid(hakukohdeoid);
        List<HarkinnanvarainenHyvaksyminen> c = harkinnanvarainenHyvaksyminenDAO.haeHarkinnanvarainenHyvaksyminen(hakukohdeoid);
        applyMuokatutJonosijat(hakukohdeoid, b, a, c);
    }

    private void applyMuokatutJonosijatToHakukohde(String hakuOid, List<HakukohdeDTO> b) {
        LOGGER.info("Haetaan muokatut jonosijat {}!", hakuOid);
        List<MuokattuJonosija> a = muokattuJonosijaDAO.readByHakuOid(hakuOid);
        LOGGER.info("Muokatut jonosijat haettu, haetaan harkinnanvaraiset {}!", hakuOid);
        List<HarkinnanvarainenHyvaksyminen> c = harkinnanvarainenHyvaksyminenDAO.haeHarkinnanvaraisetHyvaksymisetHaulle(hakuOid);
        LOGGER.info("Harkinnavaraiset haettu, loopataan hakukohteet {} - yhteensä {}", hakuOid, b.size());
        for (int i = 0; i < b.size(); i++) {
            HakukohdeDTO hakukohde = b.get(i);
            LOGGER.debug("Laitetaan jonosijat hakukohteelle {} - indeksi {}", hakukohde.getOid(), i);
            applyMuokatutJonosijat(hakukohde.getOid(), hakukohde.getValinnanvaihe(), a, c);
        }
        LOGGER.info("Muokatut jonosijat laitettu kaikille hakukohteille {}!", hakuOid);
    }

    private void applyMuokatutJonosijat(String hakukohdeoid,
                                        List<ValintatietoValinnanvaiheDTO> b, List<MuokattuJonosija> a,
                                        List<HarkinnanvarainenHyvaksyminen> c) {
        for (ValintatietoValinnanvaiheDTO dto : b) {
            for (ValintatietoValintatapajonoDTO valintatapajonoDTO : dto.getValintatapajonot()) {
                for (JonosijaDTO jonosija : valintatapajonoDTO.getJonosijat()) {
                    for (MuokattuJonosija muokattuJonosija : a) {
                        if (muokattuJonosija.getHakemusOid().equals(jonosija.getHakemusOid())
                                && valintatapajonoDTO.getOid().equals(muokattuJonosija.getValintatapajonoOid())) {
                            applyJonosija(jonosija, muokattuJonosija);
                        }
                    }
                    c.forEach(h -> {
                        if (h.getHakemusOid().equals(jonosija.getHakemusOid()) && h.getHakukohdeOid().equals(hakukohdeoid)) {
                            applyHarkinnanvarainenHyvaksynta(jonosija, h);
                        }
                    });
                }
                valintatulosConverter.sort(valintatapajonoDTO.getJonosijat());
            }
        }
    }

    private void applyHarkinnanvarainenHyvaksynta(JonosijaDTO jonosija, HarkinnanvarainenHyvaksyminen hyvaksyminen) {
        if (hyvaksyminen.getHarkinnanvaraisuusTila() == HarkinnanvaraisuusTila.HYVAKSYTTY) {
            JarjestyskriteeritulosDTO jarjestyskriteeritulos = jonosija.getJarjestyskriteerit().first();
            if (jarjestyskriteeritulos != null) {
                jarjestyskriteeritulos.setTila(JarjestyskriteerituloksenTila.HYVAKSYTTY_HARKINNANVARAISESTI);
                jarjestyskriteeritulos.setKuvaus(new HashMap<>());
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
        Map<Integer, Jarjestyskriteeritulos> map = new HashMap<>();
        for (Jarjestyskriteeritulos jktulos : kriteerit) {
            map.put(jktulos.getPrioriteetti(), jktulos);
        }
        return map;
    }

    private Map<Integer, JarjestyskriteeritulosDTO> jarjestyskriteeriDtotPrioriteetitMukaan(Collection<JarjestyskriteeritulosDTO> kriteerit) {
        Map<Integer, JarjestyskriteeritulosDTO> map = new HashMap<>();
        for (JarjestyskriteeritulosDTO dto : kriteerit) {
            map.put(dto.getPrioriteetti(), dto);
        }
        return map;
    }

    @Override
    public List<ValintatietoValinnanvaiheDTO> haeValinnanvaiheetHakukohteelle(String hakukohdeoid) {
        List<Valinnanvaihe> valinnanVaihes = valinnanvaiheDAO.readByHakukohdeOid(hakukohdeoid);
        List<ValintatietoValinnanvaiheDTO> valintatietoValinnanVaihes = valintatulosConverter.convertValinnanvaiheList(valinnanVaihes);
        applyMuokatutJonosijatToValinnanvaihe(hakukohdeoid, valintatietoValinnanVaihes);
        return valintatietoValinnanVaihes;
    }

    @Override
    public Optional<HakukohdeDTO> haeValinnanvaiheetHakukohteelleJaJonolle(String hakukohdeoid, List<String> valintatapajonot) {
        List<Valinnanvaihe> a = valinnanvaiheDAO.readByHakukohdeOid(hakukohdeoid);
        List<Valinnanvaihe> result = new ArrayList<>();
        a.forEach(vaihe -> {
            List<Valintatapajono> jonot = vaihe
                    .getValintatapajonot()
                    .stream()
                    .filter(j -> valintatapajonot.indexOf(j.getValintatapajonoOid()) != -1)
                    .collect(Collectors.toList());
            if (!jonot.isEmpty()) {
                vaihe.setValintatapajonot(jonot);
                result.add(vaihe);
            } else {
                LOGGER.warn("Yhtään jonoa ei löytynyt {}!", hakukohdeoid);
            }
        });

        if (result.isEmpty()) {
            LOGGER.warn("Yhtään valinnanvaihetta ei löytynyt {}!", hakukohdeoid);
            return Optional.empty();
        }
        List<HakukohdeDTO> b = valintatulosConverter.convertValinnanvaihe(result);
        applyMuokatutJonosijatToHakukohde(b.get(0).getHakuoid(), b);
        if (b.isEmpty()) {
            return Optional.empty();
        }
        return Optional.of(b.get(0));
    }

    @Override
    public List<HakijaryhmaDTO> haeHakijaryhmatHakukohteelle(String hakukohdeoid) {
        List<Hakijaryhma> hakijaryhmat = hakijaryhmaDAO.readByHakukohdeOid(hakukohdeoid);
        return modelMapper.mapList(hakijaryhmat, HakijaryhmaDTO.class);
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
        Map<String, HakukohdeDTO> hakukohdeDTOtOidinMukaan = new HashMap<>();
        for (Valinnanvaihe vv : valinnanvaiheet) {
            HakukohdeDTO hakukohdeDTO = getOrCreateHakukohdeDTO(hakukohdeDTOtOidinMukaan, vv);
            hakukohdeDTO.getValinnanvaihe().add(valintatulosConverter.convertValinnanvaihe(vv));
        }
        return new ArrayList<>(hakukohdeDTOtOidinMukaan.values());
    }

    private HakukohdeDTO getOrCreateHakukohdeDTO(Map<String, HakukohdeDTO> hakukohdeDTOtOidinMukaan, Valinnanvaihe vv) {
        if (hakukohdeDTOtOidinMukaan.containsKey(vv.getHakukohdeOid())) {
            return hakukohdeDTOtOidinMukaan.get(vv.getHakukohdeOid());
        } else {
            HakukohdeDTO hakukohdeDTO = new HakukohdeDTO();
            hakukohdeDTO.setHakuoid(vv.getHakuOid());
            hakukohdeDTO.setOid(vv.getHakukohdeOid());
            hakukohdeDTO.setTarjoajaoid(vv.getTarjoajaOid());
            hakukohdeDTOtOidinMukaan.put(vv.getHakukohdeOid(), hakukohdeDTO);
            return hakukohdeDTO;
        }
    }

    @Override
    public List<HakukohdeDTO> haeLasketutValinnanvaiheetHaulle(String hakuOid) {
        LOGGER.info("Valintatietoja haetaan mongosta {}!", hakuOid);
        List<Valinnanvaihe> a = valinnanvaiheDAO.readByHakuOid(hakuOid);
        LOGGER.info("Valintatietoja haettu mongosta {}!", hakuOid);
        List<HakukohdeDTO> b = valintatulosConverter.convertValinnanvaihe(a);
        LOGGER.info("Valintatiedot kovertoitu DTO:iksi {}!", hakuOid);
        applyMuokatutJonosijatToHakukohde(hakuOid, b);
        LOGGER.info("Muokatut jonosijat liitetty {}!", hakuOid);
        b.forEach(hakukohde -> hakukohde.getHakijaryhma().addAll(haeHakijaryhmatHakukohteelle(hakukohde.getOid())));
        return b;
    }

    @Override
    public ValintakoeOsallistuminen haeValintakoeOsallistumiset(String hakemusOid) {
        ValintakoeOsallistuminen byHakemusOid = valintakoeOsallistuminenDAO.findByHakemusOid(hakemusOid);
        if (byHakemusOid == null) {
            byHakemusOid = new ValintakoeOsallistuminen();
        }
        return byHakemusOid;
    }

    @Override
    public List<ValintakoeOsallistuminen> haeValintakoeOsallistumiset(Collection<String> hakemusOids) {
        return hakemusOids
                .stream()
                .map(hakemusOid -> valintakoeOsallistuminenDAO.findByHakemusOid(hakemusOid))
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
    }

    @Override
    public List<ValintakoeOsallistuminen> haeValintakoeOsallistumisetByHakukohdes(List<String> hakukohdeOids) {
        return valintakoeOsallistuminenDAO.findByHakutoiveet(hakukohdeOids)
                .stream()
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
    }

    @Override
    public List<ValintakoeOsallistuminen> haeValintakoeOsallistumisetByHakutoive(String hakukohdeOid) {
        return valintakoeOsallistuminenDAO.findByHakutoive(hakukohdeOid);
    }

    @Override
    public List<Jarjestyskriteerihistoria> haeJonosijaHistoria(String valintatapajonoOid, String hakemusOid) {
        return jarjestyskriteerihistoriaDAO.findByValintatapajonoAndHakemusOid(valintatapajonoOid, hakemusOid);
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
    public ValinnanvaiheDTO lisaaTuloksia(ValinnanvaiheDTO vaihe, String hakukohdeoid, String tarjoajaOid) {
        Valinnanvaihe haettu = valinnanvaiheDAO.haeValinnanvaihe(vaihe.getValinnanvaiheoid());
        Valinnanvaihe annettu = modelMapper.map(vaihe, Valinnanvaihe.class);
        annettu.setHakukohdeOid(hakukohdeoid);
        if (haettu == null) {
            valinnanvaiheDAO.saveOrUpdate(annettu);
        } else {
            List<Valintatapajono> vanhat = new ArrayList<Valintatapajono>();
            for (Valintatapajono jono : haettu.getValintatapajonot()) {
                boolean mukana = false;
                for (Valintatapajono uusi : annettu.getValintatapajonot()) {
                    if (uusi.getValintatapajonoOid().equals(jono.getValintatapajonoOid())) {
                        mukana = true;
                    }
                }
                if (!mukana) {
                    vanhat.add(jono);
                }
            }
            annettu.getValintatapajonot().addAll(vanhat);
            haettu.setHakukohdeOid(hakukohdeoid);
            haettu.setHakuOid(vaihe.getHakuOid());
            haettu.setTarjoajaOid(tarjoajaOid);
            haettu.setValintatapajonot(annettu.getValintatapajonot());
            valinnanvaiheDAO.saveOrUpdate(haettu);
        }
        return vaihe;
    }

    @Override
    public Optional<Valintatapajono> muokkaaValintatapajonoa(String valintatapajonoOid, Consumer<Valintatapajono> muokkausFunktio) {
        if (haeSijoitteluStatus(valintatapajonoOid) && !isOPH()) {
            throw new EiOikeuttaPoistaaValintatapajonoaSijoittelustaException("Ei oikeutta muokata valintatapajonoa");
        }
        Valinnanvaihe vaihe = valinnanvaiheDAO.findByValintatapajonoOid(valintatapajonoOid);
        vaihe.getValintatapajonot()
                .stream()
                .filter(j -> j.getValintatapajonoOid().equals(valintatapajonoOid))
                .forEach(j -> muokkausFunktio.accept(j));
        valinnanvaiheDAO.saveOrUpdate(vaihe);
        return vaihe
                .getValintatapajonot()
                .stream()
                .filter(j -> j.getValintatapajonoOid().equals(valintatapajonoOid)).findFirst();
    }

    @Override
    public boolean haeSijoitteluStatus(String valintatapajonoOid) {
        Valinnanvaihe vaihe = valinnanvaiheDAO.findByValintatapajonoOid(valintatapajonoOid);
        if (vaihe == null) {
            return false;
        }
        return vaihe.getValintatapajonot()
                .stream()
                .filter(j -> j.getValintatapajonoOid().equals(
                        valintatapajonoOid))
                .allMatch(Valintatapajono::getValmisSijoiteltavaksi);
    }

    /**
     * Muokattu jonosija works in mysterious ways.
     */
    @Override
    public MuokattuJonosija muutaJarjestyskriteeri(String valintatapajonoOid,
                                                   String hakemusOid, Integer jarjestyskriteeriPrioriteetti,
                                                   MuokattuJonosijaArvoDTO jonosija) {
        Valinnanvaihe valinnanvaihe = valinnanvaiheDAO.findByValintatapajonoOid(valintatapajonoOid);
        MuokattuJonosija muokattuJonosija = muokattuJonosijaDAO.readByValintatapajonoOid(valintatapajonoOid, hakemusOid);
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

        Map<String, String> muokattuKasin = new HashMap<>();
        if (jonosija.getSelite() != null && !jonosija.getSelite().isEmpty()) {
            muokattuKasin.put("FI", jonosija.getSelite());
        } else {
            muokattuKasin.put("FI", "Muokattu käsin");
        }

        jarjestyskriteeritulos.setKuvaus(muokattuKasin);
        jarjestyskriteeritulos.setArvo(jonosija.getArvo());
        jarjestyskriteeritulos.setTila(jonosija.getTila());

        addLogEntry(jonosija.getSelite(), muokattuJonosija,
                "jarjestyskriteeriPrioriteetti: "
                        + jarjestyskriteeriPrioriteetti + " arvo: "
                        + jonosija.getArvo() + " tila: "
                        + jonosija.getTila().name());

        muokattuJonosijaDAO.saveOrUpdate(muokattuJonosija);
        return muokattuJonosija;
    }

    @Override
    public MuokattuJonosija poistaMuokattuJonosija(String valintatapajonoOid, String hakemusOid, Integer jarjestyskriteeriPrioriteetti) {
        MuokattuJonosija muokattuJonosija;
        muokattuJonosija = muokattuJonosijaDAO.readByValintatapajonoOid(valintatapajonoOid, hakemusOid);
        if (muokattuJonosija == null) {
            return null;
        } else {
            List<Jarjestyskriteeritulos> saastettavat = muokattuJonosija.getJarjestyskriteerit()
                    .stream()
                    .filter(j -> j.getPrioriteetti() != jarjestyskriteeriPrioriteetti)
                    .collect(Collectors.toList());
            muokattuJonosija.setJarjestyskriteerit(saastettavat);
            muokattuJonosijaDAO.saveOrUpdate(muokattuJonosija);
            return muokattuJonosija;
        }
    }

    private void addLogEntry(String selite, MuokattuJonosija muokattuJonosija, String muutos) {
        LogEntry logEntry = new LogEntry();
        logEntry.setLuotu(new Date());
        logEntry.setMuokkaaja(AuthorizationUtil.getCurrentUser());
        logEntry.setSelite(selite);
        logEntry.setMuutos(muutos);
        muokattuJonosija.getLogEntries().add(logEntry);
    }

    private boolean isOPH() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        for (GrantedAuthority authority : authentication.getAuthorities()) {
            if (authority.getAuthority().contains(rootOrgOid)) {
                return true;
            }
        }
        return false;
    }
}
