package fi.vm.sade.valintalaskenta.tulos.service.impl.rest;


import fi.vm.sade.valintalaskenta.domain.dto.*;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.*;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Hakutoive;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Valintakoe;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeValinnanvaihe;

import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;

import fi.vm.sade.valintalaskenta.tulos.service.impl.ValintatietoServiceRest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.*;

/**
 * User: kkammone Date: 29.4.2013 Time: 13:24
 */
// @PreAuthorize("isAuthenticated()")
@Service
public class ValintatietoServiceRestImpl implements ValintatietoServiceRest {

    private static final Logger LOG = LoggerFactory
            .getLogger(ValintatietoServiceRestImpl.class);
    @Autowired
    private ValintalaskentaTulosService tulosService;

    @Autowired
    private ValintalaskentaModelMapper modelMapper;

    // @PreAuthorize(READ_UPDATE_CRUD)
    public List<HakemusOsallistuminenDTO> haeValintatiedotHakukohteelle(
            List<String> valintakoeOid,
            String hakukohdeOid) {
        List<HakemusOsallistuminenDTO> osallistumiset = new ArrayList<HakemusOsallistuminenDTO>();
        try {
            List<ValintakoeOsallistuminen> valinnanvaiheet = tulosService
                    .haeValintakoeOsallistumisetByHakutoive(hakukohdeOid);
            Set<String> oidit = new HashSet<String>(valintakoeOid);
            GregorianCalendar kalenteri = new GregorianCalendar();
            for (ValintakoeOsallistuminen koetulos : valinnanvaiheet) {
                for (Hakutoive hakutoive : koetulos.getHakutoiveet()) {
                    for (ValintakoeValinnanvaihe vaihe : hakutoive
                            .getValinnanVaiheet()) {
                        HakemusOsallistuminenDTO h = new HakemusOsallistuminenDTO();
                        for (Valintakoe valintakoe : vaihe.getValintakokeet()) {
                            if (oidit.contains(valintakoe.getValintakoeOid())) {
                                ValintakoeOsallistuminenDTO osallistuminen = new ValintakoeOsallistuminenDTO();
                                osallistuminen.setOsallistuminen(
                                        modelMapper.map(valintakoe.getOsallistuminenTulos().getOsallistuminen(), OsallistuminenDTO.class)
                                );
                                osallistuminen.setValintakoeOid(valintakoe
                                        .getValintakoeOid());
                                osallistuminen.setValintakoeTunniste(valintakoe
                                        .getValintakoeTunniste());
                                osallistuminen.setNimi(valintakoe.getNimi());
                                h.getOsallistumiset().add(osallistuminen);
                            }
                        }
                        // lisataan tulosjoukkoon vaan jos valinnanvaiheessa oli
                        // valintakoe hakemukselle!
                        if (!h.getOsallistumiset().isEmpty()) {
                            h.setLuontiPvm(koetulos.getCreatedAt());
                            h.setEtunimi(koetulos.getEtunimi());
                            h.setSukunimi(koetulos.getSukunimi());
                            h.setHakemusOid(koetulos.getHakemusOid());
                            osallistumiset.add(h);
                        }
                    }
                }
            }
        } catch (Exception e) {
            LOG.error("Valintakoelaskennan osallitujia ei saatu haettua!");
            LOG.error("Virhe osallistujien hakemisessa: {} {} {}",
                    e.getMessage(), e.getCause(),
                    Arrays.toString(e.getStackTrace()));
            throw new RuntimeException(
                    "Valintatieto osallistujille pyyntö epäonnistui!", e);
        }
        return osallistumiset;
    }

    // @PreAuthorize(READ_UPDATE_CRUD)
        public HakuDTO haeValintatiedot(String hakuOid) {
        try {
            List<HakukohdeDTO> a = tulosService
                    .haeLasketutValinnanvaiheetHaulle(hakuOid);

            HakuDTO hakuDTO = new HakuDTO();
            hakuDTO.setHakuOid(hakuOid);

            for (HakukohdeDTO v : a) {
                HakukohdeDTO ht = new HakukohdeDTO();
                ht.setOid(v.getOid());
                ht.setTarjoajaoid(v.getTarjoajaoid());
                hakuDTO.getHakukohteet().add(ht);

                for (ValinnanvaiheDTO valinnanvaiheDTO : v.getValinnanvaihe()) {
                    ht.getValinnanvaihe().add(
                            createValinnanvaiheTyyppi(valinnanvaiheDTO));
                }
            }
            return hakuDTO;
        } catch (Exception e) {
            LOG.error("Valintatietoja ei saatu haettua!");
            LOG.error("Virhe valintatietojen hakemisessa: {} {} {}",
                    e.getMessage(), e.getCause(),
                    Arrays.toString(e.getStackTrace()));
            throw new RuntimeException("Valintatietojen haku epäonnistui!", e);
        }

    }

    private ValintatietoValinnanvaiheDTO createValinnanvaiheTyyppi(ValinnanvaiheDTO valinnanvaihe) {
        ValintatietoValinnanvaiheDTO v = new ValintatietoValinnanvaiheDTO();
        v.setValinnanvaihe(valinnanvaihe.getJarjestysnumero());
        v.setValinnanvaiheoid(valinnanvaihe.getValinnanvaiheoid());
        for (ValintatapajonoDTO vt : valinnanvaihe.getValintatapajonot()) {
            v.getValintatapajonot().add(createValintatapajonoTyyppi(vt));
        }
        return v;
    }

    private ValintatietoValintatapajonoDTO createValintatapajonoTyyppi(ValintatapajonoDTO vt) {
        ValintatietoValintatapajonoDTO dto = new ValintatietoValintatapajonoDTO();
        dto.setValintatapajonooid(vt.getValintatapajonooid());
        dto.setAloituspaikat(vt.getAloituspaikat());
        dto.setNimi(vt.getNimi());
        dto.setPrioriteetti(vt.getPrioriteetti());
        dto.setSiirretaanSijoitteluun(vt.isSiirretaanSijoitteluun());
        dto.setEiVarasijatayttoa(vt.getEiVarasijatayttoa());
        if (vt.getTasasijasaanto() != null) {
            dto.setTasasijasaanto(vt.getTasasijasaanto());
        }

        for (JonosijaDTO jonosija : vt.getJonosijat()) {
            HakijaDTO ht = new HakijaDTO();
            ht.setPrioriteetti(jonosija.getPrioriteetti());

            if (jonosija.getTuloksenTila() == null) {
                ht.setTila(JarjestyskriteerituloksenTilaDTO.MAARITTELEMATON);
            } else {
                ht.setTila(JarjestyskriteerituloksenTilaDTO.valueOf(jonosija.getTuloksenTila().name()));
            }
            ht.setHakemusOid(jonosija.getHakemusOid());
            ht.setEtunimi(jonosija.getEtunimi());
            ht.setSukunimi(jonosija.getSukunimi());
            ht.setOid(jonosija.getHakijaOid());
            ht.setJonosija(jonosija.getJonosija());
            for (SyotettyArvoDTO sa : jonosija.getSyotetytArvot()) {
                ht.getSyotettyArvo().add(sa);

            }

            if (jonosija.isHarkinnanvarainen()) {
                ht.setHarkinnanvarainen(Boolean.TRUE);
            }

            if (jonosija.getJarjestyskriteerit().size() > 0) {
                ht.setPisteet(jonosija.getJarjestyskriteerit().first().getArvo());
            }

            dto.getHakija().add(ht);
        }
        return dto;
    }

}
