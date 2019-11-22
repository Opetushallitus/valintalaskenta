package fi.vm.sade.valintalaskenta.laskenta.service.impl;

import static fi.vm.sade.sijoittelu.tulos.dto.HakemuksenTila.PERUUNTUNUT;
import static fi.vm.sade.sijoittelu.tulos.dto.HakemuksenTila.VARALLA;
import static java.util.Arrays.asList;
import static java.util.Collections.singletonList;

import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetHakijaryhmaDTO;
import fi.vm.sade.service.valintaperusteet.dto.model.ValinnanVaiheTyyppi;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.laskenta.resource.ValintakoelaskennanKumulatiivisetTulokset;
import fi.vm.sade.valintalaskenta.laskenta.service.ValintalaskentaService;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.ValintalaskentaSuorittajaService;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.ValintakoelaskentaSuorittajaService;
import fi.vm.sade.valintalaskenta.tulos.dao.ValinnanvaiheDAO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Service
public class ValintalaskentaServiceImpl implements ValintalaskentaService {
    private static final Logger LOG = LoggerFactory.getLogger(ValintalaskentaServiceImpl.class);

    @Autowired
    private ValintalaskentaSuorittajaService valintalaskentaSuorittaja;

    @Autowired
    private ValintakoelaskentaSuorittajaService valintakoelaskentaSuorittajaService;

    @Autowired
    private ValinnanvaiheDAO valinnanvaiheDAO;

    @Override
    public String laske(List<HakemusDTO> hakemus, List<ValintaperusteetDTO> valintaperuste, List<ValintaperusteetHakijaryhmaDTO> hakijaryhmat,
                        String hakukohdeOid, String uuid, boolean korkeakouluhaku) throws RuntimeException {
        if (hakemus == null) {
            LOG.error("Hakemukset tuli nullina hakukohteelle {}", hakukohdeOid);
        }
        if (valintaperuste == null) {
            LOG.error("Valintaperuste tuli nullina hakukohteelle {}", hakukohdeOid);
        }
        if (hakemus == null || valintaperuste == null) {
            throw new RuntimeException("Hakemukset == null? " + (hakemus == null) + ", valintaperusteet == null? " + (valintaperuste == null) + " hakukohteelle " + hakukohdeOid);
        }
        try {
            valintalaskentaSuorittaja.suoritaLaskenta(hakemus, valintaperuste, hakijaryhmat, hakukohdeOid, uuid, korkeakouluhaku);
            return "Onnistui!";
        } catch (Exception e) {
            LOG.error("Valintalaskennassa tapahtui virhe hakukohteelle " + hakukohdeOid, e);
            throw new RuntimeException(e);
        }
    }

    @Override
    public String valintakokeetRinnakkain(List<HakemusDTO> hakemukset, List<ValintaperusteetDTO> valintaperuste, String uuid, ValintakoelaskennanKumulatiivisetTulokset kumulatiivisetTulokset, boolean korkeakouluhaku) throws RuntimeException {
        try {
            LOG.info("(Uuid={}) Lasketaan rinnakkain valintakoeosallistumiset {} hakemukselle", uuid, hakemukset.size());
            //hakemukset.parallelStream().forEach(hakemus -> valintakoelaskentaSuorittajaService.laske(hakemus, valintaperuste, uuid, kumulatiivisetTulokset, korkeakouluhaku));
            //Temporarily not parallel for readable debug logging
            hakemukset.stream().forEach(hakemus -> valintakoelaskentaSuorittajaService.laske(hakemus, valintaperuste, uuid, kumulatiivisetTulokset, korkeakouluhaku));
            LOG.info("(Uuid={}) Valintakoeosallistumisten laskenta {} hakemukselle valmis", uuid, hakemukset.size());
            return "Onnistui!";
        } catch (Exception e) {
            LOG.error("Valintakoevaihe epäonnistui", e);
            throw new RuntimeException(e);
        }
    }

    @Override
    public String laskeKaikki(List<HakemusDTO> hakemukset, List<ValintaperusteetDTO> valintaperuste, List<ValintaperusteetHakijaryhmaDTO> hakijaryhmat,
                              String hakukohdeOid, String uuid, boolean korkeakouluhaku) throws RuntimeException {
        valintaperuste.sort(Comparator.comparingInt(o -> o.getValinnanVaihe().getValinnanVaiheJarjestysluku()));

        ValintakoelaskennanKumulatiivisetTulokset kumulatiivisetTulokset = new ValintakoelaskennanKumulatiivisetTulokset();
        valintaperuste.stream().forEachOrdered(peruste -> {
            if (peruste.getValinnanVaihe().getValinnanVaiheTyyppi().equals(ValinnanVaiheTyyppi.VALINTAKOE)) {
                LOG.info("Suoritetaan valinnanvaiheen {} valintakoelaskenta {} hakemukselle", peruste.getValinnanVaihe().getValinnanVaiheOid(), hakemukset.size());
                valintakokeetRinnakkain(hakemukset, singletonList(peruste), uuid, kumulatiivisetTulokset, korkeakouluhaku);
            } else {
                laske(hakemukset, singletonList(peruste), hakijaryhmat, hakukohdeOid, uuid, korkeakouluhaku);
            }
        });
        return "Onnistui!";
    }

    @Override
    public void applyValisijoittelu(Map<String, List<String>> valisijoiteltavatJonot, Map<String, fi.vm.sade.sijoittelu.tulos.dto.HakemusDTO> hakemusHashMap) {
        valisijoiteltavatJonot.keySet().parallelStream().forEach(hakukohdeOid -> {
            List<Valinnanvaihe> vaiheet = valinnanvaiheDAO.readByHakukohdeOid(hakukohdeOid);
            vaiheet.forEach(vaihe -> {
                List<String> hakukohteenValisijoitelujonot = valisijoiteltavatJonot.getOrDefault(hakukohdeOid, new ArrayList<>());

                // Onko tässä valinanvaiheessa välisijoiteltavia jonoja
                final boolean valisijoitteluVaihe = vaihe.getValintatapajonot().stream().anyMatch(j -> hakukohteenValisijoitelujonot.contains(j.getValintatapajonoOid()));
                if (valisijoitteluVaihe) {
                    vaihe.getValintatapajonot()
                            .forEach(jono -> {
                                if (hakukohteenValisijoitelujonot.contains(jono.getValintatapajonoOid())) {
                                    jono.getJonosijat().forEach(jonosija -> {
                                        fi.vm.sade.sijoittelu.tulos.dto.HakemusDTO hakemusDTO = hakemusHashMap.get(hakukohdeOid + jono.getValintatapajonoOid() + jonosija.getHakemusOid());

                                        if(jonosija.isHylattyValisijoittelussa() && hakemusDTO.getTila().isHyvaksytty()) {
                                            Collections.sort(jonosija.getJarjestyskriteeritulokset(), Comparator.comparingInt(Jarjestyskriteeritulos::getPrioriteetti));
                                            Jarjestyskriteeritulos jarjestyskriteeritulos = jonosija.getJarjestyskriteeritulokset().get(0);
                                            jarjestyskriteeritulos.setTila(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA);
                                            jonosija.setHylattyValisijoittelussa(false);
                                            jarjestyskriteeritulos.setKuvaus(new HashMap<>());
                                        }
                                        
                                        if (hakemusDTO != null && asList(VARALLA, PERUUNTUNUT).contains(hakemusDTO.getTila())) {
                                            Collections.sort(jonosija.getJarjestyskriteeritulokset(), Comparator.comparingInt(Jarjestyskriteeritulos::getPrioriteetti));
                                            Jarjestyskriteeritulos jarjestyskriteeritulos = jonosija.getJarjestyskriteeritulokset().get(0);
                                            jarjestyskriteeritulos.setTila(JarjestyskriteerituloksenTila.HYLATTY);
                                            jonosija.setHylattyValisijoittelussa(true);
                                            Map<String, String> kuvaukset = new HashMap<>();
                                            if (hakemusDTO.getTila() == VARALLA) {
                                                kuvaukset.put("FI", "Pisteesi eivät riittäneet valintakoekutsuun");
                                                kuvaukset.put("SV", "Dina poäng räckte inte till för en kallelse till urvalsprov");
                                                kuvaukset.put("EN", "You did not score high enough for an invitation to the entrance examination");
                                            } else if (hakemusDTO.getTila() == PERUUNTUNUT) {
                                                kuvaukset.put("FI", "Hyväksyttiin korkeammalle hakutoiveelle");
                                                kuvaukset.put("SV", "Godkänd till ett högre ansökningsönskemål");
                                                kuvaukset.put("EN", "Accepted to a higher preference");
                                            }
                                            jarjestyskriteeritulos.setKuvaus(kuvaukset);
                                        }
                                    });
                                }
                            });
                    valinnanvaiheDAO.saveOrUpdate(vaihe);
                }
            });
        });

    }

    @Override
    public void applyErillissijoittelu(Map<String, List<String>> jonot, Long ajo) {
        jonot.keySet().parallelStream().forEach(hakukohdeOid -> {
            List<Valinnanvaihe> vaiheet = valinnanvaiheDAO.readByHakukohdeOid(hakukohdeOid);
            vaiheet.forEach(vaihe -> {
                vaihe.getValintatapajonot()
                        .forEach(jono -> {
                            if (jonot.getOrDefault(hakukohdeOid, new ArrayList<>()).indexOf(jono.getValintatapajonoOid()) != -1) {
                                jono.setSijoitteluajoId(ajo);
                            }
                        });
                valinnanvaiheDAO.saveOrUpdate(vaihe);
            });
        });
    }
}
