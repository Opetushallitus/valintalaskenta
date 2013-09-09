package fi.vm.sade.valintalaskenta.tulos.service.impl;

import fi.vm.sade.service.valintaperusteet.schema.TasasijasaantoTyyppi;
import fi.vm.sade.service.valintatiedot.ValintatietoService;
import fi.vm.sade.service.valintatiedot.schema.*;
import fi.vm.sade.valintalaskenta.domain.dto.HakukohdeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.JonosijaDTO;
import fi.vm.sade.valintalaskenta.domain.dto.ValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.dto.ValintatapajonoDTO;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Hakutoive;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Valintakoe;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeValinnanvaihe;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.convert.ConversionService;
import org.springframework.security.access.annotation.Secured;
import org.springframework.security.access.prepost.PreAuthorize;

import javax.jws.WebParam;
import javax.xml.datatype.DatatypeFactory;
import java.util.*;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.*;

/**
 * User: kkammone Date: 29.4.2013 Time: 13:24
 */
@PreAuthorize("isAuthenticated()")
public class ValintatietoServiceImpl implements ValintatietoService {

    @Autowired
    private ValintalaskentaTulosService tulosService;

    @Autowired
    private ConversionService conversionService;

    @Override
    @Secured({ READ, UPDATE, CRUD })
    public List<HakemusOsallistuminenTyyppi> haeValintatiedotHakukohteelle(
            @WebParam(name = "valintakoeOid", targetNamespace = "") List<String> valintakoeOid,
            @WebParam(name = "hakukohdeOid", targetNamespace = "") String hakukohdeOid) {
        List<HakemusOsallistuminenTyyppi> osallistumiset = new ArrayList<HakemusOsallistuminenTyyppi>();
        List<ValintakoeOsallistuminen> valinnanvaiheet = tulosService
                .haeValintakoeOsallistumisetByHakutoive(hakukohdeOid);
        Set<String> oidit = new HashSet<String>(valintakoeOid);
        GregorianCalendar kalenteri = new GregorianCalendar();
        for (ValintakoeOsallistuminen koetulos : valinnanvaiheet) {
            for (Hakutoive hakutoive : koetulos.getHakutoiveet()) {
                for (ValintakoeValinnanvaihe vaihe : hakutoive.getValinnanVaiheet()) {
                    HakemusOsallistuminenTyyppi h = new HakemusOsallistuminenTyyppi();
                    for (Valintakoe valintakoe : vaihe.getValintakokeet()) {
                        if (oidit.contains(valintakoe.getValintakoeOid())) {
                            ValintakoeOsallistuminenTyyppi osallistuminen = new ValintakoeOsallistuminenTyyppi();
                            osallistuminen.setOsallistuminen(Osallistuminen.valueOf(valintakoe.getOsallistuminenTulos()
                                    .getOsallistuminen().name()));
                            osallistuminen.setValintakoeOid(valintakoe.getValintakoeOid());
                            osallistuminen.setValintakoeTunniste(valintakoe.getValintakoeTunniste());
                            h.getOsallistumiset().add(osallistuminen);
                        }
                    }
                    // lisataan tulosjoukkoon vaan jos valinnanvaiheessa oli
                    // valintakoe hakemukselle!
                    if (!h.getOsallistumiset().isEmpty()) {
                        kalenteri.setTime(koetulos.getCreatedAt());
                        try {
                            h.setLuontiPvm(DatatypeFactory.newInstance().newXMLGregorianCalendar(kalenteri));
                        } catch (Exception e) {
                            e.printStackTrace(); // <- creating date failed!
                        }
                        h.setHakemusOid(koetulos.getHakemusOid());
                        osallistumiset.add(h);
                    }
                }
            }
        }

        return osallistumiset;
    }

    @Override
    @Secured({ READ, UPDATE, CRUD })
    public HakuTyyppi haeValintatiedot(@WebParam(name = "hakuOid", targetNamespace = "") String hakuOid) {

        List<HakukohdeDTO> a = tulosService.haeLasketutValinnanvaiheetHaulle(hakuOid);

        HakuTyyppi hakuTyyppi = new HakuTyyppi();
        hakuTyyppi.setHakuOid(hakuOid);

        for (HakukohdeDTO v : a) {
            HakukohdeTyyppi ht = new HakukohdeTyyppi();
            ht.setOid(v.getOid());
            ht.setTarjoajaOid(v.getTarjoajaoid());
            hakuTyyppi.getHakukohteet().add(ht);

            for (ValinnanvaiheDTO valinnanvaiheDTO : v.getValinnanvaihe()) {
                ht.getValinnanvaihe().add(createValinnanvaiheTyyppi(valinnanvaiheDTO));

            }
        }
        return hakuTyyppi;
    }

    private ValinnanvaiheTyyppi createValinnanvaiheTyyppi(ValinnanvaiheDTO valinnanvaihe) {
        ValinnanvaiheTyyppi v = new ValinnanvaiheTyyppi();
        v.setValinnanvaihe(valinnanvaihe.getJarjestysnumero());
        v.setValinnanvaiheOid(valinnanvaihe.getValinnanvaiheoid());
        for (ValintatapajonoDTO vt : valinnanvaihe.getValintatapajono()) {
            v.getValintatapajono().add(createValintatapajonoTyyppi(vt));
        }
        return v;
    }

    private ValintatapajonoTyyppi createValintatapajonoTyyppi(ValintatapajonoDTO vt) {
        ValintatapajonoTyyppi valintatapajonoTyyppi = new ValintatapajonoTyyppi();
        valintatapajonoTyyppi.setOid(vt.getOid());
        valintatapajonoTyyppi.setAloituspaikat(vt.getAloituspaikat());
        valintatapajonoTyyppi.setNimi(vt.getNimi());
        valintatapajonoTyyppi.setPrioriteetti(vt.getPrioriteetti());
        valintatapajonoTyyppi.setSiirretaanSijoitteluun(vt.isSiirretaanSijoitteluun());
        valintatapajonoTyyppi.setEiVarasijatayttoa(vt.getEiVarasijatayttoa());
        if (vt.getTasasijasaanto() != null) {
            valintatapajonoTyyppi.setTasasijasaanto(TasasijasaantoTyyppi.valueOf(vt.getTasasijasaanto().name()));
        }

        for (JonosijaDTO jonosija : vt.getJonosijat()) {
            HakijaTyyppi ht = new HakijaTyyppi();
            ht.setPrioriteetti(jonosija.getPrioriteetti());

            if (jonosija.getTuloksenTila() == null) {
                ht.setTila(HakemusTilaTyyppi.MAARITTELEMATON);
            } else {
                ht.setTila(HakemusTilaTyyppi.valueOf(jonosija.getTuloksenTila().name()));
            }
            ht.setHakemusOid(jonosija.getHakemusOid());
            ht.setEtunimi(jonosija.getEtunimi());
            ht.setSukunimi(jonosija.getSukunimi());
            ht.setOid(jonosija.getHakijaOid());
            ht.setJonosija(jonosija.getJonosija());

            if (jonosija.isHarkinnanvarainen()) {
                ht.setHarkinnanvarainen(Boolean.TRUE);
            }

            if(jonosija.getJarjestyskriteerit().size() > 0 && jonosija.getJarjestyskriteerit().get(0).getArvo() != null) {
                ht.setPisteet(jonosija.getJarjestyskriteerit().get(0).getArvo().toString());
            }

            valintatapajonoTyyppi.getHakija().add(ht);
        }
        return valintatapajonoTyyppi;
    }

}
