package fi.vm.sade.valintalaskenta.tulos.service.impl;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.CRUD;
import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.READ;
import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.UPDATE;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;
import javax.jws.soap.SOAPBinding;
import javax.jws.soap.SOAPBinding.ParameterStyle;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.convert.ConversionService;
import org.springframework.security.access.annotation.Secured;
import org.springframework.security.access.prepost.PreAuthorize;

import fi.vm.sade.service.valintaperusteet.schema.TasasijasaantoTyyppi;
import fi.vm.sade.service.valintatiedot.ValintatietoService;
import fi.vm.sade.service.valintatiedot.messages.HaeValintatiedotHakukohteelleTyyppi;
import fi.vm.sade.service.valintatiedot.messages.HaeValintatiedotHakukohteelleVastausTyyppi;
import fi.vm.sade.service.valintatiedot.schema.HakemusTilaTyyppi;
import fi.vm.sade.service.valintatiedot.schema.HakijaTyyppi;
import fi.vm.sade.service.valintatiedot.schema.HakuTyyppi;
import fi.vm.sade.service.valintatiedot.schema.HakukohdeTyyppi;
import fi.vm.sade.service.valintatiedot.schema.ValinnanvaiheTyyppi;
import fi.vm.sade.service.valintatiedot.schema.ValintatapajonoTyyppi;
import fi.vm.sade.valintalaskenta.domain.Hakukohde;
import fi.vm.sade.valintalaskenta.domain.Jonosija;
import fi.vm.sade.valintalaskenta.domain.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;

/**
 * Created with IntelliJ IDEA. User: kkammone Date: 29.4.2013 Time: 13:24 To
 * change this template use File | Settings | File Templates.
 */
@PreAuthorize("isAuthenticated()")
public class ValintatietoServiceImpl implements ValintatietoService {

    @Autowired
    private ValintalaskentaTulosService tulosService;

    @Autowired
    private ConversionService conversionService;

    @Override
    @SOAPBinding(parameterStyle = ParameterStyle.BARE)
    @WebResult(name = "haeValintaTiedotHakukohteelleVastaus", targetNamespace = "http://valintatiedot.service.sade.vm.fi/messages", partName = "parameters")
    @WebMethod
    public HaeValintatiedotHakukohteelleVastausTyyppi haeValintatiedotHakukohteelle(
            HaeValintatiedotHakukohteelleTyyppi parameters) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    @Secured({ READ, UPDATE, CRUD })
    public HakuTyyppi haeValintatiedot(@WebParam(name = "hakuOid", targetNamespace = "") String hakuOid) {

        List<Hakukohde> a = tulosService.haeLasketutValinnanvaiheetHaulle(hakuOid);

        HakuTyyppi hakuTyyppi = new HakuTyyppi();
        hakuTyyppi.setHakuOid(hakuOid);

        Map<String, HakukohdeTyyppi> hakukohdeMap = new HashMap<String, HakukohdeTyyppi>();

        for (Hakukohde v : a) {
            HakukohdeTyyppi ht = hakukohdeMap.get(v.getOid());
            if (ht == null) {
                ht = new HakukohdeTyyppi();
                ht.setOid(v.getOid());
                hakukohdeMap.put(v.getOid(), ht);
            }
            ht.getValinnanvaihe().add(createValinnanvaiheTyyppi(v.getValinnanvaihe()));
        }

        hakuTyyppi.getHakukohteet().addAll(new ArrayList(hakukohdeMap.values()));

        return hakuTyyppi;
    }

    private ValinnanvaiheTyyppi createValinnanvaiheTyyppi(Valinnanvaihe valinnanvaihe) {
        ValinnanvaiheTyyppi v = new ValinnanvaiheTyyppi();
        v.setValinnanvaihe(valinnanvaihe.getJarjestysnumero());
        v.setValinnanvaiheOid(valinnanvaihe.getValinnanvaiheoid());
        for (fi.vm.sade.valintalaskenta.domain.Valintatapajono vt : valinnanvaihe.getValintatapajono()) {
            v.getValintatapajono().add(createValintatapajonoTyyppi(vt));
        }
        return v;
    }

    private ValintatapajonoTyyppi createValintatapajonoTyyppi(fi.vm.sade.valintalaskenta.domain.Valintatapajono vt) {
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

        // Sorttaa jonosijat ja laita oikea jonosija tulos niille
        List<Jonosija> jonosijat = vt.getJonosijat();
        // ValintatapajonoHelper.sortJonosijat(jonosijat);

        for (Jonosija jonosija : jonosijat) {
            HakijaTyyppi ht = new HakijaTyyppi();
            ht.setPrioriteetti(jonosija.getPrioriteetti());

            if (jonosija.getTuloksenTila() == null) {
                ht.setTila(HakemusTilaTyyppi.MAARITTELEMATON);
            } else {
                ht.setTila(HakemusTilaTyyppi.valueOf(jonosija.getTuloksenTila().name()));
            }
            ht.setHakemusOid(jonosija.getHakemusoid());
            ht.setOid(jonosija.getHakijaoid());
            ht.setJonosija(jonosija.getJonosija());
            if (jonosija.isHarkinnanvarainen()) {
                ht.setHarkinnanvarainen(Boolean.TRUE);
            }

            valintatapajonoTyyppi.getHakija().add(ht);
        }
        return valintatapajonoTyyppi;
    }

}
