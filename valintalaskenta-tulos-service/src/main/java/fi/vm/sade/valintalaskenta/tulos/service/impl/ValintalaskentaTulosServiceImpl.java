package fi.vm.sade.valintalaskenta.tulos.service.impl;

import java.util.List;

import fi.vm.sade.valintalaskenta.domain.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import fi.vm.sade.valintalaskenta.tulos.dao.HakukohdeDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.JarjestyskriteeritulosDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.ValinnanvaiheDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.ValintatapajonoDAO;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;

/**
 *
 * @author Jussi Jartamo
 *
 */
@Service
public class ValintalaskentaTulosServiceImpl implements ValintalaskentaTulosService {

    private static final Logger LOGGER = LoggerFactory.getLogger(ValintalaskentaTulosServiceImpl.class);

    @Autowired
    private HakukohdeDAO hakukohdeDAO;

    @Autowired
    private JarjestyskriteeritulosDAO jarjestyskriteeritulosDAO;

    @Autowired
    private ValinnanvaiheDAO valinnanvaiheDAO;

    @Autowired
    private ValintatapajonoDAO valintatapajonoDAO;

    @Override
    public List<Versioituhakukohde> haeHakukohteet() {
        return hakukohdeDAO.readAll();
    }

    @Override
    public List<Versioituhakukohde> haeHakukohteetHaulle(String hakuoid) {
        return hakukohdeDAO.readByHakuOid(hakuoid);
    }

    @Override
    public List<Jarjestyskriteeritulos> haeJarjestyskriteerituloksetValintatapajonolle(String valintatapajonooid) {
        return jarjestyskriteeritulosDAO.readByValintatapajonoOid(valintatapajonooid);
    }

    @Override
    public List<Valinnanvaihe> haeValinnanvaiheetHakukohteelle(String hakukohdeoid) {
        return valinnanvaiheDAO.readByHakukohdeOid(hakukohdeoid);
    }

    @Override
    public List<Valintatapajono> haeValintatapajonoValinnanvaiheelle(String valinnanvaiheoid) {
        return valintatapajonoDAO.readByValinnanvaiheOid(valinnanvaiheoid);
    }

    @Override
    public List<Hakukohde> haeLasketutValinnanvaiheetHaulle(String hakuOid) {
        return valinnanvaiheDAO.readByHakuOid(hakuOid);
    }

}
