package fi.vm.sade.valintalaskenta.laskenta.service.it;

import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteetTyyppi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: kjsaila
 * Date: 20/12/13
 * Time: 08:07
 * To change this template use File | Settings | File Templates.
 */
@XmlRootElement(name = "haeValintaperusteetVastaus")
@XmlAccessorType(XmlAccessType.FIELD)
public class ValintaperusteetTyyppis
{
    @XmlElement(name = "valintaPerusteet")
    private List<ValintaperusteetTyyppi> tyypit = null;

    public List<ValintaperusteetTyyppi> getTyypit() {
        return tyypit;
    }

    public void setTyypit(List<ValintaperusteetTyyppi> tyypit) {
        this.tyypit = tyypit;
    }
}
