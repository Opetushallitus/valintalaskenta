package fi.vm.sade.valintalaskenta.domain.dto;

import fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos;

import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: kkammone
 * Date: 13.5.2013
 * Time: 9:50
 * To change this template use File | Settings | File Templates.
 */
public class JonosijaDTO {

    private List<Jarjestyskriteeritulos> jarjestyskriteerit;

    public List<Jarjestyskriteeritulos> getJarjestyskriteerit() {
        return jarjestyskriteerit;
    }

    public void setJarjestyskriteerit(List<Jarjestyskriteeritulos> jarjestyskriteerit) {
        this.jarjestyskriteerit = jarjestyskriteerit;
    }
}
