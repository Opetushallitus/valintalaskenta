package fi.vm.sade.valintalaskenta.laskenta;

/**
 *         Monessako jonossa hakemus esiintyy ja monestiko hakemus on
 *         hyväksytty. Jos hakemus ei ole hyväksytty kertaakaan niin hakemus on
 *         hylättävä jatkossa vaikka myöhemmissä vaiheissa tulisi onnistumisia.
 */
public class Esiintyminen {
    private Integer hyvaksyttavissa;
    private Integer esiintyy;

    public Esiintyminen(Integer hyvaksyttavissa, Integer esiintyy) {
        this.hyvaksyttavissa = hyvaksyttavissa;
        this.esiintyy = esiintyy;
    }

    public void inkrementoiEsiintyminen() {
        ++esiintyy;
    }

    public void inkrementoiHyvaksyttavissa() {
        ++hyvaksyttavissa;
    }

    public Integer getEsiintyy() {
        return esiintyy;
    }

    public Integer getHyvaksyttavissa() {
        return hyvaksyttavissa;
    }
}
