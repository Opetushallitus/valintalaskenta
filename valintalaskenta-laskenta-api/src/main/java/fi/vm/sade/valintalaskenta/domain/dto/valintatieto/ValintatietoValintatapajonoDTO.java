package fi.vm.sade.valintalaskenta.domain.dto.valintatieto;

import fi.vm.sade.valintalaskenta.domain.dto.HakijaDTO;
import fi.vm.sade.valintalaskenta.domain.dto.JonosijaDTO;
import fi.vm.sade.valintalaskenta.domain.dto.ValintatapajonoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.Tasasijasaanto;
import io.swagger.annotations.ApiModelProperty;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

public class ValintatietoValintatapajonoDTO extends ValintatapajonoDTO {
    private List<HakijaDTO> hakija;

    @ApiModelProperty(value = "Varasijojen lkm. 0 == pois päältä", required = true)
    private Integer varasijat = 0;

    @ApiModelProperty(value = "Kuinka monta päivää varasijoja täytetään", required = true)
    private Integer varasijaTayttoPaivat = 0;

    @ApiModelProperty(value = "Varasijasääntöjä käytetään alkaen")
    private Date varasijojaKaytetaanAlkaen;

    @ApiModelProperty(value = "Varasijoja täytetään asti")
    private Date varasijojaTaytetaanAsti;

    @ApiModelProperty(value = "Valintatapajono, josta vapaaksi jääneet paikat täytetään", required = false)
    private String tayttojono;

    @ApiModelProperty(value = "Erillishaussa käytetty sijoitteluajo", required = false)
    private Long sijoitteluajoId;

    public ValintatietoValintatapajonoDTO() {
    }

    public ValintatietoValintatapajonoDTO(String valintatapajonooid, String nimi, int prioriteetti, int aloituspaikat, boolean siirretaanSijoitteluun, Tasasijasaanto tasasijasaanto, Boolean eiVarasijatayttoa, Boolean kaikkiEhdonTayttavatHyvaksytaan, Boolean poissaOlevaTaytto, Boolean kaytetaanValintalaskentaa, Boolean valmisSijoiteltavaksi, List<JonosijaDTO> jonosijat, Boolean aktiivinen, List<HakijaDTO> hakija, Integer varasijat, Integer varasijaTayttoPaivat, Date varasijojaKaytetaanAlkaen, Date varasijojaTaytetaanAsti, String tayttojono, Long sijoitteluajoId) {
        super(valintatapajonooid, nimi, prioriteetti, aloituspaikat, siirretaanSijoitteluun, tasasijasaanto, eiVarasijatayttoa, kaikkiEhdonTayttavatHyvaksytaan, poissaOlevaTaytto, kaytetaanValintalaskentaa, valmisSijoiteltavaksi, jonosijat, aktiivinen);
        this.hakija = hakija;
        this.varasijat = varasijat;
        this.varasijaTayttoPaivat = varasijaTayttoPaivat;
        this.varasijojaKaytetaanAlkaen = varasijojaKaytetaanAlkaen;
        this.varasijojaTaytetaanAsti = varasijojaTaytetaanAsti;
        this.tayttojono = tayttojono;
        this.sijoitteluajoId = sijoitteluajoId;
    }

    public List<HakijaDTO> getHakija() {
        if (hakija == null) {
            hakija = new ArrayList<HakijaDTO>();
        }
        return hakija;
    }

    public void setHakija(List<HakijaDTO> hakija) {
        this.hakija = hakija;
    }

    public Integer getVarasijat() {
        return varasijat;
    }

    public void setVarasijat(Integer varasijat) {
        this.varasijat = varasijat;
    }

    public Integer getVarasijaTayttoPaivat() {
        return varasijaTayttoPaivat;
    }

    public void setVarasijaTayttoPaivat(Integer varasijaTayttoPaivat) {
        this.varasijaTayttoPaivat = varasijaTayttoPaivat;
    }

    public Date getVarasijojaKaytetaanAlkaen() {
        return varasijojaKaytetaanAlkaen;
    }

    public void setVarasijojaKaytetaanAlkaen(Date varasijojaKaytetaanAlkaen) {
        this.varasijojaKaytetaanAlkaen = varasijojaKaytetaanAlkaen;
    }

    public Date getVarasijojaTaytetaanAsti() {
        return varasijojaTaytetaanAsti;
    }

    public void setVarasijojaTaytetaanAsti(Date varasijojaTaytetaanAsti) {
        this.varasijojaTaytetaanAsti = varasijojaTaytetaanAsti;
    }

    public String getTayttojono() {
        return tayttojono;
    }

    public void setTayttojono(String tayttojono) {
        this.tayttojono = tayttojono;
    }

    public Long getSijoitteluajoId() {
        return sijoitteluajoId;
    }

    public void setSijoitteluajoId(Long sijoitteluajoId) {
        this.sijoitteluajoId = sijoitteluajoId;
    }

    public boolean empty() {
        return hakija == null || hakija.isEmpty();
    }

    @Override
    public String toString() {
        return "ValintatietoValintatapajonoDTO{" +
            "hakija=" + hakija +
            ", varasijat=" + varasijat +
            ", varasijaTayttoPaivat=" + varasijaTayttoPaivat +
            ", varasijojaKaytetaanAlkaen=" + varasijojaKaytetaanAlkaen +
            ", varasijojaTaytetaanAsti=" + varasijojaTaytetaanAsti +
            ", tayttojono='" + tayttojono + '\'' +
            ", sijoitteluajoId=" + sijoitteluajoId +
            '}';
    }
}
