package fi.vm.sade.valintalaskenta.domain.dto;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;


import com.wordnik.swagger.annotations.ApiModel;
import com.wordnik.swagger.annotations.ApiModelProperty;

import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila;

/**
 * Created with IntelliJ IDEA. User: kkammone Date: 13.5.2013 Time: 9:50 To
 * change this template use File | Settings | File Templates.
 */
@ApiModel(value = "JonosijaDTO", description = "Jonosija")
public class JonosijaDTO {

	@ApiModelProperty(value = "Jonosijanumero", required = true)
	private int jonosija;

	@ApiModelProperty(value = "Hakemus OID", required = true)
	private String hakemusOid;

	@ApiModelProperty(value = "Hakija OID")
	private String hakijaOid;

	@ApiModelProperty(value = "Järjestyskriteerit", required = true)
	private SortedSet<JarjestyskriteeritulosDTO> jarjestyskriteerit = new TreeSet<JarjestyskriteeritulosDTO>();

	@ApiModelProperty(value = "Prioriteetti", required = true)
	private int prioriteetti;

	@ApiModelProperty(value = "Sukunimi")
	private String sukunimi;

	@ApiModelProperty(value = "Etunimi")
	private String etunimi;

	@ApiModelProperty(value = "Onko hakija hakenut harkinnanvaraisesti")
	private boolean harkinnanvarainen = false;

	@ApiModelProperty(value = "Jonosijan tila", required = true)
	private JarjestyskriteerituloksenTila tuloksenTila;

	@ApiModelProperty(value = "Jonosijan historiat")
	private List<String> historiat;

	@ApiModelProperty(value = "Jonosijan syötetyt arvot")
	private List<SyotettyArvoDTO> syotetytArvot = new ArrayList<SyotettyArvoDTO>();

	@ApiModelProperty(value = "Jonosijan tallennetut funktiotulokset")
	private List<FunktioTulosDTO> funktioTulokset = new ArrayList<FunktioTulosDTO>();

	@ApiModelProperty(value = "Onko jonosijaa muokattu manuaalisesti")
	private boolean muokattu = false;

	public List<String> getHistoriat() {
		return historiat;
	}

	public void setHistoriat(List<String> historiat) {
		this.historiat = historiat;
	}

	public SortedSet<JarjestyskriteeritulosDTO> getJarjestyskriteerit() {
		return jarjestyskriteerit;
	}

	public void setJarjestyskriteerit(
			SortedSet<JarjestyskriteeritulosDTO> jarjestyskriteerit) {
		this.jarjestyskriteerit = jarjestyskriteerit;
	}

	public String getHakijaOid() {
		return hakijaOid;
	}

	public void setHakijaOid(String hakijaOid) {
		this.hakijaOid = hakijaOid;
	}

	public String getHakemusOid() {
		return hakemusOid;
	}

	public void setHakemusOid(String hakemusOid) {
		this.hakemusOid = hakemusOid;
	}

	public int getJonosija() {
		return jonosija;
	}

	public void setJonosija(int jonosija) {
		this.jonosija = jonosija;
	}

	public void setEtunimi(String etunimi) {
		this.etunimi = etunimi;
	}

	public void setSukunimi(String sukunimi) {
		this.sukunimi = sukunimi;
	}

	public void setPrioriteetti(int prioriteetti) {
		this.prioriteetti = prioriteetti;
	}

	public int getPrioriteetti() {
		return prioriteetti;
	}

	public String getSukunimi() {
		return sukunimi;
	}

	public String getEtunimi() {
		return etunimi;
	}

	public JarjestyskriteerituloksenTila getTuloksenTila() {
		return tuloksenTila;
	}

	public void setTuloksenTila(JarjestyskriteerituloksenTila tuloksenTila) {
		this.tuloksenTila = tuloksenTila;
	}

	public boolean isHarkinnanvarainen() {
		return harkinnanvarainen;
	}

	public void setHarkinnanvarainen(boolean harkinnanvarainen) {
		this.harkinnanvarainen = harkinnanvarainen;
	}

	public boolean isMuokattu() {
		return muokattu;
	}

	public void setMuokattu(boolean muokattu) {
		this.muokattu = muokattu;
	}

	public List<SyotettyArvoDTO> getSyotetytArvot() {
		return syotetytArvot;
	}

	public void setSyotetytArvot(List<SyotettyArvoDTO> syotetytArvot) {
		this.syotetytArvot = syotetytArvot;
	}

	public List<FunktioTulosDTO> getFunktioTulokset() {
		return funktioTulokset;
	}

	public void setFunktioTulokset(List<FunktioTulosDTO> funktioTulokset) {
		this.funktioTulokset = funktioTulokset;
	}
}