package fi.vm.sade.valintalaskenta.domain.dto;

import java.math.BigDecimal;
import java.util.Map;

import com.wordnik.swagger.annotations.ApiModel;
import com.wordnik.swagger.annotations.ApiModelProperty;

import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila;

/**
 * @author Jussi Jartamo
 */
@ApiModel(value = "JarjestyskriteeritulosDTO", description = "Järjestyskriteerin tulos")
public class JarjestyskriteeritulosDTO implements
		Comparable<JarjestyskriteeritulosDTO> {

	@ApiModelProperty(value = "Järjestyskriteerin lukuarvo", required = true)
	private BigDecimal arvo;

	@ApiModelProperty(value = "Järjestyskriteerin tila", required = true)
	private JarjestyskriteerituloksenTila tila;

	@ApiModelProperty(value = "Monikielinen kuvaus (esim. hylkäyksen syy)")
	private Map<String, String> kuvaus;

	@ApiModelProperty(value = "Järjestyskriteerin prioriteetti", required = true)
	private int prioriteetti;

	@ApiModelProperty(value = "Järjestyskriteerin nimi")
	private String nimi;

	@Override
	public int compareTo(JarjestyskriteeritulosDTO o) {
		return Integer.valueOf(prioriteetti).compareTo(o.getPrioriteetti());
	}

	public BigDecimal getArvo() {
		return arvo;
	}

	public void setArvo(BigDecimal arvo) {
		this.arvo = arvo;
	}

	public JarjestyskriteerituloksenTila getTila() {
		return tila;
	}

	public void setTila(JarjestyskriteerituloksenTila tila) {
		this.tila = tila;
	}

	public int getPrioriteetti() {
		return prioriteetti;
	}

	public void setPrioriteetti(int prioriteetti) {
		this.prioriteetti = prioriteetti;
	}

	public String getNimi() {
		return nimi;
	}

	public void setNimi(String nimi) {
		this.nimi = nimi;
	}

	public Map<String, String> getKuvaus() {
		return kuvaus;
	}

	public void setKuvaus(Map<String, String> kuvaus) {
		this.kuvaus = kuvaus;
	}
}