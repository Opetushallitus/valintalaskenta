package fi.vm.sade.valintalaskenta.domain.dto;

import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetHakijaryhmaDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by jukais on 26.3.2014.
 */
public class LaskeDTO {
	private List<HakemusDTO> hakemus;
	private List<ValintaperusteetDTO> valintaperuste;
    private List<ValintaperusteetHakijaryhmaDTO> hakijaryhmat = new ArrayList<ValintaperusteetHakijaryhmaDTO>();

	public LaskeDTO() {
		this.hakemus = null;
		this.valintaperuste = null;
	}

    public LaskeDTO(List<HakemusDTO> hakemus,
                    List<ValintaperusteetDTO> valintaperuste) {
        this.hakemus = hakemus;
        this.valintaperuste = valintaperuste;
    }

	public LaskeDTO(List<HakemusDTO> hakemus,
			List<ValintaperusteetDTO> valintaperuste, List<ValintaperusteetHakijaryhmaDTO> hakijaryhmat) {
		this.hakemus = hakemus;
		this.valintaperuste = valintaperuste;
        this.hakijaryhmat = hakijaryhmat;
	}

	public List<HakemusDTO> getHakemus() {
		if (hakemus == null) {
			hakemus = new ArrayList<HakemusDTO>();
		}
		return hakemus;
	}

	public void setHakemus(List<HakemusDTO> hakemus) {
		this.hakemus = hakemus;
	}

	public List<ValintaperusteetDTO> getValintaperuste() {
		if (valintaperuste == null) {
			valintaperuste = new ArrayList<ValintaperusteetDTO>();
		}
		return valintaperuste;
	}

	public void setValintaperuste(List<ValintaperusteetDTO> valintaperuste) {
		this.valintaperuste = valintaperuste;
	}

    public List<ValintaperusteetHakijaryhmaDTO> getHakijaryhmat() {
        return hakijaryhmat;
    }

    public void setHakijaryhmat(List<ValintaperusteetHakijaryhmaDTO> hakijaryhmat) {
        this.hakijaryhmat = hakijaryhmat;
    }
}
