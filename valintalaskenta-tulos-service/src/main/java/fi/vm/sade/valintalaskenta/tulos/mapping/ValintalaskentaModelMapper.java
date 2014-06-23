package fi.vm.sade.valintalaskenta.tulos.mapping;

import java.util.ArrayList;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

import org.modelmapper.Converter;
import org.modelmapper.ModelMapper;
import org.modelmapper.PropertyMap;
import org.modelmapper.spi.MappingContext;

import fi.vm.sade.valintalaskenta.domain.dto.JarjestyskriteeritulosDTO;
import fi.vm.sade.valintalaskenta.domain.dto.JonosijaDTO;
import fi.vm.sade.valintalaskenta.domain.dto.ValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;

/**
 * User: wuoti Date: 9.12.2013 Time: 9.18
 */
public class ValintalaskentaModelMapper extends ModelMapper {

	public ValintalaskentaModelMapper() {
		super();
		this.addMappings(new PropertyMap<ValinnanvaiheDTO, Valinnanvaihe>() {
			protected void configure() {
				map().setValinnanvaiheOid(source.getValinnanvaiheoid());
			}
		});
		this.addMappings(new PropertyMap<JonosijaDTO, Jonosija>() {
			@Override
			protected void configure() {
				Converter<SortedSet<JarjestyskriteeritulosDTO>, List<Jarjestyskriteeritulos>> dtoToJarjestyskriteeritulosConverter = new Converter<SortedSet<JarjestyskriteeritulosDTO>, List<Jarjestyskriteeritulos>>() {
					public List<Jarjestyskriteeritulos> convert(
							MappingContext<SortedSet<JarjestyskriteeritulosDTO>, List<Jarjestyskriteeritulos>> context) {
						List<Jarjestyskriteeritulos> result = new ArrayList<Jarjestyskriteeritulos>();
						for (JarjestyskriteeritulosDTO arg : context
								.getSource()) {
							Jarjestyskriteeritulos j = new Jarjestyskriteeritulos();
							j.setArvo(arg.getArvo());
							j.setKuvaus(arg.getKuvaus());
							j.setNimi(arg.getNimi());
							j.setPrioriteetti(arg.getPrioriteetti());
							j.setTila(arg.getTila());
							result.add(j);
						}

						return result;
					}
				};

				using(dtoToJarjestyskriteeritulosConverter).map(
						source.getJarjestyskriteerit())
						.setJarjestyskriteeritulokset(null);

				map().setHakutoiveprioriteetti(source.getPrioriteetti());
			}
		});

		this.addMappings(new PropertyMap<Jonosija, JonosijaDTO>() {
			@Override
			protected void configure() {
				Converter<List<Jarjestyskriteeritulos>, SortedSet<JarjestyskriteeritulosDTO>> JarjestyskriteeritulosToDtoConverter = new Converter<List<Jarjestyskriteeritulos>, SortedSet<JarjestyskriteeritulosDTO>>() {
					public SortedSet<JarjestyskriteeritulosDTO> convert(
							MappingContext<List<Jarjestyskriteeritulos>, SortedSet<JarjestyskriteeritulosDTO>> context) {
						SortedSet<JarjestyskriteeritulosDTO> result = new TreeSet<JarjestyskriteeritulosDTO>();
						for (Jarjestyskriteeritulos arg : context.getSource()) {
							JarjestyskriteeritulosDTO j = new JarjestyskriteeritulosDTO();
							j.setArvo(arg.getArvo());
							j.setKuvaus(arg.getKuvaus());
							j.setNimi(arg.getNimi());
							j.setPrioriteetti(arg.getPrioriteetti());
							j.setTila(arg.getTila());
							result.add(j);
						}

						return result;
					}
				};

				using(JarjestyskriteeritulosToDtoConverter).map(
						source.getJarjestyskriteeritulokset())
						.setJarjestyskriteerit(null);

				map().setPrioriteetti(source.getHakutoiveprioriteetti());
			}
		});

	}

	public <FROM, TO> List<TO> mapList(List<FROM> list, final Class<TO> to) {

		List<TO> toList = new ArrayList<TO>();

		for (FROM f : list) {
			toList.add(map(f, to));
		}

		return toList;
	}

}
