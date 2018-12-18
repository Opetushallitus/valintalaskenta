package fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

import org.springframework.stereotype.Component;

import fi.vm.sade.valintalaskenta.domain.dto.LaskeDTO;
import org.springframework.util.StopWatch;

@Component
public class ValisijoitteluKasittelija {
    public static final class ValisijoiteltavatJonot {
        public final Set<Integer> valinnanvaiheet;
        public final Map<String, List<String>> jonot;

        public ValisijoiteltavatJonot(final Set<Integer> valinnanvaiheet, final Map<String, List<String>> jonot) {
            this.valinnanvaiheet = valinnanvaiheet;
            this.jonot = jonot;
        }
    }
    public ValisijoiteltavatJonot valisijoiteltavatJonot(List<LaskeDTO> lista, StopWatch stopWatch) {
        stopWatch.start("Muodostetaan välisijoiteltavien jonojen lista");
        Map<String, List<String>> hakukohteet = new ConcurrentHashMap<>();
        Set<Integer> valinnanvaiheet = new TreeSet<>();
        lista.forEach(dto -> {
            List<String> jonot = new ArrayList<>();
            dto.getValintaperuste().stream()
                    .filter(p -> !p.getValinnanVaihe().getValintatapajono().isEmpty() && p.getValinnanVaihe().getAktiivinen())
                    .forEach(peruste -> {
                        List<String> collect = peruste.getValinnanVaihe().getValintatapajono().stream()
                                .filter(j -> j.getValisijoittelu()).map(j -> j.getOid()).collect(Collectors.toList());

                        if(!collect.isEmpty()) {
                            peruste.getValinnanVaihe().getValintatapajono().stream()
                                    .filter(j -> j.getValisijoittelu()).forEach(j -> valinnanvaiheet.add(peruste.getValinnanVaihe().getValinnanVaiheJarjestysluku()));
                            jonot.addAll(collect);
                        }

                    }
                    );

            if (!jonot.isEmpty()) {
                hakukohteet.putIfAbsent(dto.getHakukohdeOid(), jonot);
            }
        });
        stopWatch.stop();
        return new ValisijoiteltavatJonot(valinnanvaiheet, hakukohteet);

    }
}
