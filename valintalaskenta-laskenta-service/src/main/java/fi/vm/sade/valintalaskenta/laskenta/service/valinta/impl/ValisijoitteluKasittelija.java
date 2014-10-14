package fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl;

import fi.vm.sade.valintalaskenta.domain.dto.LaskeDTO;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * Created by kjsaila on 21/09/14.
 */
@Component
public class ValisijoitteluKasittelija {

    public Pair<Set<Integer>, Map<String, List<String>>> valisijoiteltavatJonot(List<LaskeDTO> lista) {
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

        return new ImmutablePair<>(valinnanvaiheet, hakukohteet);

    }

}
