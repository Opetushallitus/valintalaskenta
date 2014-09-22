package fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl;

import fi.vm.sade.valintalaskenta.domain.dto.LaskeDTO;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * Created by kjsaila on 21/09/14.
 */
@Component
public class ValisijoitteluKasittelija {

    public Map<String, List<String>> valisijoiteltavatJonot(List<LaskeDTO> lista) {
        Map<String, List<String>> hakukohteet = new ConcurrentHashMap<>();
        lista.parallelStream().forEach(dto -> {
            List<String> jonot = new ArrayList<>();
            dto.getValintaperuste().stream()
                    .filter(p -> !p.getValinnanVaihe().getValintatapajono().isEmpty() && p.getValinnanVaihe().getAktiivinen())
                    .forEach(peruste -> {
                        jonot.addAll(peruste.getValinnanVaihe().getValintatapajono().stream()
                                .filter(j -> j.getValisijoittelu()).map(j -> j.getOid()).collect(Collectors.toList()));
                    }
                    );

            if (!jonot.isEmpty()) {
                hakukohteet.putIfAbsent(dto.getHakukohdeOid(), jonot);
            }
        });

        return hakukohteet;
    }

}
