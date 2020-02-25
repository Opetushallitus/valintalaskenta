package fi.vm.sade.valintalaskenta.domain.dto;

import java.util.HashMap;
import java.util.Map;

public class SuoritustiedotDTO {
    private Map<String, String> koskiOpiskeluoikeudetJsonitOppijanumeroittain = new HashMap<>();

    public boolean onKoskiopiskeluoikeudet(String oppijanumero) {
        return koskiOpiskeluoikeudetJsonitOppijanumeroittain.containsKey(oppijanumero);
    }

    public void asetaKoskiopiskeluoikeudet(String oppijanumero, String koskiOpiskeluoikeudetJson) {
        if (koskiOpiskeluoikeudetJsonitOppijanumeroittain.containsKey(oppijanumero)) {
            throw new IllegalArgumentException(String.format("Yritettiin asettaa koskiopiskeluoikeudet uudestaan oppijanumerolle %s , vaikka data löytyi jo. Vaikuttaa bugilta.", oppijanumero));
        }
        koskiOpiskeluoikeudetJsonitOppijanumeroittain.put(oppijanumero, koskiOpiskeluoikeudetJson);
    }

    public String haeKoskiOpiskeluoikeudetJson(String oppijanumero) {
        return koskiOpiskeluoikeudetJsonitOppijanumeroittain.get(oppijanumero);
    }

    /**
     * Jackson-deserialisointi edellyttää tämän.
     */
    public Map<String, String> getKoskiOpiskeluoikeudetJsonitOppijanumeroittain() {
        return koskiOpiskeluoikeudetJsonitOppijanumeroittain;
    }
}
