package fi.vm.sade.valintalaskenta.laskenta;

import fi.vm.sade.jetty.OpintopolkuJetty;

import java.time.Duration;

public class ValintaLaskentaLaskentaJetty extends OpintopolkuJetty {
    public static final ValintaLaskentaLaskentaJetty JETTY = new ValintaLaskentaLaskentaJetty();
    public static final String CONTEXT_PATH = "/valintalaskenta-laskenta-service";

    public static void main(String[] args) {
        JETTY.start(CONTEXT_PATH);
    }
}
