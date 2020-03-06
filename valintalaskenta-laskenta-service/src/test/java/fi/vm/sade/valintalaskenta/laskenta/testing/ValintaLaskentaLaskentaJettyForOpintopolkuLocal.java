package fi.vm.sade.valintalaskenta.laskenta.testing;

import fi.vm.sade.jetty.OpintopolkuJetty;
import fi.vm.sade.valintalaskenta.laskenta.ValintaLaskentaLaskentaJetty;

import java.time.Duration;

/**
 * Normaali käyttö: https://github.com/Opetushallitus/local-environment
 */
public class ValintaLaskentaLaskentaJettyForOpintopolkuLocal extends OpintopolkuJetty {
    public final static int port = Integer.parseInt(System.getProperty("valintalaskenta.port", "8081"));

    public static void main(String... args) {
        new ValintaLaskentaLaskentaJettyForOpintopolkuLocal().start();
    }

    private void start() {
        start(ValintaLaskentaLaskentaJetty.CONTEXT_PATH, port, 5, 10, Duration.ofMinutes(1));
    }
}
