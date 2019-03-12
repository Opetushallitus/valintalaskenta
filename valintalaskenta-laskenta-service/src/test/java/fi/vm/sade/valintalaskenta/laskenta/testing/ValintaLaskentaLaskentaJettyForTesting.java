package fi.vm.sade.valintalaskenta.laskenta.testing;

import fi.vm.sade.integrationtest.util.PortChecker;
import fi.vm.sade.integrationtest.util.ProjectRootFinder;
import fi.vm.sade.valintalaskenta.laskenta.ValintaLaskentaLaskentaJetty;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.webapp.WebAppContext;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;

public class ValintaLaskentaLaskentaJettyForTesting {
    public final static int port = Integer.parseInt(System.getProperty("valintalaskenta-laskenta.port", String.valueOf(PortChecker.findFreeLocalPort())));
    public static final String rootUrl = "http://localhost:" + port + "/valintalaskenta-laskenta-service/resources";

    private static Server server = new Server(port);

    public static void main(String[] args) {
        startShared();
    }

    public static void startShared() {
        try {
            String root =  ProjectRootFinder.findProjectRoot() + "/valintalaskenta-laskenta-service";
            WebAppContext wac = new WebAppContext();
            wac.setResourceBase(root);
            wac.setDescriptor(root + "/src/test/resources/it-profile-web.xml");
            System.setProperty("ValintaLaskentaLaskentaJetty.server.rootUrl", rootUrl);
            ValintaLaskentaLaskentaJetty.JETTY.start(wac, server, ValintaLaskentaLaskentaJetty.CONTEXT_PATH);
        } catch (Throwable t) {
            throw new RuntimeException(t);
        }
    }


    public static class ApplicationContextGetter implements ApplicationContextAware {
        public static ApplicationContext applicationContext;

        @Override
        public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
            ApplicationContextGetter.applicationContext = applicationContext;
        }
    }
}
