package fi.vm.sade.valintalaskenta.laskenta.testing;

import fi.vm.sade.integrationtest.util.PortChecker;
import fi.vm.sade.integrationtest.util.ProjectRootFinder;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.webapp.WebAppContext;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;

public class ValintaLaskentaLaskentaJetty {
    public final static int port = Integer.parseInt(System.getProperty("valintalaskenta-laskenta.port", String.valueOf(PortChecker.findFreeLocalPort())));
    public static final String rootUrl = "http://localhost:" + port + "/valintalaskenta-laskenta-service/resources";

    private static Server server = new Server(port);

    public static void main(String[] args) throws Exception{
        startShared();
    }

    public static void startShared() {
        startShared(true);
    }

    public static void startShared(boolean useMocks) {
        try {
            if (server.isStopped()) {
                String root =  ProjectRootFinder.findProjectRoot() + "/valintalaskenta-laskenta-service";
                WebAppContext wac = new WebAppContext();
                if (useMocks) {
                    wac.setResourceBase(root);
                    wac.setDescriptor(root + "/src/test/resources/it-profile-web.xml");
                } else {
                    // This has not been tested. Use at your own risk.
                    wac.setResourceBase(root + "/src/main/webapp");
                }

                wac.setContextPath("/valintalaskenta-laskenta-service");
                wac.setParentLoaderPriority(true);
                server.setHandler(wac);
                server.setStopAtShutdown(true);
                System.setProperty("ValintaLaskentaLaskentaJetty.server.rootUrl", rootUrl);
                server.start();
            }
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
