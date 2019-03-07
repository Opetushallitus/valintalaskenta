package fi.vm.sade.valintalaskenta.laskenta;

import ch.qos.logback.access.jetty.RequestLogImpl;
import org.eclipse.jetty.server.Connector;
import org.eclipse.jetty.server.RequestLog;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.server.ServerConnector;
import org.eclipse.jetty.util.Jetty;
import org.eclipse.jetty.util.resource.Resource;
import org.eclipse.jetty.util.thread.QueuedThreadPool;
import org.eclipse.jetty.webapp.WebAppContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.Duration;

public class ValintaLaskentaLaskentaJetty {
    private static final int SERVICE_PORT_IN_ECS_CONFIGURATION = 8080;
    private static final Logger LOG = LoggerFactory.getLogger(ValintaLaskentaLaskentaJetty.class);

    public static void main(String[] args) {
        WebAppContext webAppContext = new WebAppContext();
        webAppContext.setBaseResource(Resource.newClassPathResource("/webapp"));
        start(webAppContext, createServer());
    }

    private static Server createServer() {
        int minThreads = 5;
        int maxThreads = 50;
        int idleThreadTimeoutMs = (int) Duration.ofMinutes(1).toMillis();
        QueuedThreadPool threadPool = new QueuedThreadPool(maxThreads, minThreads, idleThreadTimeoutMs);
        Server server = new Server(threadPool);
        ServerConnector serverConnector = new ServerConnector(server);
        serverConnector.setPort(SERVICE_PORT_IN_ECS_CONFIGURATION);
        server.setConnectors(new Connector[]{ serverConnector });
        return server;
    }

    public static void start(WebAppContext webAppContext, Server server) {
        try {
            if (server.isStopped()) {
                webAppContext.setContextPath("/valintalaskenta-laskenta-service");
                LOG.info(String.format("Starting Jetty %s at port %d for context %s to path %s",
                    Jetty.VERSION, ((ServerConnector) server.getConnectors()[0]).getPort(), webAppContext.getWar(), webAppContext.getContextPath()));
                webAppContext.setParentLoaderPriority(true);
                server.setHandler(webAppContext);
                server.setStopAtShutdown(true);
                server.setRequestLog(createAccessLogConfiguration());
                server.start();
            }
        } catch (Throwable t) {
            throw new RuntimeException(t);
        }
    }

    private static RequestLog createAccessLogConfiguration() {
        RequestLogImpl requestLog = new RequestLogImpl();
        String logbackAccess = System.getProperty("logback.access");
        if (logbackAccess != null) {
            requestLog.setFileName(logbackAccess);
        } else {
            LOG.warn("Jetty access log is printed to console, use -Dlogback.access=path/to/logback-access.xml to set configuration file");
            requestLog.setResource("/logback-access-to-stdout.xml");
        }
        requestLog.start();
        return requestLog;
    }
}
