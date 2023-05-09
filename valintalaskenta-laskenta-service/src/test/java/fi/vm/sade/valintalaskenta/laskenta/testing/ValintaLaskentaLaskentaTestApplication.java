package fi.vm.sade.valintalaskenta.laskenta.testing;

import fi.vm.sade.java_utils.security.OpintopolkuSingleSignOutFilter;
import org.apache.cxf.transport.servlet.CXFServlet;
import org.jasig.cas.client.session.SingleSignOutHttpSessionListener;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ImportResource;
import org.springframework.web.WebApplicationInitializer;
import org.springframework.web.context.ContextLoaderListener;
import org.springframework.web.context.support.AnnotationConfigWebApplicationContext;
import org.springframework.web.servlet.DispatcherServlet;

import javax.servlet.*;
import java.util.EnumSet;

@SpringBootApplication
@ImportResource({"classpath:spring/application-context.xml"})
public class ValintaLaskentaLaskentaTestApplication implements WebApplicationInitializer {
    public static void main(final String[] args) {
        SpringApplication.run(ValintaLaskentaLaskentaTestApplication.class, args);
    }

    @Override
    public void onStartup(final ServletContext container) throws ServletException {
        final AnnotationConfigWebApplicationContext context = new AnnotationConfigWebApplicationContext();
        container.addListener(new ContextLoaderListener(context));
        container.addListener(new SingleSignOutHttpSessionListener());

        final FilterRegistration.Dynamic signOutFilter = container.addFilter("signOut", new OpintopolkuSingleSignOutFilter());
        signOutFilter.addMappingForUrlPatterns(EnumSet.of(DispatcherType.ASYNC), true, "/*");

        final ServletRegistration.Dynamic cxf = container.addServlet("cxf", new CXFServlet());
        cxf.addMapping("/resources/*");

        final ServletRegistration.Dynamic exporter = container.addServlet("exporter", new DispatcherServlet());
        exporter.addMapping("/export/*");
    }
}
