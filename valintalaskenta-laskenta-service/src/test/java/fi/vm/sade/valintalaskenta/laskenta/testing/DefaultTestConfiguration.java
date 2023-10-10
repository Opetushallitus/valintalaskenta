package fi.vm.sade.valintalaskenta.laskenta.testing;

import static java.lang.Integer.parseInt;

import fi.vm.sade.javautils.opintopolku_spring_security.Authorizer;
import fi.vm.sade.valintalaskenta.laskenta.App;
import fi.vm.sade.valintalaskenta.laskenta.config.SwaggerConfiguration;
import fi.vm.sade.valintalaskenta.laskenta.resource.ValintalaskentaPaloissaResourceImpl;
import fi.vm.sade.valintalaskenta.laskenta.resource.ValintalaskentaResourceImpl;
import fi.vm.sade.valintalaskenta.laskenta.resource.external.ErillisSijoitteluResource;
import fi.vm.sade.valintalaskenta.laskenta.resource.external.ValiSijoitteluResource;
import fi.vm.sade.valintalaskenta.laskenta.resource.external.ValintaperusteetValintatapajonoResource;
import fi.vm.sade.valintalaskenta.laskenta.service.ValintalaskentaService;
import fi.vm.sade.valintalaskenta.laskenta.service.impl.ValintalaskentaServiceImpl;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.ValisijoitteluKasittelija;
import fi.vm.sade.valintalaskenta.laskenta.testing.TestApp;
import fi.vm.sade.valintalaskenta.tulos.logging.LaskentaAuditLogMock;
import io.swagger.v3.oas.models.OpenAPI;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.boot.web.embedded.tomcat.TomcatServletWebServerFactory;
import org.springframework.boot.web.server.WebServerFactoryCustomizer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.data.jdbc.repository.config.AbstractJdbcConfiguration;
import org.springframework.data.jdbc.repository.config.EnableJdbcRepositories;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcOperations;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;
import org.springframework.security.cas.authentication.CasAuthenticationProvider;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.stereotype.Component;
import org.springframework.transaction.TransactionManager;

import javax.sql.DataSource;


public class DefaultTestConfiguration {



}
