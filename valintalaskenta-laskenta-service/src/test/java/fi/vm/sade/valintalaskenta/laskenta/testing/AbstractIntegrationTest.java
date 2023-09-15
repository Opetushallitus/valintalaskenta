package fi.vm.sade.valintalaskenta.laskenta.testing;

import fi.vm.sade.valintalaskenta.laskenta.App;
import fi.vm.sade.valintalaskenta.laskenta.testing.DefaultTestConfiguration;
import fi.vm.sade.valintalaskenta.laskenta.testing.TestApp;
import fi.vm.sade.valintalaskenta.tulos.logging.LaskentaAuditLog;
import fi.vm.sade.valintalaskenta.tulos.logging.LaskentaAuditLogMock;
import org.junit.After;
import org.junit.Before;
import org.junit.runner.RunWith;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.*;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestExecutionListeners;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.support.DependencyInjectionTestExecutionListener;
import org.springframework.test.context.support.DirtiesContextTestExecutionListener;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.NONE)
//@Import(DefaultTestConfiguration.class)
//@ActiveProfiles("test")
//@ContextConfiguration(locations = "classpath:application-context-test.xml")
@RunWith(SpringJUnit4ClassRunner.class)
/*@TestExecutionListeners(
        listeners = {
                DependencyInjectionTestExecutionListener.class,
                DirtiesContextTestExecutionListener.class
        })*/
@EnableWebSecurity(debug = true)
@EnableJpaRepositories(basePackages = "fi.vm.sade.valintalaskenta.laskenta.dao.repository")
@EntityScan(basePackages = "fi.vm.sade.valintalaskenta.domain.*")
@ComponentScan(basePackages = {"fi.vm.sade.valintalaskenta.laskenta.*"},
        excludeFilters = {
                @ComponentScan.Filter(type = FilterType.REGEX, pattern = "fi.vm.sade.valintalaskenta.laskenta.config.*"),
                @ComponentScan.Filter(type = FilterType.ASSIGNABLE_TYPE, classes = App.class)
        })
public abstract class AbstractIntegrationTest {

}
