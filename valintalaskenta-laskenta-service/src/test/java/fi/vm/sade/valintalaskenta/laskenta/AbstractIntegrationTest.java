package fi.vm.sade.valintalaskenta.laskenta;

import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;

@SpringBootTest(
        classes = App.class,
        webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT,
        properties = { "spring.datasource.url=jdbc:tc:postgresql:11-alpine:///it-valintalaskenta" }
)
@ActiveProfiles("test")
public abstract class AbstractIntegrationTest {
}
