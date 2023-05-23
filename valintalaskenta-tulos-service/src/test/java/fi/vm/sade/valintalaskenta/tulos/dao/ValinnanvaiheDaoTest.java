package fi.vm.sade.valintalaskenta.tulos.dao;

import static com.lordofthejars.nosqlunit.mongodb.MongoDbRule.MongoDbRuleBuilder.newMongoDbRule;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import com.lordofthejars.nosqlunit.annotation.UsingDataSet;
import com.lordofthejars.nosqlunit.core.LoadStrategyEnum;
import com.lordofthejars.nosqlunit.mongodb.MongoDbRule;
import fi.vm.sade.javautils.opintopolku_spring_security.Authorizer;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import org.junit.Rule;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.migrationsupport.rules.EnableRuleMigrationSupport;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

/** Created by kjsaila on 11/03/15. */
@ContextConfiguration(locations = "classpath:application-context-test.xml")
@ExtendWith(SpringExtension.class)
@UsingDataSet
@EnableRuleMigrationSupport
public class ValinnanvaiheDaoTest {

  @Autowired ValinnanvaiheDAO valinnanvaiheDAO;

  @Autowired private ApplicationContext applicationContext;

  @Bean
  public Authorizer authorizer() {
    return Mockito.mock(Authorizer.class);
  }

  @Rule public MongoDbRule mongoDbRule = newMongoDbRule().defaultSpringMongoDb("test");

  @Test
  @UsingDataSet(locations = "valinnanvaiheJono.json", loadStrategy = LoadStrategyEnum.CLEAN_INSERT)
  public void testHaeTuloksetValinnantapajonolle() {
    {
      Valinnanvaihe vaihe = valinnanvaiheDAO.findByValintatapajonoOid("jono1");

      assertNotNull(vaihe);
      assertEquals("vaihe1", vaihe.getValinnanvaiheOid());
    }
  }
}
