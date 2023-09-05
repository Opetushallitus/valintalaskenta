package fi.vm.sade.valintalaskenta.laskenta.service.it;

import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.isEmptyOrNullString;
import static org.hamcrest.Matchers.not;
import static org.junit.Assert.assertThat;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValinnanvaiheDAO;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import javax.persistence.Entity;

/**
 * A little utility for dumping valintalaskentadb contents to a form that can be easily used for
 * test fixtures. Add valintalaskentadb URL to run configuration VM parameters, e.g. <code>
 * -ea -Xmx7G -DVIRKAILIJAMONGO_URL=mongodb://oph:PASSWORD@qa-mongodb1.oph.ware.fi,qa-mongodb2.oph.ware.fi,qa-mongodb3.oph.ware.fi:27017
 * </code> Note that the JSONs produced might still need some manual fixing.
 */
@ContextConfiguration(locations = ValintalaskentaDbDumpingTest.SPRING_CONFIG_XML)
@RunWith(SpringJUnit4ClassRunner.class)
public class ValintalaskentaDbDumpingTest {
  protected static final String SPRING_CONFIG_XML =
      "classpath:fi/vm/sade/valintalaskenta/laskenta/service/it/test-valintalaskentadb-access.xml";

  private final Gson gson = new Gson();

  @Autowired private ApplicationContext applicationContext;

  @Autowired private ValinnanvaiheDAO valinnanvaiheDAO;

  /** The real method used for dumping the data */
  public static void main(String... args) {
    ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext(SPRING_CONFIG_XML);
    ValintalaskentaDbDumpingTest dumper =
        (ValintalaskentaDbDumpingTest) context.getBean("valintalaskentaDbDumper");
    OidsToDump oidsToDump =
        OidsToDump.withHakuOid("1.2.246.562.29.59856749474")
            .withHakukohdeOids(
                "1.2.246.562.20.29592509383",
                "1.2.246.562.20.38970489639",
                "1.2.246.562.20.68517235666",
                "1.2.246.562.20.70074466049",
                "1.2.246.562.20.72034194232",
                "1.2.246.562.20.74774856426",
                "1.2.246.562.20.80972757381",
                "1.2.246.562.20.83571262399",
                "1.2.246.562.20.982230627410")
            .withHakemusOids("1.2.246.562.11.00009176948", "1.2.246.562.11.00009584734")
            .build();
    System.out.println(dumper.dumpToJson(oidsToDump));
  }

  @Test
  public void smokeTest() {
    OidsToDump oidsInVoidaanHyvaksyaJson =
        OidsToDump.withHakuOid("1.2.246.562.29.173465377510")
            .withHakukohdeOids(
                "1.2.246.562.20.51939036631",
                "1.2.246.562.20.64586301414",
                "1.2.246.562.20.66128426039",
                "1.2.246.562.20.66128426039",
                "1.2.246.562.20.68163578682",
                "1.2.246.562.20.68508673735",
                "1.2.246.562.20.70883151881",
                "1.2.246.562.20.73318431647",
                "1.2.246.562.20.75182408387",
                "1.2.246.562.20.90332205401",
                "1.2.246.562.20.92384321318",
                "1.2.246.562.20.93258129167")
            .withHakemusOids("1.2.246.562.11.00001212279", "1.2.246.562.11.00001223556")
            .build();
    DumpContents valintalaskentaResults = dumpData(oidsInVoidaanHyvaksyaJson);
    assertThat(valintalaskentaResults.jonosijat.size(), greaterThan(2));
    String json = dumpToJson(oidsInVoidaanHyvaksyaJson);
    assertThat(json, not(isEmptyOrNullString()));
  }

  private String dumpToJson(OidsToDump oidsToDump) {
    DumpContents dumpContents = dumpData(oidsToDump);
    return gson.toJson(dumpContents.toJson()).replaceAll("\"id\":", "\"_id\":");
  }

  public DumpContents dumpData(OidsToDump oidsToDump) {
    List<Valinnanvaihe> valinnanvaihes = new LinkedList<>();
    for (int jarjestysNumero = 0; jarjestysNumero < 10; jarjestysNumero++) {
      for (String hakukohdeOid : oidsToDump.hakukohdeOids) {
        valinnanvaihes.addAll(
            valinnanvaiheDAO.haeValinnanvaiheetJarjestysnumerolla(
                oidsToDump.hakuOid, hakukohdeOid, jarjestysNumero));
      }
    }
    cleanUpForPrinting(valinnanvaihes, oidsToDump.hakemusOids);

    List<Valintatapajono> jonot =
        valinnanvaihes.stream()
            .flatMap(vv -> vv.getValintatapajonot().stream())
            .collect(Collectors.toList());
    List<Jonosija> jonosijat =
        jonot.stream().flatMap(j -> j.getJonosijat().stream()).collect(Collectors.toList());

    jonot.forEach(j -> j.getJonosijat().clear());

    return new DumpContents(gson, valinnanvaihes, jonot, jonosijat);
  }

  private String collectionName(Class<?> clazz) {
    return clazz.getAnnotation(Entity.class).name();
  }

  private void cleanUpForPrinting(List<Valinnanvaihe> valinnanvaihes, List<String> hakemusOids) {
    valinnanvaihes.forEach(
        vaihe -> {
          vaihe.setCreatedAt(null);
          Set<Valintatapajono> sailytettavatJonot = new HashSet<>();
          vaihe
              .getValintatapajonot()
              .forEach(
                  jono -> {
                    List<Jonosija> sailytettavatJonosijat = new LinkedList<>();
                    List<String> sailytettavatJonosijaIdt = new LinkedList<>();
                    jono.getJonosijat()
                        .forEach(
                            jonosija -> {
                              if (hakemusOids.contains(jonosija.getHakemusOid())) {
                                sailytettavatJonosijat.add(jonosija);
                                sailytettavatJonot.add(jono);
                              }
                            });
                    jono.getJonosijaIdt()
                        .forEach(
                            id -> {
                              if (sailytettavatJonosijat.stream()
                                  .anyMatch(sailytettava -> sailytettava.getId().equals(id))) {
                                sailytettavatJonosijaIdt.add(id);
                              }
                            });
                    jono.setJonosijat(sailytettavatJonosijat);
                    jono.setJonosijaIdt(sailytettavatJonosijaIdt);
                  });
          vaihe.setValintatapajonot(new ArrayList<>(sailytettavatJonot));
        });
  }

  public class DumpContents {
    private final JsonArray valinnanvaiheet;
    private final JsonArray valintatapajonot;
    private final JsonArray jonosijat;

    public DumpContents(
        Gson gson,
        List<Valinnanvaihe> valinnanvaiheet,
        List<Valintatapajono> valintatapajonot,
        List<Jonosija> jonosijat) {
      this.valintatapajonot = (JsonArray) gson.toJsonTree(valintatapajonot);
      this.jonosijat = (JsonArray) gson.toJsonTree(jonosijat);
      JsonArray valinnanvaiheJson = (JsonArray) gson.toJsonTree(valinnanvaiheet);
      valinnanvaiheJson.forEach(replaceValintatapajonoSubElementsWithDbRefs());
      this.valinnanvaiheet = valinnanvaiheJson;
    }

    public JsonElement toJson() {
      JsonObject byCollectionName = new JsonObject();
      byCollectionName.add(collectionName(Valinnanvaihe.class), valinnanvaiheet);
      byCollectionName.add(collectionName(Valintatapajono.class), valintatapajonot);
      byCollectionName.add(collectionName(Jonosija.class), jonosijat);
      return byCollectionName;
    }

    private Consumer<JsonElement> replaceValintatapajonoSubElementsWithDbRefs() {
      return vaiheElement -> {
        JsonObject vaiheObject = vaiheElement.getAsJsonObject();
        JsonArray vaiheenJonot = vaiheObject.get("valintatapajonot").getAsJsonArray();
        JsonArray jonotPelkkienRefienKanssa = new JsonArray();
        vaiheenJonot.forEach(
            jonoElement -> {
              JsonObject jonoRefEelement = new JsonObject();
              jonoRefEelement.add("$ref", new JsonPrimitive(collectionName(Valintatapajono.class)));
              jonoRefEelement.add("$id", jonoElement.getAsJsonObject().get("id"));
              jonotPelkkienRefienKanssa.add(jonoRefEelement);
            });
        vaiheObject.remove("valintatapajonot");
        vaiheObject.add("valintatapajonot", jonotPelkkienRefienKanssa);
      };
    }
  }

  public static class OidsToDump {
    public final String hakuOid;
    public final List<String> hakukohdeOids;
    public final List<String> hakemusOids;

    public OidsToDump(String hakuOid, List<String> hakukohdeOids, List<String> hakemusOids) {
      this.hakuOid = hakuOid;
      this.hakukohdeOids = hakukohdeOids;
      this.hakemusOids = hakemusOids;
    }

    public static OidsToDump.Builder withHakuOid(String hakuOid) {
      return new OidsToDump.Builder(hakuOid);
    }

    public static class Builder {
      private final String hakuOid;
      private String[] hakukohdeOids;
      private String[] hakemusOids;

      public Builder(String hakuOid) {
        this.hakuOid = hakuOid;
      }

      public Builder withHakukohdeOids(String... hakukohdeOids) {
        this.hakukohdeOids = hakukohdeOids;
        return this;
      }

      public Builder withHakemusOids(String... hakemusOids) {
        this.hakemusOids = hakemusOids;
        return this;
      }

      public OidsToDump build() {
        return new OidsToDump(hakuOid, Arrays.asList(hakukohdeOids), Arrays.asList(hakemusOids));
      }
    }
  }
}
