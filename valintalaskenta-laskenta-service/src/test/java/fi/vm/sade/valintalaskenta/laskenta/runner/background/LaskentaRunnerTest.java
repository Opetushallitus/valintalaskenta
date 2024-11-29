package fi.vm.sade.valintalaskenta.laskenta.runner.background;

import fi.vm.sade.valintalaskenta.domain.dto.seuranta.*;
import fi.vm.sade.valintalaskenta.laskenta.dao.SeurantaDao;
import fi.vm.sade.valintalaskenta.runner.background.LaskentaRunner;
import java.time.Instant;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class LaskentaRunnerTest {

  static class TestSeurantaDAO implements SeurantaDao {
    @Override
    public TunnisteDto luoLaskenta(
        String userOID,
        String haunnimi,
        String nimi,
        String hakuOid,
        LaskentaTyyppi tyyppi,
        boolean erillishaku,
        Optional<Integer> valinnanvaihe,
        boolean valintakoelaskenta,
        Collection<HakukohdeDto> hakukohdeOids) {
      throw new UnsupportedOperationException();
    }

    @Override
    public Optional<LaskentaDto> haeLaskenta(String uuid) {
      throw new UnsupportedOperationException();
    }

    @Override
    public Optional<ImmutablePair<UUID, Collection<String>>> otaSeuraavatHakukohteetTyonAlle(
        String noodiId, int maxYhtaaikaisetHakukohteet) {
      throw new UnsupportedOperationException();
    }

    @Override
    public void peruutaLaskenta(String uuid, Optional<IlmoitusDto> ilmoitus) {
      throw new UnsupportedOperationException();
    }

    @Override
    public LaskentaDto resetoiLaskenta(String uuid, boolean nollaaIlmoitukset) {
      throw new UnsupportedOperationException();
    }

    @Override
    public void merkkaaHakukohteetValmiiksi(UUID uuid, Collection<String> hakukohdeOids) {
      throw new UnsupportedOperationException();
    }

    @Override
    public void merkkaaHakukohteetEpaonnistuneeksi(
        UUID uuid, Collection<String> hakukohdeOids, int maxYritykset, String message) {
      throw new UnsupportedOperationException();
    }

    @Override
    public void merkkaaNoodiLiveksi(String noodiId) {
      throw new UnsupportedOperationException();
    }

    @Override
    public void resetoiKuolleidenNoodienLaskennat(int viive) {
      throw new UnsupportedOperationException();
    }

    @Override
    public Optional<YhteenvetoDto> haeYhteenveto(String uuid) {
      throw new UnsupportedOperationException();
    }

    @Override
    public Collection<YhteenvetoDto> haeYhteenvetoKaikilleLaskennoille(Instant luotuAlkaen) {
      throw new UnsupportedOperationException();
    }

    @Override
    public void siivoa(Date viimeinenSailottavaPaivamaara) {
      throw new UnsupportedOperationException();
    }

    @Override
    public String lueParametri(String nimi) {
      throw new UnsupportedOperationException();
    }
  }

  private LaskentaDto getLaskentaDto(String uuid) {
    return new LaskentaDto(
        uuid.toString(),
        "",
        "",
        "",
        "hakuOid",
        Instant.now().toEpochMilli(),
        LaskentaTila.MENEILLAAN,
        LaskentaTyyppi.HAKUKOHDE,
        null,
        null,
        false,
        Optional.empty(),
        false,
        Optional.empty(),
        false) {
      @Override
      public List<HakukohdeDto> getHakukohteet() {
        throw new UnsupportedOperationException();
      }
    };
  }

  @Test
  public void testOnnistunutLaskenta() {
    UUID seuraavaUUID = UUID.randomUUID();
    Collection<String> seuraavatHakukohteet = List.of("hakukohdeOid1", "hakukohdeOid2");
    Collection<String> valmiitHakukohteet = new ArrayList<>();
    int rinnakkaisetHakukohteet = 34;

    LaskentaRunner laskentaRunner =
        new LaskentaRunner(
            (laskenta, hakukohdeOids) -> {
              // laskenta onnistuu, palalutetaan uuid
              return CompletableFuture.completedFuture(seuraavaUUID.toString());
            },
            new TestSeurantaDAO() {
              boolean started = false;

              @Override
              public String lueParametri(String nimi) {
                return rinnakkaisetHakukohteet + "";
              }

              @Override
              public Optional<ImmutablePair<UUID, Collection<String>>>
                  otaSeuraavatHakukohteetTyonAlle(String noodiId, int maxYhtaaikaisetHakukohteet) {
                if (started) return Optional.empty();
                started = true;
                Assertions.assertEquals(rinnakkaisetHakukohteet, maxYhtaaikaisetHakukohteet);
                return Optional.of(new ImmutablePair<>(seuraavaUUID, seuraavatHakukohteet));
              }

              @Override
              public Optional<LaskentaDto> haeLaskenta(String uuid) {
                Assertions.assertEquals(seuraavaUUID.toString(), uuid);
                return Optional.of(getLaskentaDto(uuid));
              }

              @Override
              public void merkkaaHakukohteetValmiiksi(UUID uuid, Collection<String> hakukohdeOids) {
                Assertions.assertEquals(seuraavaUUID, uuid);
                valmiitHakukohteet.addAll(hakukohdeOids);
              }
            });

    // ajetaan laskenta yhdelle satsille hakukohteita
    laskentaRunner.fetchAndStartHakukohteet().join();

    // hakukohteet merkitty valmiiksi
    Assertions.assertEquals(seuraavatHakukohteet, valmiitHakukohteet);
  }

  @Test
  public void testEpaonnistunutLaskenta() {
    UUID seuraavaUUID = UUID.randomUUID();
    Collection<String> seuraavatHakukohteet = List.of("hakukohdeOid1", "hakukohdeOid2");
    Collection<String> epaonnistuneetHakukohteet = new ArrayList<>();
    int rinnakkaisetHakukohteet = 34;

    LaskentaRunner laskentaRunner =
        new LaskentaRunner(
            (laskenta, hakukohdeOids) -> {
              // laskenta epäonnistuu virheeseen
              throw new RuntimeException("eeppinen katastrofi");
            },
            new TestSeurantaDAO() {
              boolean started = false;

              @Override
              public String lueParametri(String nimi) {
                return rinnakkaisetHakukohteet + "";
              }

              @Override
              public Optional<ImmutablePair<UUID, Collection<String>>>
                  otaSeuraavatHakukohteetTyonAlle(String noodiId, int maxYhtaaikaisetHakukohteet) {
                Assertions.assertEquals(rinnakkaisetHakukohteet, maxYhtaaikaisetHakukohteet);
                if (started) return Optional.empty();
                started = true;
                return Optional.of(new ImmutablePair<>(seuraavaUUID, seuraavatHakukohteet));
              }

              @Override
              public Optional<LaskentaDto> haeLaskenta(String uuid) {
                Assertions.assertEquals(seuraavaUUID.toString(), uuid);
                return Optional.of(getLaskentaDto(uuid));
              }

              @Override
              public void merkkaaHakukohteetEpaonnistuneeksi(
                  UUID uuid, Collection<String> hakukohdeOids, int maxYritykset, String message) {
                Assertions.assertEquals(seuraavaUUID, uuid);
                epaonnistuneetHakukohteet.addAll(hakukohdeOids);
              }
            });

    // ajetaan laskenta yhdelle satsille hakukohteita
    laskentaRunner.fetchAndStartHakukohteet().join();

    // hakukohteet merkitty epäonnistuneiksi
    Assertions.assertEquals(seuraavatHakukohteet, epaonnistuneetHakukohteet);
  }
}
