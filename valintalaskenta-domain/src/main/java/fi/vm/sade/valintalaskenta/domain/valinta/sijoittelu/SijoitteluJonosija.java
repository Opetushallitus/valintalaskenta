package fi.vm.sade.valintalaskenta.domain.valinta.sijoittelu;

import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteeritulosContainer;
import fi.vm.sade.valintalaskenta.domain.valinta.SyotettyArvoContainer;
import java.util.UUID;

public class SijoitteluJonosija {

  // Jonosijan tiedot

  public UUID jonosijaId;

  public String hakemusOid;

  public String hakijaOid;

  public SyotettyArvoContainer syotetytArvot;

  public JarjestyskriteeritulosContainer jarjestyskriteeritulokset;

  public int hakutoiveprioriteetti;

  public Boolean harkinnanvarainen;

  public Boolean hylattyValisijoittelussa;

  public UUID valintatapajono;
}
