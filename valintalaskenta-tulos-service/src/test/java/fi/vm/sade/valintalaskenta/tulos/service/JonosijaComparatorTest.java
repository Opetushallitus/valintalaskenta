package fi.vm.sade.valintalaskenta.tulos.service;

import static org.junit.jupiter.api.Assertions.assertEquals;

import fi.vm.sade.valintalaskenta.domain.dto.JonosijaDTO;
import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import fi.vm.sade.valintalaskenta.tulos.service.impl.converters.ValintatulosConverter;
import fi.vm.sade.valintalaskenta.tulos.service.impl.converters.ValintatulosConverterImpl;
import java.util.List;
import org.junit.jupiter.api.Test;

public class JonosijaComparatorTest {

  private ValintatulosConverter valintatulosConverter = new ValintatulosConverterImpl();

  @Test
  public void testComparator() {

    java.util.List<Jonosija> list = new java.util.ArrayList<Jonosija>();

    Jonosija sija0 = new Jonosija();
    sija0.setHakemusOid("sija0");
    Jonosija sija1 = new Jonosija();
    sija1.setHakemusOid("sija1");
    Jonosija sija2 = new Jonosija();
    sija2.setHakemusOid("sija2");
    Jonosija sija3 = new Jonosija();
    sija3.setHakemusOid("sija3");
    Jonosija sija4 = new Jonosija();
    sija4.setHakemusOid("sija4");
    Jonosija sija5 = new Jonosija();
    sija5.setHakemusOid("sija5");

    list.add(sija1);
    list.add(sija0);
    list.add(sija3);
    list.add(sija5);
    list.add(sija2);
    list.add(sija4);

    Jarjestyskriteeritulos s0t1 = new Jarjestyskriteeritulos();
    Jarjestyskriteeritulos s0t2 = new Jarjestyskriteeritulos();
    s0t1.setArvo(new java.math.BigDecimal("2"));
    s0t1.setPrioriteetti(1);
    s0t2.setArvo(new java.math.BigDecimal("2"));
    s0t2.setPrioriteetti(2);
    sija0.getJarjestyskriteeritulokset().jarjestyskriteeritulokset.add(s0t1);
    sija0.getJarjestyskriteeritulokset().jarjestyskriteeritulokset.add(s0t2);
    s0t1.setTila(JarjestyskriteerituloksenTila.HYVAKSYTTY_HARKINNANVARAISESTI);
    sija0.setHarkinnanvarainen(true);

    Jarjestyskriteeritulos s1t1 = new Jarjestyskriteeritulos();
    Jarjestyskriteeritulos s1t2 = new Jarjestyskriteeritulos();
    s1t1.setArvo(new java.math.BigDecimal("2"));
    s1t1.setPrioriteetti(1);
    s1t2.setArvo(new java.math.BigDecimal("2"));
    s1t2.setPrioriteetti(2);
    sija1.getJarjestyskriteeritulokset().jarjestyskriteeritulokset.add(s1t1);
    sija1.getJarjestyskriteeritulokset().jarjestyskriteeritulokset.add(s1t1);

    Jarjestyskriteeritulos s2t1 = new Jarjestyskriteeritulos();
    Jarjestyskriteeritulos s2t2 = new Jarjestyskriteeritulos();
    s2t1.setArvo(new java.math.BigDecimal("2"));
    s2t1.setPrioriteetti(1);
    s2t2.setArvo(new java.math.BigDecimal("2"));
    s2t2.setPrioriteetti(2);
    sija2.getJarjestyskriteeritulokset().jarjestyskriteeritulokset.add(s2t1);
    sija2.getJarjestyskriteeritulokset().jarjestyskriteeritulokset.add(s2t2);

    Jarjestyskriteeritulos s3t1 = new Jarjestyskriteeritulos();
    Jarjestyskriteeritulos s3t2 = new Jarjestyskriteeritulos();
    s3t1.setArvo(new java.math.BigDecimal("4"));
    s3t1.setPrioriteetti(1);
    s3t2.setArvo(new java.math.BigDecimal("1"));
    s3t2.setPrioriteetti(2);
    sija3.getJarjestyskriteeritulokset().jarjestyskriteeritulokset.add(s3t1);
    sija3.getJarjestyskriteeritulokset().jarjestyskriteeritulokset.add(s3t2);

    Jarjestyskriteeritulos s4t1 = new Jarjestyskriteeritulos();
    Jarjestyskriteeritulos s4t2 = new Jarjestyskriteeritulos();
    s4t1.setArvo(new java.math.BigDecimal("3"));
    s4t1.setPrioriteetti(1);
    s4t2.setArvo(new java.math.BigDecimal("1"));
    s4t2.setPrioriteetti(2);
    sija4.getJarjestyskriteeritulokset().jarjestyskriteeritulokset.add(s4t1);
    sija4.getJarjestyskriteeritulokset().jarjestyskriteeritulokset.add(s4t2);

    Jarjestyskriteeritulos s5t1 = new Jarjestyskriteeritulos();
    Jarjestyskriteeritulos s5t2 = new Jarjestyskriteeritulos();
    s5t1.setArvo(new java.math.BigDecimal("3000"));
    s5t1.setPrioriteetti(1);
    s5t2.setArvo(new java.math.BigDecimal("1000"));
    s5t2.setPrioriteetti(2);
    s5t1.setTila(JarjestyskriteerituloksenTila.HYLATTY);
    sija5.getJarjestyskriteeritulokset().jarjestyskriteeritulokset.add(s5t1);
    sija5.getJarjestyskriteeritulokset().jarjestyskriteeritulokset.add(s5t2);

    for (Jonosija j : list) {
      System.out.println("Before sort: " + j.getHakemusOid());
    }

    List<JonosijaDTO> a = valintatulosConverter.convertJonosija(list);
    valintatulosConverter.sort(a);

    for (JonosijaDTO j : a) {
      System.out.println("After sort: " + j.getHakemusOid());
    }

    assertEquals(a.get(0).getHakemusOid(), sija0.getHakemusOid());
    assertEquals(a.get(1).getHakemusOid(), sija3.getHakemusOid());
    assertEquals(a.get(2).getHakemusOid(), sija4.getHakemusOid());
    assertEquals(a.get(5).getHakemusOid(), sija5.getHakemusOid());
  }
}
