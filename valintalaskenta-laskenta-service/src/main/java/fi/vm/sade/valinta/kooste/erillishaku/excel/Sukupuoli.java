package fi.vm.sade.valinta.kooste.erillishaku.excel;

public enum Sukupuoli {
  MIES,
  NAINEN,
  EI_SUKUPUOLTA;

  public static Sukupuoli fromString(String s) {
    try {
      if ("1".equals(s)) {
        return MIES;
      } else if ("2".equals(s)) {
        return NAINEN;
      } else {
        return Sukupuoli.valueOf(s);
      }
    } catch (IllegalArgumentException e) {
      return EI_SUKUPUOLTA;
    }
  }

  public static String toHenkiloString(Sukupuoli sukupuoli) {
    switch (sukupuoli) {
      case MIES:
        return "1";
      case NAINEN:
        return "2";
      default:
        return "";
    }
  }

  public static String toHenkiloString(String sukupuoli) {
    if (MIES.name().equalsIgnoreCase(sukupuoli)) {
      return "1";
    } else if (NAINEN.name().equalsIgnoreCase(sukupuoli)) {
      return "2";
    }
    return "";
  }

  public static Sukupuoli toSukupuoliEnum(String sukupuoli) {
    if (MIES.name().equalsIgnoreCase(sukupuoli)) {
      return MIES;
    } else if ("1".equalsIgnoreCase(sukupuoli)) {
      return MIES;
    } else if (NAINEN.name().equalsIgnoreCase(sukupuoli)) {
      return NAINEN;
    } else if ("2".equalsIgnoreCase(sukupuoli)) {
      return NAINEN;
    }
    return EI_SUKUPUOLTA;
  }

  public static String toSukupuoliString(String sukupuoli) {
    if (MIES.name().equalsIgnoreCase(sukupuoli)) {
      return MIES.name();
    } else if ("1".equalsIgnoreCase(sukupuoli)) {
      return MIES.name();
    } else if (NAINEN.name().equalsIgnoreCase(sukupuoli)) {
      return NAINEN.name();
    } else if ("2".equalsIgnoreCase(sukupuoli)) {
      return NAINEN.name();
    }
    return EI_SUKUPUOLTA.name();
  }
}
