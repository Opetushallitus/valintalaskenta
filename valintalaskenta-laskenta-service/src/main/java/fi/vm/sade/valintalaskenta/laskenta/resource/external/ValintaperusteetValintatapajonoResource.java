package fi.vm.sade.valintalaskenta.laskenta.resource.external;

import java.util.List;
import java.util.Map;

public interface ValintaperusteetValintatapajonoResource {
  Map<String, List<String>> findKopiot(final List<String> oid);
}
