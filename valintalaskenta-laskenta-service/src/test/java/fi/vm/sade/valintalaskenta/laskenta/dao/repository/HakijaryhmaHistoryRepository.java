package fi.vm.sade.valintalaskenta.laskenta.dao.repository;

import fi.vm.sade.valintalaskenta.laskenta.dao.HakijaryhmaHistory;
import java.util.UUID;
import org.springframework.data.repository.CrudRepository;

public interface HakijaryhmaHistoryRepository extends CrudRepository<HakijaryhmaHistory, UUID> {}
