package fi.vm.sade.valintalaskenta.laskenta.dao.repository;

import fi.vm.sade.valintalaskenta.laskenta.dao.HakijaryhmaHistory;
import org.springframework.data.repository.CrudRepository;

import java.util.UUID;

public interface HakijaryhmaHistoryRepository extends CrudRepository<HakijaryhmaHistory, UUID> {
}
