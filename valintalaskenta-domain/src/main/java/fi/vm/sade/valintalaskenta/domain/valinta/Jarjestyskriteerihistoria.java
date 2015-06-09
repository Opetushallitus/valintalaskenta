package fi.vm.sade.valintalaskenta.domain.valinta;

import org.mongodb.morphia.annotations.Entity;
import org.mongodb.morphia.annotations.Id;
import org.bson.types.ObjectId;

@Entity("Jarjestyskriteerihistoria")
public class Jarjestyskriteerihistoria {
    @Id
    private ObjectId id;

    private String historia;

    public ObjectId getId() {
        return id;
    }

    public void setId(ObjectId id) {
        this.id = id;
    }

    public String getHistoria() {
        return historia;
    }

    public void setHistoria(String historia) {
        this.historia = historia;
    }
}
