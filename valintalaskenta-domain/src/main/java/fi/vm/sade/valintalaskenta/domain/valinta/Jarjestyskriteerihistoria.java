package fi.vm.sade.valintalaskenta.domain.valinta;

import com.google.code.morphia.annotations.Entity;
import com.google.code.morphia.annotations.Id;
import org.bson.types.ObjectId;

/**
 * User: tommiha
 * Date: 8/7/13
 * Time: 1:08 PM
 */
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
