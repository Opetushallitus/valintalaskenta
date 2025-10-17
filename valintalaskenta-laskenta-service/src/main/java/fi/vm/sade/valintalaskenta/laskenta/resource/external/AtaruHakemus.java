package fi.vm.sade.valintalaskenta.laskenta.resource.external;

import com.google.gson.annotations.SerializedName;

public record AtaruHakemus(
    @SerializedName("hakemus_oid") String hakemusOid,
    @SerializedName("henkilo_oid") String henkiloOid)
    implements ExternalHakemus {}
