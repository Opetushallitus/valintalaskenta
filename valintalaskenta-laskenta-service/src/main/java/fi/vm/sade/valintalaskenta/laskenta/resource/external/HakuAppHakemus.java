package fi.vm.sade.valintalaskenta.laskenta.resource.external;

import com.google.gson.annotations.SerializedName;

public record HakuAppHakemus(
    @SerializedName("oid") String hakemusOid, @SerializedName("personOid") String henkiloOid)
    implements ExternalHakemus {}
