ALTER TABLE harkinnanvarainen_hyvaksyminen ADD COLUMN transaction_id bigint not null default txid_current();

CREATE TABLE harkinnanvarainen_hyvaksyminen_history (like harkinnanvarainen_hyvaksyminen);

CREATE OR REPLACE FUNCTION update_harkinnanvarainen_hyvaksyminen_history() returns trigger as
$$
begin
    insert into harkinnanvarainen_hyvaksyminen_history (
        id,
        created_at,
        harkinnanvaraisuus_tila,
        hakukohde_oid,
        hakemus_oid,
        haku_oid,
        transaction_id
    ) values (
        old.id,
        old.created_at,
        old.harkinnanvaraisuus_tila,
        old.hakukohde_oid,
        old.hakemus_oid,
        old.haku_oid,
        old.transaction_id
    );
    return null;
end;
$$ language plpgsql;

create trigger harkinnanvarainen_hyvaksyminen_history
    after update on harkinnanvarainen_hyvaksyminen
    for each row
    when (old.transaction_id <> txid_current())
execute procedure update_harkinnanvarainen_hyvaksyminen_history();

create trigger delete_harkinnanvarainen_hyvaksyminen_history
    after delete on harkinnanvarainen_hyvaksyminen
    for each row
execute procedure update_harkinnanvarainen_hyvaksyminen_history();


ALTER TABLE hakijaryhma ADD COLUMN transaction_id bigint not null default txid_current();

CREATE TABLE hakijaryhma_history (like hakijaryhma);

CREATE OR REPLACE FUNCTION update_hakijaryhma_history() returns trigger as
$$
begin
    insert into hakijaryhma_history (
        id,
        hakijaryhma_oid,
        prioriteetti,
        created_at,
        hakukohde_oid,
        nimi,
        kuvaus,
        kiintio,
        kayta_kaikki,
        tarkka_kiintio,
        kaytetaan_ryhmaan_kuuluvia,
        hakijaryhmatyyppi_koodiuri,
        valintatapajono_oid,
        transaction_id
    ) values (
         old.id,
         old.hakijaryhma_oid,
         old.prioriteetti,
         old.created_at,
         old.hakukohde_oid,
         old.nimi,
         old.kuvaus,
         old.kiintio,
         old.kayta_kaikki,
         old.tarkka_kiintio,
         old.kaytetaan_ryhmaan_kuuluvia,
         old.hakijaryhmatyyppi_koodiuri,
         old.valintatapajono_oid,
         old.transaction_id
     );
    return null;
end;
$$ language plpgsql;

create trigger hakijaryhma_history
    after update on hakijaryhma
    for each row
    when (old.transaction_id <> txid_current())
execute procedure update_hakijaryhma_history();

create trigger delete_hakijaryhma_history
    after delete on hakijaryhma
    for each row
execute procedure update_hakijaryhma_history();


ALTER TABLE valinnanvaihe ADD COLUMN transaction_id bigint not null default txid_current();

CREATE TABLE valinnanvaihe_history (like valinnanvaihe);

CREATE OR REPLACE FUNCTION update_valinnanvaihe_history() returns trigger as
$$
begin
    insert into valinnanvaihe_history (
        id,
        jarjestysnumero,
        created_at,
        haku_oid,
        hakukohde_oid,
        valinnanvaihe_oid,
        tarjoaja_oid,
        nimi,
        transaction_id
    ) values (
             old.id,
             old.jarjestysnumero,
             old.created_at,
             old.haku_oid,
             old.hakukohde_oid,
             old.valinnanvaihe_oid,
             old.tarjoaja_oid,
             old.nimi,
             old.transaction_id
     );
    return null;
end;
$$ language plpgsql;

create trigger valinnanvaihe_history
    after update on valinnanvaihe
    for each row
    when (old.transaction_id <> txid_current())
execute procedure update_valinnanvaihe_history();

create trigger delete_valinnanvaihe_history
    after delete on valinnanvaihe
    for each row
execute procedure update_valinnanvaihe_history();


ALTER TABLE valintakoe_osallistuminen ADD COLUMN transaction_id bigint not null default txid_current();

CREATE TABLE valintakoe_osallistuminen_history (like valintakoe_osallistuminen);

CREATE OR REPLACE FUNCTION update_valintakoe_osallistuminen_history() returns trigger as
$$
begin
    insert into valintakoe_osallistuminen_history (
        id,
        haku_oid,
        hakemus_oid,
        hakija_oid,
        created_at,
        transaction_id
    ) values (
             old.id,
             old.haku_oid,
             old.hakemus_oid,
             old.hakija_oid,
             old.created_at,
             old.transaction_id
             );
    return null;
end;
$$ language plpgsql;

create trigger valintakoe_osallistuminen_history
    after update on valintakoe_osallistuminen
    for each row
    when (old.transaction_id <> txid_current())
execute procedure update_valintakoe_osallistuminen_history();

create trigger delete_valintakoe_osallistuminen_history
    after delete on valintakoe_osallistuminen
    for each row
execute procedure update_valintakoe_osallistuminen_history();


ALTER TABLE hakutoive ADD COLUMN transaction_id bigint not null default txid_current();

CREATE TABLE hakutoive_history (like hakutoive);

CREATE OR REPLACE FUNCTION update_hakutoive_history() returns trigger as
$$
begin
    insert into hakutoive_history (
        id,
        hakukohde_oid,
        valintakoe_osallistuminen,
        created_at,
        transaction_id
    ) values (
             old.id,
             old.hakukohde_oid,
             old.valintakoe_osallistuminen,
             old.created_at,
             old.transaction_id
             );
    return null;
end;
$$ language plpgsql;

create trigger hakutoive_history
    after update on hakutoive
    for each row
    when (old.transaction_id <> txid_current())
execute procedure update_hakutoive_history();

create trigger delete_hakutoive_history
    after delete on hakutoive
    for each row
execute procedure update_hakutoive_history();



ALTER TABLE valintakoe_valinnanvaihe ADD COLUMN transaction_id bigint not null default txid_current();

CREATE TABLE valintakoe_valinnanvaihe_history (like valintakoe_valinnanvaihe);

CREATE OR REPLACE FUNCTION update_valintakoe_valinnanvaihe_history() returns trigger as
$$
begin
    insert into valintakoe_valinnanvaihe_history (
        id,
        valinnanvaihe_oid,
        valinnan_vaihe_jarjestysluku,
        hakutoive,
        created_at,
        transaction_id
    ) values (
             old.id,
             old.valinnanvaihe_oid,
             old.valinnan_vaihe_jarjestysluku,
             old.hakutoive,
             old.created_at,
             old.transaction_id
             );
    return null;
end;
$$ language plpgsql;

create trigger valintakoe_valinnanvaihe_history
    after update on valintakoe_valinnanvaihe
    for each row
    when (old.transaction_id <> txid_current())
execute procedure update_valintakoe_valinnanvaihe_history();

create trigger delete_valintakoe_valinnanvaihe_history
    after delete on valintakoe_valinnanvaihe
    for each row
execute procedure update_valintakoe_valinnanvaihe_history();


ALTER TABLE valintakoe ADD COLUMN transaction_id bigint not null default txid_current();

CREATE TABLE valintakoe_history (like valintakoe);

CREATE OR REPLACE FUNCTION update_valintakoe_history() returns trigger as
$$
begin
    insert into valintakoe_history (
        id,
        valintakoe_oid,
        valintakoe_tunniste,
        nimi,
        aktiivinen,
        valintakoe_valinnanvaihe,
        lahetetaanko_koekutsut,
        osallistuminen,
        kutsuttavien_maara,
        kutsun_kohde,
        kutsun_kohde_avain,
        kuvaus_fi,
        kuvaus_sv,
        kuvaus_en,
        laskenta_tila,
        laskenta_tulos,
        tekninen_kuvaus,
        created_at,
        transaction_id
    ) values (
             old.id,
             old.valintakoe_oid,
             old.valintakoe_tunniste,
             old.nimi,
             old.aktiivinen,
             old.valintakoe_valinnanvaihe,
             old.lahetetaanko_koekutsut,
             old.osallistuminen,
             old.kutsuttavien_maara,
             old.kutsun_kohde,
             old.kutsun_kohde_avain,
             old.kuvaus_fi,
             old.kuvaus_sv,
             old.kuvaus_en,
             old.laskenta_tila,
             old.laskenta_tulos,
             old.tekninen_kuvaus,
             old.created_at,
             old.transaction_id
             );
    return null;
end;
$$ language plpgsql;

create trigger valintakoe_history
    after update on valintakoe
    for each row
    when (old.transaction_id <> txid_current())
execute procedure update_valintakoe_history();

create trigger delete_valintakoe_history
    after delete on valintakoe
    for each row
execute procedure update_valintakoe_history();


ALTER TABLE valintatapajono ADD COLUMN transaction_id bigint not null default txid_current();

CREATE TABLE valintatapajono_history (like valintatapajono);

CREATE OR REPLACE FUNCTION update_valintatapajono_history() returns trigger as
$$
begin
    insert into valintatapajono_history (
        id,
        created_at,
        valintatapajono_oid,
        nimi,
        prioriteetti,
        aloituspaikat,
        siirretaan_sijoitteluun,
        tasasijasaanto,
        ei_varasijatayttoa,
        kaikki_ehdon_tayttavat_hyvaksytaan,
        kaytetaan_valintalaskentaa,
        valmis_sijoiteltavaksi,
        kaytetaan_kokonaispisteita,
        valinnanvaihe,
        valinnanvaihe_key,
        sijoitteluajo_id,
        poissa_oleva_taytto,
        transaction_id
    ) values (
             old.id,
             old.created_at,
             old.valintatapajono_oid,
             old.nimi,
             old.prioriteetti,
             old.aloituspaikat,
             old.siirretaan_sijoitteluun,
             old.tasasijasaanto,
             old.ei_varasijatayttoa,
             old.kaikki_ehdon_tayttavat_hyvaksytaan,
             old.kaytetaan_valintalaskentaa,
             old.valmis_sijoiteltavaksi,
             old.kaytetaan_kokonaispisteita,
             old.valinnanvaihe,
             old.valinnanvaihe_key,
             old.sijoitteluajo_id,
             old.poissa_oleva_taytto,
             old.transaction_id
             );
    return null;
end;
$$ language plpgsql;

create trigger valintatapajono_history
    after update on valintatapajono
    for each row
    when (old.transaction_id <> txid_current())
execute procedure update_valintatapajono_history();

create trigger delete_valintatapajono_history
    after delete on valintatapajono
    for each row
execute procedure update_valintatapajono_history();


ALTER TABLE jonosija ADD COLUMN transaction_id bigint not null default txid_current();

CREATE TABLE jonosija_history (like jonosija);

CREATE OR REPLACE FUNCTION update_jonosija_history() returns trigger as
$$
begin
    insert into jonosija_history (
        id,
        created_at,
        hakemus_oid,
        hakija_oid,
        hakutoiveprioriteetti,
        harkinnanvarainen,
        hylatty_valisijoittelussa,
        valintatapajono,
        hakijaryhma,
        funktio_tulokset,
        syotetyt_arvot,
        transaction_id
    ) values (
             old.id,
             old.created_at,
             old.hakemus_oid,
             old.hakija_oid,
             old.hakutoiveprioriteetti,
             old.harkinnanvarainen,
             old.hylatty_valisijoittelussa,
             old.valintatapajono,
             old.hakijaryhma,
             old.funktio_tulokset,
             old.syotetyt_arvot,
             old.transaction_id
             );
    return null;
end;
$$ language plpgsql;

create trigger jonosija_history
    after update on jonosija
    for each row
    when (old.transaction_id <> txid_current())
execute procedure update_jonosija_history();

create trigger delete_jonosija_history
    after delete on jonosija
    for each row
execute procedure update_jonosija_history();



ALTER TABLE muokattu_jonosija ADD COLUMN transaction_id bigint not null default txid_current();

CREATE TABLE muokattu_jonosija_history (like muokattu_jonosija);

CREATE OR REPLACE FUNCTION update_muokattu_jonosija_history() returns trigger as
$$
begin
    insert into muokattu_jonosija_history (
        id,
        created_at,
        hakukohde_oid,
        haku_oid,
        valintatapajono_oid,
        hakemus_oid,
        harkinnanvarainen,
        prioriteetti,
        selite,
        muutos,
        transaction_id
    ) values (
             old.id,
             old.created_at,
             old.hakukohde_oid,
             old.haku_oid,
             old.valintatapajono_oid,
             old.hakemus_oid,
             old.harkinnanvarainen,
             old.prioriteetti,
             old.selite,
             old.muutos,
             old.transaction_id
             );
    return null;
end;
$$ language plpgsql;

create trigger muokattu_jonosija_history
    after update on muokattu_jonosija
    for each row
    when (old.transaction_id <> txid_current())
execute procedure update_muokattu_jonosija_history();

create trigger delete_muokattu_jonosija_history
    after delete on muokattu_jonosija
    for each row
execute procedure update_muokattu_jonosija_history();



ALTER TABLE jarjestyskriteeritulos ADD COLUMN transaction_id bigint not null default txid_current();

CREATE TABLE jarjestyskriteeritulos_history (like jarjestyskriteeritulos);

CREATE OR REPLACE FUNCTION update_jarjestyskriteeritulos_history() returns trigger as
$$
begin
    insert into jarjestyskriteeritulos_history (
        id,
        created_at,
        prioriteetti,
        arvo,
        tila,
        nimi,
        kuvaus_fi,
        kuvaus_sv,
        kuvaus_en,
        tekninen_kuvaus,
        jonosija,
        jonosija_key,
        muokattu_jonosija,
        transaction_id
    ) values (
                 old.id,
                 old.created_at,
                 old.prioriteetti,
                 old.arvo,
                 old.tila,
                 old.nimi,
                 old.kuvaus_fi,
                 old.kuvaus_sv,
                 old.kuvaus_en,
                 old.tekninen_kuvaus,
                 old.jonosija,
                 old.jonosija_key,
                 old.muokattu_jonosija,
                 old.transaction_id
             );
    return null;
end;
$$ language plpgsql;

create trigger jarjestyskriteeritulos_history
    after update on jarjestyskriteeritulos
    for each row
    when (old.transaction_id <> txid_current())
execute procedure update_jarjestyskriteeritulos_history();

create trigger delete_jarjestyskriteeritulos_history
    after delete on jarjestyskriteeritulos
    for each row
execute procedure update_jarjestyskriteeritulos_history();
