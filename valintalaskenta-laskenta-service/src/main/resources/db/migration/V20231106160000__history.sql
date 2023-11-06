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
