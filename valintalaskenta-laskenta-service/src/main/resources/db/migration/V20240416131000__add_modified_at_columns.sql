alter table valinnanvaihe
    add column if not exists last_modified timestamptz;

alter table valintatapajono
    add column if not exists last_modified timestamptz;

alter table jonosija
    add column if not exists last_modified timestamptz;

alter table muokattu_jonosija
    add column if not exists last_modified timestamptz;

alter table harkinnanvarainen_hyvaksyminen
    add column if not exists last_modified timestamptz;

alter table valintakoe_valinnanvaihe
    add column if not exists last_modified timestamptz;

alter table valintakoe
    add column if not exists last_modified timestamptz;

CREATE INDEX IF NOT EXISTS valinnanvaihe_last_modified_idx ON valinnanvaihe (last_modified);
CREATE INDEX IF NOT EXISTS valintatapajono_last_modified_idx ON valintatapajono (last_modified);
CREATE INDEX IF NOT EXISTS jonosija_last_modified_idx ON jonosija (last_modified);
CREATE INDEX IF NOT EXISTS muokattu_jonosija_last_modified_idx ON muokattu_jonosija (last_modified);
CREATE INDEX IF NOT EXISTS Harkinnanvarainen_hyvaksyminen_last_modified_idx ON Harkinnanvarainen_hyvaksyminen (last_modified);
CREATE INDEX IF NOT EXISTS valintakoe_osallistuminen_created_at_idx ON valintakoe_osallistuminen (created_at);
CREATE INDEX IF NOT EXISTS hakutoive_created_at_idx ON hakutoive (created_at);
CREATE INDEX IF NOT EXISTS valintakoe_valinnanvaihe_last_modified_idx ON valintakoe_valinnanvaihe (last_modified);
CREATE INDEX IF NOT EXISTS valintakoe_last_modified_idx ON valintakoe (last_modified);

-- Luodaan funktio joka hoitaa aikaleiman päivityksen
create or replace function set_last_modified() returns trigger as
$$
begin
    new.last_modified := now()::timestamptz;
return new;
end;
$$ language plpgsql;

create or replace trigger set_valinnanvaihe_last_modified_on_change
    before insert or update
    on valinnanvaihe
    for each row
execute procedure set_last_modified();

create or replace trigger set_valintatapajono_last_modified_on_change
    before insert or update
    on valintatapajono
    for each row
execute procedure set_last_modified();

create or replace trigger set_jonosija_last_modified_on_change
    before insert or update
    on jonosija
    for each row
execute procedure set_last_modified();

create or replace trigger set_muokattu_jonosija_last_modified_on_change
    before insert or update
    on muokattu_jonosija
    for each row
execute procedure set_last_modified();

create or replace trigger set_harkinnanvarainen_hyvaksyminen_last_modified_on_change
    before insert or update
    on harkinnanvarainen_hyvaksyminen
    for each row
execute procedure set_last_modified();

create or replace trigger set_valintakoe_valinnanvaihe_last_modified_on_change
    before insert or update
    on valintakoe_valinnanvaihe
    for each row
execute procedure set_last_modified();

create or replace trigger set_valintakoe_last_modified_on_change
    before insert or update
    on valintakoe
    for each row
execute procedure set_last_modified();