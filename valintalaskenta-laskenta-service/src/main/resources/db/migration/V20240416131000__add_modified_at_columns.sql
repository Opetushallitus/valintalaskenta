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