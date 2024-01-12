CREATE TABLE IF NOT EXISTS data_migration_control (
    id BIGSERIAL PRIMARY KEY,
    created_at timestamp with time zone default now(),
    updated_at timestamp with time zone default now(),
    hakukohde_oid varchar(127),
    haku_oid varchar(127) NOT NULL,
    success boolean DEFAULT NULL,
    error_message text
);
