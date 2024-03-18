CREATE TABLE IF NOT EXISTS jarjestyskriteerihistoria (
    id BIGSERIAL PRIMARY KEY,
    tunniste uuid NOT NULL DEFAULT uuid_generate_v4(),
    created_at timestamp with time zone default now(),
    historia text,
    laskettu_uudelleen boolean NOT NULL DEFAULT false
);
