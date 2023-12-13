CREATE TABLE jarjestyskriteerihistoria (
    id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
    created_at timestamp with time zone default now(),
    historia text NOT NULL,
    laskettu_uudelleen boolean NOT NULL DEFAULT false
);
