CREATE TABLE IF NOT EXISTS nix_packages (
    id SERIAL PRIMARY KEY,
    attribute_name TEXT NOT NULL,
    package_name TEXT NOT NULL,
    version TEXT,
    description TEXT,
    ingestion_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
CREATE INDEX IF NOT EXISTS idx_package_name ON nix_packages(package_name);
