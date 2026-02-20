--
-- PostgreSQL database dump
--

\restrict N9g2WgXcNq6pcV585RDbn0uvdfkqZHjWU2xVb6eT0bZjFni8aZ8iZzorITZgtwz

-- Dumped from database version 16.12
-- Dumped by pg_dump version 16.12

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: nix_packages; Type: TABLE; Schema: public; Owner: nixdude
--

CREATE TABLE public.nix_packages (
    id integer NOT NULL,
    attribute_name text,
    package_name text NOT NULL,
    version text,
    description text,
    ingestion_date timestamp without time zone DEFAULT CURRENT_TIMESTAMP,
    depth integer
);


ALTER TABLE public.nix_packages OWNER TO nixdude;

--
-- Name: nix_packages_id_seq; Type: SEQUENCE; Schema: public; Owner: nixdude
--

CREATE SEQUENCE public.nix_packages_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE public.nix_packages_id_seq OWNER TO nixdude;

--
-- Name: nix_packages_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: nixdude
--

ALTER SEQUENCE public.nix_packages_id_seq OWNED BY public.nix_packages.id;


--
-- Name: package_dependencies; Type: TABLE; Schema: public; Owner: nixdude
--

CREATE TABLE public.package_dependencies (
    parent_id integer NOT NULL,
    child_id integer NOT NULL
);


ALTER TABLE public.package_dependencies OWNER TO nixdude;

--
-- Name: nix_packages id; Type: DEFAULT; Schema: public; Owner: nixdude
--

ALTER TABLE ONLY public.nix_packages ALTER COLUMN id SET DEFAULT nextval('public.nix_packages_id_seq'::regclass);


--
-- Name: nix_packages nix_packages_package_name_key; Type: CONSTRAINT; Schema: public; Owner: nixdude
--

ALTER TABLE ONLY public.nix_packages
    ADD CONSTRAINT nix_packages_package_name_key UNIQUE (package_name);


--
-- Name: nix_packages nix_packages_pkey; Type: CONSTRAINT; Schema: public; Owner: nixdude
--

ALTER TABLE ONLY public.nix_packages
    ADD CONSTRAINT nix_packages_pkey PRIMARY KEY (id);


--
-- Name: package_dependencies package_dependencies_pkey; Type: CONSTRAINT; Schema: public; Owner: nixdude
--

ALTER TABLE ONLY public.package_dependencies
    ADD CONSTRAINT package_dependencies_pkey PRIMARY KEY (parent_id, child_id);


--
-- Name: idx_package_name; Type: INDEX; Schema: public; Owner: nixdude
--

CREATE INDEX idx_package_name ON public.nix_packages USING btree (package_name);


--
-- Name: package_dependencies package_dependencies_child_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: nixdude
--

ALTER TABLE ONLY public.package_dependencies
    ADD CONSTRAINT package_dependencies_child_id_fkey FOREIGN KEY (child_id) REFERENCES public.nix_packages(id);


--
-- Name: package_dependencies package_dependencies_parent_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: nixdude
--

ALTER TABLE ONLY public.package_dependencies
    ADD CONSTRAINT package_dependencies_parent_id_fkey FOREIGN KEY (parent_id) REFERENCES public.nix_packages(id);


--
-- PostgreSQL database dump complete
--

\unrestrict N9g2WgXcNq6pcV585RDbn0uvdfkqZHjWU2xVb6eT0bZjFni8aZ8iZzorITZgtwz

