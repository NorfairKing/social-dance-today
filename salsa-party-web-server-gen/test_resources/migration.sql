CREATE TABLE "user"("id" INTEGER PRIMARY KEY,"email_address" VARCHAR NOT NULL,"passphrase_hash" VARCHAR NOT NULL,"verification_key" VARCHAR NULL,"created" TIMESTAMP NOT NULL,CONSTRAINT "unique_user_email_address" UNIQUE ("email_address"));
CREATE TABLE "organiser"("id" INTEGER PRIMARY KEY,"uuid" BLOB NOT NULL,"user" INTEGER NOT NULL REFERENCES "user","name" VARCHAR NOT NULL,"homepage" VARCHAR NULL DEFAULT NULL,"created" TIMESTAMP NOT NULL,"modified" TIMESTAMP NULL DEFAULT NULL,CONSTRAINT "unique_organiser_u_u_i_d" UNIQUE ("uuid"),CONSTRAINT "unique_organiser_user" UNIQUE ("user"));
CREATE TABLE "organiser_reminder"("id" INTEGER PRIMARY KEY,"organiser" INTEGER NOT NULL REFERENCES "organiser","consent" BOOLEAN NOT NULL DEFAULT false,"secret" BLOB NOT NULL DEFAULT NULL,"last" TIMESTAMP NULL,CONSTRAINT "unique_organiser_reminder_organiser" UNIQUE ("organiser"),CONSTRAINT "unique_organiser_reminder_secret" UNIQUE ("secret"));
CREATE TABLE "place"("id" INTEGER PRIMARY KEY,"query" VARCHAR NOT NULL,"lat" INTEGER NOT NULL,"lon" INTEGER NOT NULL,CONSTRAINT "unique_place_query" UNIQUE ("query"));
CREATE TABLE "party"("id" INTEGER PRIMARY KEY,"uuid" BLOB NOT NULL,"slug" VARCHAR NULL DEFAULT null,"organiser" INTEGER NOT NULL REFERENCES "organiser","title" VARCHAR NOT NULL,"description" VARCHAR NULL,"day" DATE NOT NULL,"start" TIME NULL,"homepage" VARCHAR NULL,"price" VARCHAR NULL DEFAULT NULL,"cancelled" BOOLEAN NOT NULL DEFAULT 0,"created" TIMESTAMP NOT NULL,"modified" TIMESTAMP NULL DEFAULT NULL,"place" INTEGER NOT NULL REFERENCES "place",CONSTRAINT "unique_party_u_u_i_d" UNIQUE ("uuid"));
CREATE TABLE "party_poster"("id" INTEGER PRIMARY KEY,"party" INTEGER NOT NULL REFERENCES "party","image" INTEGER NOT NULL REFERENCES "image","created" TIMESTAMP NOT NULL,"modified" TIMESTAMP NULL DEFAULT NULL,CONSTRAINT "unique_party_poster" UNIQUE ("party"));
CREATE TABLE "image"("id" INTEGER PRIMARY KEY,"key" INTEGER NOT NULL,"type" VARCHAR NOT NULL,"blob" BLOB NOT NULL,"created" TIMESTAMP NOT NULL,CONSTRAINT "unique_image_key" UNIQUE ("key"));
CREATE TABLE "schedule"("id" INTEGER PRIMARY KEY,"uuid" BLOB NOT NULL,"organiser" INTEGER NOT NULL REFERENCES "organiser","recurrence" BLOB NOT NULL,"title" VARCHAR NOT NULL,"description" VARCHAR NULL,"start" TIME NULL,"homepage" VARCHAR NULL,"price" VARCHAR NULL DEFAULT NULL,"created" TIMESTAMP NOT NULL,"modified" TIMESTAMP NULL DEFAULT NULL,"place" INTEGER NOT NULL REFERENCES "place",CONSTRAINT "unique_schedule_u_u_i_d" UNIQUE ("uuid"));
CREATE TABLE "schedule_poster"("id" INTEGER PRIMARY KEY,"schedule" INTEGER NOT NULL REFERENCES "schedule","image" INTEGER NOT NULL REFERENCES "image","created" TIMESTAMP NOT NULL,"modified" TIMESTAMP NULL DEFAULT NULL,CONSTRAINT "unique_schedule_poster" UNIQUE ("schedule"));
CREATE TABLE "schedule_party"("id" INTEGER PRIMARY KEY,"schedule" INTEGER NOT NULL REFERENCES "schedule","party" INTEGER NOT NULL REFERENCES "party","scheduled" TIMESTAMP NOT NULL,CONSTRAINT "unique_schedule_party" UNIQUE ("schedule","party"));
CREATE TABLE "importer_metadata"("id" INTEGER PRIMARY KEY,"name" VARCHAR NOT NULL,"last_run" TIMESTAMP NULL,"last_run_end" TIMESTAMP NULL,"last_run_imported" INTEGER NULL,CONSTRAINT "unique_importer_metadata_name" UNIQUE ("name"));
CREATE TABLE "external_event"("id" INTEGER PRIMARY KEY,"uuid" BLOB NOT NULL,"slug" VARCHAR NULL DEFAULT null,"key" VARCHAR NOT NULL,"title" VARCHAR NOT NULL,"description" VARCHAR NULL,"organiser" VARCHAR NULL,"day" DATE NOT NULL,"start" TIME NULL,"homepage" VARCHAR NULL,"price" VARCHAR NULL DEFAULT NULL,"cancelled" BOOLEAN NOT NULL DEFAULT 0,"created" TIMESTAMP NOT NULL,"modified" TIMESTAMP NULL DEFAULT NULL,"place" INTEGER NOT NULL REFERENCES "place","importer" INTEGER NOT NULL REFERENCES "importer_metadata","origin" VARCHAR NOT NULL,CONSTRAINT "unique_external_event_u_u_i_d" UNIQUE ("uuid"),CONSTRAINT "unique_external_event_key" UNIQUE ("importer","key"));
CREATE TABLE "external_event_poster"("id" INTEGER PRIMARY KEY,"external_event" INTEGER NOT NULL REFERENCES "external_event","image" INTEGER NOT NULL REFERENCES "image","created" TIMESTAMP NOT NULL,"modified" TIMESTAMP NULL DEFAULT NULL,CONSTRAINT "unique_external_event_poster" UNIQUE ("external_event"));
CREATE TABLE "static_map"("id" INTEGER PRIMARY KEY,"place" INTEGER NOT NULL REFERENCES "place","image" INTEGER NOT NULL REFERENCES "image",CONSTRAINT "unique_static_map_place" UNIQUE ("place"));
