"""
gpkg_writer.py

Minimal, dependency-free GeoPackage (.gpkg) writer. GeoPackage is a
SQLite database with a defined schema (OGC GeoPackage spec), so this
uses Python's built-in sqlite3 module plus hand-rolled WKB geometry
encoding -- no GDAL/fiona/geopandas required.

Supports Point and LineString feature layers, one row at a time or in
batches, with an arbitrary flat attribute schema per layer.
"""

import sqlite3
import struct
import os


def _wkb_point(x, y):
    return struct.pack("<BIdd", 1, 1, x, y)


def _wkb_linestring(points):
    body = struct.pack("<BII", 1, 2, len(points))
    for x, y in points:
        body += struct.pack("<dd", x, y)
    return body


def _gpkg_geom_blob(wkb_bytes, srs_id):
    # GeoPackage binary header: magic "GP", version, flags, srs_id (no envelope)
    header = b"GP" + bytes([0]) + bytes([0x01]) + struct.pack("<i", srs_id)
    return header + wkb_bytes


def point_blob(x, y, srs_id):
    return _gpkg_geom_blob(_wkb_point(x, y), srs_id)


def linestring_blob(points, srs_id):
    return _gpkg_geom_blob(_wkb_linestring(points), srs_id)


class GeoPackageWriter:
    def __init__(self, path, overwrite=True):
        if overwrite and os.path.exists(path):
            os.remove(path)
        self.conn = sqlite3.connect(path)
        self._init_schema()

    def _init_schema(self):
        c = self.conn.cursor()
        # GeoPackage requires application_id = 'GPKG' (0x47504B47) and a
        # user_version encoding the spec version (10300 = v1.3.0) so that
        # QGIS/ArcGIS recognize this as a GeoPackage rather than plain SQLite.
        c.execute("PRAGMA application_id = 0x47504B47")
        c.execute("PRAGMA user_version = 10300")
        c.executescript("""
        PRAGMA foreign_keys = ON;

        CREATE TABLE gpkg_spatial_ref_sys (
            srs_name TEXT NOT NULL,
            srs_id INTEGER NOT NULL PRIMARY KEY,
            organization TEXT NOT NULL,
            organization_coordsys_id INTEGER NOT NULL,
            definition TEXT NOT NULL,
            description TEXT
        );

        CREATE TABLE gpkg_contents (
            table_name TEXT NOT NULL PRIMARY KEY,
            data_type TEXT NOT NULL,
            identifier TEXT UNIQUE,
            description TEXT DEFAULT '',
            last_change DATETIME NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now')),
            min_x DOUBLE, min_y DOUBLE, max_x DOUBLE, max_y DOUBLE,
            srs_id INTEGER,
            CONSTRAINT fk_gc_r_srs_id FOREIGN KEY (srs_id) REFERENCES gpkg_spatial_ref_sys(srs_id)
        );

        CREATE TABLE gpkg_geometry_columns (
            table_name TEXT NOT NULL,
            column_name TEXT NOT NULL,
            geometry_type_name TEXT NOT NULL,
            srs_id INTEGER NOT NULL,
            z TINYINT NOT NULL,
            m TINYINT NOT NULL,
            CONSTRAINT pk_geom_cols PRIMARY KEY (table_name, column_name),
            CONSTRAINT fk_gc_tn FOREIGN KEY (table_name) REFERENCES gpkg_contents(table_name),
            CONSTRAINT fk_gc_srs FOREIGN KEY (srs_id) REFERENCES gpkg_spatial_ref_sys(srs_id)
        );
        """)

        c.execute(
            "INSERT INTO gpkg_spatial_ref_sys VALUES "
            "('Undefined cartesian SRS', -1, 'NONE', -1, 'undefined', 'undefined cartesian coordinate reference system')"
        )
        c.execute(
            "INSERT INTO gpkg_spatial_ref_sys VALUES "
            "('Undefined geographic SRS', 0, 'NONE', 0, 'undefined', 'undefined geographic coordinate reference system')"
        )
        c.execute(
            "INSERT INTO gpkg_spatial_ref_sys VALUES "
            "('WGS 84 geodetic', 4326, 'EPSG', 4326, "
            "'GEOGCS[\"WGS 84\",DATUM[\"WGS_1984\",SPHEROID[\"WGS 84\",6378137,298.257223563]],"
            "PRIMEM[\"Greenwich\",0],UNIT[\"degree\",0.0174532925199433]]', 'WGS 84')"
        )
        self.conn.commit()

    def _sql_type(self, ftype):
        return {"TEXT": "TEXT", "INTEGER": "INTEGER", "REAL": "REAL"}[ftype]

    def create_layer(self, table_name, geom_type, srs_id, fields):
        """fields: list of (name, sql_type) where sql_type in TEXT/INTEGER/REAL."""
        c = self.conn.cursor()
        col_defs = ", ".join(f'"{name}" {self._sql_type(t)}' for name, t in fields)
        c.execute(
            f'CREATE TABLE "{table_name}" '
            f'(fid INTEGER PRIMARY KEY AUTOINCREMENT, geom BLOB, {col_defs})'
        )
        c.execute(
            "INSERT INTO gpkg_contents "
            "(table_name, data_type, identifier, description, srs_id) VALUES (?,?,?,?,?)",
            (table_name, "features", table_name, "", srs_id),
        )
        c.execute(
            "INSERT INTO gpkg_geometry_columns VALUES (?,?,?,?,0,0)",
            (table_name, "geom", geom_type, srs_id),
        )
        self.conn.commit()
        self._field_names = {table_name: [f[0] for f in fields]}.get(table_name, [])
        self.conn.commit()
        return [f[0] for f in fields]

    def insert_rows(self, table_name, field_names, rows):
        """rows: iterable of (geom_blob, val1, val2, ...) tuples, val order
        matching field_names."""
        placeholders = ", ".join(["?"] * (1 + len(field_names)))
        cols = ", ".join(['geom'] + [f'"{n}"' for n in field_names])
        sql = f'INSERT INTO "{table_name}" ({cols}) VALUES ({placeholders})'
        c = self.conn.cursor()
        c.executemany(sql, rows)

    def update_extent(self, table_name, minx, miny, maxx, maxy):
        c = self.conn.cursor()
        c.execute(
            "UPDATE gpkg_contents SET min_x=?, min_y=?, max_x=?, max_y=? WHERE table_name=?",
            (minx, miny, maxx, maxy, table_name),
        )

    def commit(self):
        self.conn.commit()

    def close(self):
        self.conn.commit()
        self.conn.close()
