"""
ricon_to_geopackage.py

Combines RICON node/edge CSVs (nid_df.csv, sites_df.csv, EdgeList.csv)
and a linked NHDPlusV2 flowline shapefile into ONE GeoPackage with three
feature layers:

  ricon_nodes     (Point)      -- dams + gages, from nid_df.csv/sites_df.csv
  ricon_edges     (LineString) -- straight-line node-to-node edges, from EdgeList.csv
  nhd_flowlines   (LineString) -- true river-channel geometry, from the
                                  linked NHDFlowline shapefile

Why one file: a GeoPackage can hold many layers, so this keeps the whole
RICON CRB network -- graph structure AND true channel geometry -- in a
single, GIS-standard, non-proprietary file, instead of scattered
shapefile sets. It also removes the .dbf 10-character field name
truncation problem that affected the shapefile export, since GeoPackage
attribute columns carry full names.

Dependencies: the Python standard library + pandas. Uses the project's
own pure-Python shp_reader.py and gpkg_writer.py, so no GDAL, fiona or
geopandas is needed.

Usage:
  python ricon_to_geopackage.py \
      --nid nid_df.csv --sites sites_df.csv --edges EdgeList.csv \
      --flowlines linked_nhdflowlines.shp \
      --out ricon_crb.gpkg
"""

import argparse
import sys
import pandas as pd

import os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from gpkg_writer import GeoPackageWriter, point_blob, linestring_blob
from shp_reader import read_dbf_fields, iter_dbf_records, iter_shp_geometries, shapefile_record_count

SRS_WGS84 = 4326


def dbf_type_to_sql(ftype, dec):
    if ftype == "N":
        return "REAL" if dec and dec > 0 else "INTEGER"
    if ftype == "F":
        return "REAL"
    return "TEXT"


def load_nodes(nid_path, sites_path):
    nid = pd.read_csv(nid_path, dtype={"IDS": str})
    sites = pd.read_csv(sites_path, dtype={"IDS": str})
    for df, default_type in [(nid, "dam"), (sites, "gage")]:
        if "POINTTYPE" not in df.columns:
            df["POINTTYPE"] = default_type
    nodes = pd.concat([nid, sites], ignore_index=True)
    before = len(nodes)
    nodes = nodes.dropna(subset=["LONGITUDE", "LATITUDE"]).copy()
    dropped = before - len(nodes)
    if dropped:
        print(f"Warning: dropped {dropped} node(s) with missing coordinates.")
    dupe_ids = nodes["IDS"][nodes["IDS"].duplicated()].unique()
    if len(dupe_ids):
        print(f"Warning: {len(dupe_ids)} duplicate IDS values in node tables.")
    return nodes


def build_edges(edge_path, nodes_df):
    edges = pd.read_csv(edge_path, dtype={"FROM_NODE": str, "TO_NODE": str})
    coord_lookup = nodes_df.drop_duplicates(subset="IDS").set_index("IDS")[["LONGITUDE", "LATITUDE"]]

    def make_line(row):
        try:
            fx, fy = coord_lookup.loc[row["FROM_NODE"]]
            tx, ty = coord_lookup.loc[row["TO_NODE"]]
        except KeyError:
            return None
        return [(float(fx), float(fy)), (float(tx), float(ty))]

    edges["_line_coords"] = edges.apply(make_line, axis=1)
    n_missing = edges["_line_coords"].isna().sum()
    if n_missing:
        print(f"Warning: {n_missing} of {len(edges)} edges could not be matched to node "
              "coordinates and were dropped.")
    return edges.dropna(subset=["_line_coords"]).copy()


def write_nodes_layer(gpkg, nodes_df):
    cols = [c for c in nodes_df.columns if c not in ("LONGITUDE", "LATITUDE")]
    fields = []
    for c in cols:
        if pd.api.types.is_float_dtype(nodes_df[c]):
            fields.append((c, "REAL"))
        elif pd.api.types.is_integer_dtype(nodes_df[c]):
            fields.append((c, "INTEGER"))
        else:
            fields.append((c, "TEXT"))

    gpkg.create_layer("ricon_nodes", "POINT", SRS_WGS84, fields)

    rows = []
    xs, ys = [], []
    for _, row in nodes_df.iterrows():
        x, y = float(row["LONGITUDE"]), float(row["LATITUDE"])
        xs.append(x)
        ys.append(y)
        blob = point_blob(x, y, SRS_WGS84)
        vals = tuple(row[c] if pd.notna(row[c]) else None for c in cols)
        rows.append((blob,) + vals)

    gpkg.insert_rows("ricon_nodes", cols, rows)
    gpkg.update_extent("ricon_nodes", min(xs), min(ys), max(xs), max(ys))
    gpkg.commit()
    print(f"ricon_nodes: wrote {len(rows)} points")


def write_edges_layer(gpkg, edges_df):
    cols = [c for c in edges_df.columns if c != "_line_coords"]
    fields = []
    for c in cols:
        if pd.api.types.is_float_dtype(edges_df[c]):
            fields.append((c, "REAL"))
        elif pd.api.types.is_integer_dtype(edges_df[c]):
            fields.append((c, "INTEGER"))
        else:
            fields.append((c, "TEXT"))

    gpkg.create_layer("ricon_edges", "LINESTRING", SRS_WGS84, fields)

    rows = []
    xs, ys = [], []
    for _, row in edges_df.iterrows():
        pts = row["_line_coords"]
        for x, y in pts:
            xs.append(x)
            ys.append(y)
        blob = linestring_blob(pts, SRS_WGS84)
        vals = tuple(row[c] if pd.notna(row[c]) else None for c in cols)
        rows.append((blob,) + vals)

    gpkg.insert_rows("ricon_edges", cols, rows)
    gpkg.update_extent("ricon_edges", min(xs), min(ys), max(xs), max(ys))
    gpkg.commit()
    print(f"ricon_edges: wrote {len(rows)} lines")


def write_flowlines_layer(gpkg, shp_path, dbf_path, shx_path, batch_size=5000):
    field_defs, header_size, record_size, n_records = read_dbf_fields(dbf_path)
    shx_count = shapefile_record_count(shx_path)
    if shx_count != n_records:
        print(f"Note: .shx record count ({shx_count}) differs from .dbf record count "
              f"({n_records}); using .dbf count.")

    fields = [(name, dbf_type_to_sql(ftype, dec)) for name, ftype, length, dec in field_defs]
    field_names = [f[0] for f in fields]
    gpkg.create_layer("nhd_flowlines", "LINESTRING", SRS_WGS84, fields)

    dbf_gen = iter_dbf_records(dbf_path, field_defs, header_size, record_size, n_records)
    shp_gen = iter_shp_geometries(shp_path)

    batch = []
    total = 0
    xmin = ymin = float("inf")
    xmax = ymax = float("-inf")
    skipped_multipart = 0

    for parts, attrs in zip(shp_gen, dbf_gen):
        if not parts or not parts[0]:
            continue
        if len(parts) == 1:
            pts = parts[0]
        else:
            # Multi-part polyline: keep the longest part as the representative
            # line for this record so the layer stays simple LineString type;
            # flag how often this happens.
            skipped_multipart += 1
            pts = max(parts, key=len)

        for x, y in pts:
            if x < xmin: xmin = x
            if x > xmax: xmax = x
            if y < ymin: ymin = y
            if y > ymax: ymax = y

        blob = linestring_blob(pts, SRS_WGS84)
        vals = tuple(attrs.get(name) for name in field_names)
        batch.append((blob,) + vals)
        total += 1

        if len(batch) >= batch_size:
            gpkg.insert_rows("nhd_flowlines", field_names, batch)
            gpkg.commit()
            batch = []
            if total % 50000 == 0:
                print(f"  ...{total} flowline records written")

    if batch:
        gpkg.insert_rows("nhd_flowlines", field_names, batch)
        gpkg.commit()

    gpkg.update_extent("nhd_flowlines", xmin, ymin, xmax, ymax)
    gpkg.commit()
    print(f"nhd_flowlines: wrote {total} lines"
          + (f" ({skipped_multipart} multi-part records reduced to longest part)"
             if skipped_multipart else ""))


def main():
    ap = argparse.ArgumentParser(description="Combine RICON nodes/edges + NHD flowlines into one GeoPackage")
    ap.add_argument("--nid", required=True)
    ap.add_argument("--sites", required=True)
    ap.add_argument("--edges", required=True)
    ap.add_argument("--flowlines", required=True, help="Path to linked_nhdflowlines.shp")
    ap.add_argument("--out", default="ricon_crb.gpkg")
    args = ap.parse_args()

    nodes_df = load_nodes(args.nid, args.sites)
    edges_df = build_edges(args.edges, nodes_df)

    gpkg = GeoPackageWriter(args.out, overwrite=True)
    write_nodes_layer(gpkg, nodes_df)
    write_edges_layer(gpkg, edges_df)

    flowlines_prefix = args.flowlines[:-4] if args.flowlines.endswith(".shp") else args.flowlines
    write_flowlines_layer(
        gpkg,
        shp_path=flowlines_prefix + ".shp",
        dbf_path=flowlines_prefix + ".dbf",
        shx_path=flowlines_prefix + ".shx",
    )

    gpkg.close()
    print(f"\nDone. Wrote {args.out} with layers: ricon_nodes, ricon_edges, nhd_flowlines")


if __name__ == "__main__":
    main()
