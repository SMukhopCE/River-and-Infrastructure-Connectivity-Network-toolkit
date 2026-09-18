"""
ricon_to_geospatial.py

Converts RICON toolkit CSV outputs for the Colorado River Basin (CRB) --
node tables (nid_df.csv, sites_df.csv) and Edge_List.csv -- into
geospatial files (Shapefile / GeoPackage) usable in any GIS.

Reference:
  Mukhopadhyay, S., Sankarasubramanian, A. & Awasthi, C. "Developing the
  hydrological dependency structure between streamgage and reservoir
  networks." Sci Data 7, 319 (2020). https://doi.org/10.1038/s41597-020-00660-6
  Dataset: https://doi.org/10.6084/m9.figshare.11849157

-----------------------------------------------------------------------
SCOPE OF THIS VERSION
-----------------------------------------------------------------------
Per project scoping: CRB dataset only, edges rendered as STRAIGHT LINES
between node coordinates. The code is structured so that a "true river
channel" edge geometry mode can be added later without touching the
node-building or file-writing logic -- see build_edges_true_channel()
below, which is currently a stub with the design notes for that future
work.

-----------------------------------------------------------------------
INPUTS
-----------------------------------------------------------------------
  nid_df.csv     : dam nodes  (IDS, NAME, LONGITUDE, LATITUDE, COMID, ...)
  sites_df.csv   : gage nodes (same schema)
  Edge_List.csv  : FROM_NODE, TO_NODE, EDGE_LENGTHKM, FROM_NODETYPE,
                   TO_NODETYPE, FROM_COMID, TO_COMID, FROM_LEVELPATHI,
                   TO_LEVELPATHI, FROM_NODE_NAME, TO_NODE_NAME

-----------------------------------------------------------------------
OUTPUTS (written to --outdir)
-----------------------------------------------------------------------
  ricon_nodes.shp / .gpkg   : point layer, all dams + gages combined
  ricon_edges.shp / .gpkg   : line layer, straight edges FROM_NODE -> TO_NODE

-----------------------------------------------------------------------
USAGE
-----------------------------------------------------------------------
  pip install geopandas shapely pandas

  python ricon_to_geospatial.py \
      --nid nid_df.csv --sites sites_df.csv --edges Edge_List.csv \
      --outdir ./ricon_gis

  # sanity check against the paper's validation case (Glen Canyon -> Hoover,
  # should sum to ~592.9 km across the edges on that path):
  python ricon_to_geospatial.py ... --validate-path AZ10307 NV10122
"""

import argparse
import os
import sys

import pandas as pd
import geopandas as gpd
from shapely.geometry import Point, LineString


# =========================================================================
# Node layer
# =========================================================================

def load_nodes(nid_path, sites_path):
    """Load and combine the dam (nid_df) and gage (sites_df) node tables
    into a single point GeoDataFrame in EPSG:4326."""
    frames = []
    for path, default_type in [(nid_path, "dam"), (sites_path, "gage")]:
        if path is None:
            continue
        df = pd.read_csv(path, dtype={"IDS": str})
        if "POINTTYPE" not in df.columns:
            df["POINTTYPE"] = default_type
        frames.append(df)

    if not frames:
        raise ValueError("No node files provided (need --nid and/or --sites).")

    nodes = pd.concat(frames, ignore_index=True)

    required = {"IDS", "LONGITUDE", "LATITUDE"}
    missing = required - set(nodes.columns)
    if missing:
        raise ValueError(f"Node file(s) missing required columns: {missing}")

    before = len(nodes)
    nodes = nodes.dropna(subset=["LONGITUDE", "LATITUDE"]).copy()
    if len(nodes) < before:
        print(
            f"Warning: dropped {before - len(nodes)} node(s) with missing "
            "coordinates.",
            file=sys.stderr,
        )

    dupe_ids = nodes["IDS"][nodes["IDS"].duplicated()].unique()
    if len(dupe_ids):
        print(
            f"Warning: {len(dupe_ids)} duplicate IDS value(s) found in node "
            f"tables (e.g. {list(dupe_ids[:5])}). Edge joins use the first "
            "match.",
            file=sys.stderr,
        )

    nodes["geometry"] = [
        Point(xy) for xy in zip(nodes["LONGITUDE"], nodes["LATITUDE"])
    ]
    gdf = gpd.GeoDataFrame(nodes, geometry="geometry", crs="EPSG:4326")
    return gdf


# =========================================================================
# Edge layer -- straight-line mode (current scope)
# =========================================================================

def build_edges_straight(edge_path, nodes_gdf):
    """Join Edge_List.csv to node coordinates and build straight-line
    LineString edges between FROM_NODE and TO_NODE."""
    edges = pd.read_csv(edge_path, dtype={"FROM_NODE": str, "TO_NODE": str})

    required = {"FROM_NODE", "TO_NODE"}
    missing = required - set(edges.columns)
    if missing:
        raise ValueError(f"Edge_List.csv missing required columns: {missing}")

    coord_lookup = (
        nodes_gdf.drop_duplicates(subset="IDS")
        .set_index("IDS")[["LONGITUDE", "LATITUDE"]]
    )

    def make_line(row):
        try:
            fx, fy = coord_lookup.loc[row["FROM_NODE"]]
            tx, ty = coord_lookup.loc[row["TO_NODE"]]
        except KeyError:
            return None
        return LineString([(fx, fy), (tx, ty)])

    edges["geometry"] = edges.apply(make_line, axis=1)

    n_missing = edges["geometry"].isna().sum()
    if n_missing:
        print(
            f"Warning: {n_missing} of {len(edges)} edges could not be "
            "matched to node coordinates (FROM_NODE/TO_NODE not found in "
            "node table) and will be dropped.",
            file=sys.stderr,
        )
    edges = edges.dropna(subset=["geometry"]).copy()

    return gpd.GeoDataFrame(edges, geometry="geometry", crs="EPSG:4326")


# =========================================================================
# Edge layer -- true river-channel mode (FUTURE WORK, not built yet)
# =========================================================================

def build_edges_true_channel(edge_path, nodes_gdf, nhdplus_flowline_path=None):
    """
    PLACEHOLDER for a future edge mode that traces the actual NHDPlusV2
    river channel between each edge's endpoints, instead of a straight
    line.

    Design notes for when this gets built:
      1. Requires the NHDPlusV2 flowline GEOMETRY for VPU 14 and 15
         (the NHDFlowline shapefile itself -- NOT included in the RICON
         CSVs; NHD_data.RData only carries attribute tables such as
         PlusFlowlineVAA and PlusFlow, no geometry).
      2. For each edge, walk the FROM_COMID -> TO_COMID path using the
         PlusFlow connectivity table (same "move downstream" logic the
         RICON R package already implements in create_network()/
         find_distance()), collecting every intermediate COMID.
      3. Dissolve/merge the flowline geometries for that ordered COMID
         list into a single LineString per edge (watch for direction --
         NHDFlowline geometries are not guaranteed to be digitized
         upstream-to-downstream, so segments may need reversing before
         merging).
      4. Cross-check total length against EDGE_LENGTHKM from Edge_List.csv
         as a QA step -- they should match closely since that column was
         itself computed by summing flowline lengths along the same path.

    This function intentionally raises NotImplementedError for now so the
    CLI fails loudly rather than silently falling back to straight lines
    if someone requests this mode before it exists.
    """
    raise NotImplementedError(
        "True-channel edge geometry is not implemented yet -- this is a "
        "planned extension. Requires NHDPlusV2 flowline geometry for VPU "
        "14/15 (see docstring). Use --edge-geometry straight for now."
    )


# =========================================================================
# Validation helper (paper's Glen Canyon -> Hoover check)
# =========================================================================

def validate_path_length(edges_df, from_node, to_node):
    """Rough sanity check: sum EDGE_LENGTHKM along a directed chain of
    FROM_NODE->TO_NODE hops starting at from_node. Prints what it finds;
    this is a lightweight spot-check, not a full path-finder."""
    if "EDGE_LENGTHKM" not in edges_df.columns:
        print("EDGE_LENGTHKM column not present; skipping validation.")
        return

    current = from_node
    total = 0.0
    hops = 0
    visited = set()
    while current != to_node and current not in visited:
        visited.add(current)
        next_row = edges_df.loc[edges_df["FROM_NODE"] == current]
        if next_row.empty:
            print(f"Validation: no outgoing edge from {current}; path search stopped.")
            return
        row = next_row.iloc[0]
        total += float(row["EDGE_LENGTHKM"])
        current = row["TO_NODE"]
        hops += 1
        if hops > 500:
            print("Validation: exceeded 500 hops, stopping (possible loop or bad match).")
            return

    if current == to_node:
        print(f"Validation: {from_node} -> {to_node} = {total:.1f} km over {hops} edges.")
    else:
        print(f"Validation: could not reach {to_node} from {from_node} via FROM_NODE chain.")


# =========================================================================
# Output writing
# =========================================================================

def write_layer(gdf, outdir, name, fmt):
    os.makedirs(outdir, exist_ok=True)
    if fmt in ("shp", "both"):
        shp_path = os.path.join(outdir, f"{name}.shp")
        gdf.to_file(shp_path, driver="ESRI Shapefile")
        print(f"Wrote {shp_path}")
    if fmt in ("gpkg", "both"):
        gpkg_path = os.path.join(outdir, f"{name}.gpkg")
        gdf.to_file(gpkg_path, driver="GPKG", layer=name)
        print(f"Wrote {gpkg_path}")


# =========================================================================
# CLI
# =========================================================================

def main():
    ap = argparse.ArgumentParser(description="Convert RICON CRB CSVs to shapefile/GeoPackage")
    ap.add_argument("--nid", help="Path to nid_df.csv (dam nodes)")
    ap.add_argument("--sites", help="Path to sites_df.csv (gage nodes)")
    ap.add_argument("--edges", help="Path to Edge_List.csv")
    ap.add_argument("--outdir", default="./ricon_gis", help="Output directory")
    ap.add_argument(
        "--format", choices=["shp", "gpkg", "both"], default="shp",
        help="Output format(s) to write",
    )
    ap.add_argument(
        "--edge-geometry", choices=["straight", "true_channel"], default="straight",
        help="Edge geometry mode. 'true_channel' is not implemented yet "
             "(see build_edges_true_channel docstring).",
    )
    ap.add_argument(
        "--validate-path", nargs=2, metavar=("FROM_NODE", "TO_NODE"),
        help="Optional: sanity-check summed EDGE_LENGTHKM between two node "
             "IDS, e.g. --validate-path AZ10307 NV10122",
    )
    args = ap.parse_args()

    if not args.nid and not args.sites:
        ap.error("Provide at least one of --nid or --sites")

    nodes_gdf = load_nodes(args.nid, args.sites)
    write_layer(nodes_gdf, args.outdir, "ricon_nodes", args.format)

    if args.edges:
        if args.edge_geometry == "straight":
            edges_gdf = build_edges_straight(args.edges, nodes_gdf)
        else:
            edges_gdf = build_edges_true_channel(args.edges, nodes_gdf)
        write_layer(edges_gdf, args.outdir, "ricon_edges", args.format)

        if args.validate_path:
            raw_edges = pd.read_csv(args.edges, dtype={"FROM_NODE": str, "TO_NODE": str})
            validate_path_length(raw_edges, args.validate_path[0], args.validate_path[1])

    print("Done.")


if __name__ == "__main__":
    main()
