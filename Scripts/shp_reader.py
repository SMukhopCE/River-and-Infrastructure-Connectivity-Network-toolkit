"""
shp_reader.py

Minimal, dependency-free, streaming reader for Esri Shapefiles. Reads
records one at a time (does not load the whole file into memory), which
matters for large NHDPlusV2 flowline shapefiles. Supports the shape types
needed here: Point (1) and PolyLine (3).
"""

import struct


def read_dbf_fields(dbf_path):
    """Return (field_defs, header_size, record_size, n_records).
    field_defs: list of (name, type_char, length, decimals)."""
    with open(dbf_path, "rb") as f:
        header = f.read(32)
        n_records = struct.unpack("<I", header[4:8])[0]
        header_size = struct.unpack("<H", header[8:10])[0]
        record_size = struct.unpack("<H", header[10:12])[0]

        n_fields = (header_size - 32 - 1) // 32
        field_defs = []
        for _ in range(n_fields):
            fd = f.read(32)
            name = fd[0:11].split(b"\x00")[0].decode("ascii", errors="replace")
            ftype = chr(fd[11])
            length = fd[16]
            dec = fd[17]
            field_defs.append((name, ftype, length, dec))
    return field_defs, header_size, record_size, n_records


def iter_dbf_records(dbf_path, field_defs, header_size, record_size, n_records,
                      encoding="utf-8"):
    """Yield one dict {field_name: python_value} per record, streaming."""
    with open(dbf_path, "rb") as f:
        f.seek(header_size)
        for _ in range(n_records):
            raw = f.read(record_size)
            if len(raw) < record_size:
                break
            if raw[0:1] == b"*":
                continue  # deleted record
            pos = 1
            rec = {}
            for name, ftype, length, dec in field_defs:
                chunk = raw[pos:pos + length]
                pos += length
                text = chunk.decode(encoding, errors="replace").strip()
                if ftype == "N":
                    if text == "":
                        val = None
                    elif dec and dec > 0:
                        try:
                            val = float(text)
                        except ValueError:
                            val = None
                    else:
                        try:
                            val = int(float(text))
                        except ValueError:
                            val = None
                elif ftype == "F":
                    try:
                        val = float(text) if text else None
                    except ValueError:
                        val = None
                else:
                    val = text
                rec[name] = val
            yield rec


def iter_shp_geometries(shp_path):
    """Yield one shape per record as a list of parts, each part a list of
    (x, y) tuples. Point shapes yield a single part with one point.
    Streaming: reads sequentially, does not load the whole file."""
    with open(shp_path, "rb") as f:
        header = f.read(100)
        shapetype = struct.unpack("<i", header[32:36])[0]

        while True:
            rec_header = f.read(8)
            if len(rec_header) < 8:
                break
            content_words = struct.unpack(">I", rec_header[4:8])[0]
            content_bytes = content_words * 2
            content = f.read(content_bytes)

            rtype = struct.unpack("<i", content[0:4])[0]

            if rtype == 0:
                yield []
                continue

            if rtype == 1:  # Point
                x, y = struct.unpack("<dd", content[4:20])
                yield [[(x, y)]]
                continue

            if rtype in (3, 5):  # PolyLine or Polygon (same physical layout)
                num_parts = struct.unpack("<i", content[36:40])[0]
                num_points = struct.unpack("<i", content[40:44])[0]
                parts_start = 44
                parts_idx = struct.unpack(
                    f"<{num_parts}i", content[parts_start:parts_start + 4 * num_parts]
                )
                points_start = parts_start + 4 * num_parts
                all_points = []
                for i in range(num_points):
                    off = points_start + i * 16
                    x, y = struct.unpack("<dd", content[off:off + 16])
                    all_points.append((x, y))

                parts = []
                for pi in range(num_parts):
                    start = parts_idx[pi]
                    end = parts_idx[pi + 1] if pi + 1 < num_parts else num_points
                    parts.append(all_points[start:end])
                yield parts
                continue

            raise NotImplementedError(f"Shape type {rtype} not supported by this reader")


def shapefile_record_count(shx_path):
    import os
    return (os.path.getsize(shx_path) - 100) // 8
