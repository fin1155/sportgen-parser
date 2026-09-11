"""Refresh deployment-file checksums without reading or bundling credentials."""
import hashlib
import json
from pathlib import Path

root = Path(__file__).resolve().parent.parent
path = root / "manifest.json"
manifest = json.loads(path.read_text())
files = set(manifest["files"])
files.update(str(file.relative_to(root)) for file in (root / "src").glob("*.R"))
files.update(str(file.relative_to(root)) for file in (root / "www").rglob("*") if file.is_file())
manifest["files"] = {
    name: {"checksum": hashlib.md5((root / name).read_bytes()).hexdigest()}
    for name in sorted(files)
}
path.write_text(json.dumps(manifest, ensure_ascii=False, indent=2) + "\n")
print(f"Updated {len(files)} deployment files")
