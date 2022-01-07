import gzip
import os
import os.path as pth
import shutil
import time
from gzip import GzipFile
from subprocess import Popen
from tempfile import gettempdir
import requests


def view_pdb(db_id: str):
    """
    # todo: scrape wikipedia page to pdb code
    #    https://en.wikipedia.org/wiki/Muscarinic_acetylcholine_receptor_M4
    # todo: search wikipedia page (NLP+force(?))
    #    to receptors (pages w/ pdb codes)
    """
    garlic_exec = "garlic"
    cache_dir = pth.join(gettempdir(), "view_pdb_cache_dir")
    if not pth.exists(cache_dir):
        os.mkdir(cache_dir)
    download_url_base = "https://files.rcsb.org/download/"
    pdb_file_path = pth.join(
        cache_dir,
        "{db_id}.pdb".format(
            db_id=db_id,
        ),
    )
    pdb_archive_path = pth.join(
        cache_dir,
        "{db_id}.pdb.gz".format(
            db_id=db_id,
        ),
    )

    #    breakpoint()

    pdb_file_url = requests.compat.urljoin(
        download_url_base,
        "{db_id}.pdb".format(
            db_id=db_id,
        ),
    )
    pdb_archive_url = requests.compat.urljoin(
        download_url_base,
        "{db_id}.pdb.gz".format(
            db_id=db_id,
        ),
    )

    if not pth.exists(pdb_file_path) and not pth.exists(pdb_archive_path):
        res: requests.models.Response = requests.get(pdb_archive_url, stream=True)
        assert res.status_code == 200
        chunks = res.iter_content(
            # read data in "whatever size the chunks are recv'd"
            # todo: extract personal dsl fn, documenting the "whatever"
            chunk_size=None,
        )
        with open(pdb_archive_path, "wb") as f:
            for chunk in chunks:
                f.write(chunk)

    if not pth.exists(pdb_file_path):
        with gzip.open(pdb_archive_path) as f_gz, open(pdb_file_path, "wb") as f:
            shutil.copyfileobj(f_gz, f)

    #    breakpoint()
    proc = Popen([garlic_exec, pdb_file_path])
    # todo: workaround garlic numeric keypad requirement
    #   - forward keys from python terminal
    #   - wrap window (X11 or other layer) to access keys
    #          while view selected.
    try:
        print("interrupt Ctl-c to exit")
        while True:
            time.sleep(1024)
    except KeyboardInterrupt:
        pass
    finally:
        proc.terminate()
        print("waiting on garlic exit")
        proc.wait()

    return

    # attempts at optimization
    with open() as f:
        with GzipFile(
            f,
            # default 'rb'.  no text mode opt here.  gzip.open() only
            mode="rb",
        ) as gf:
            gf.read1()
