"""
R.P.D. Ransom's Python Daemon
"""
import os
import pathlib
import subprocess
import asyncio
from subprocess import (
    run,
)
from typing import (
    List,
    Any,
)
from functools import (
    partial,
)

##BEGIN aliases
# modules
aio=asyncio
subps=subprocess
# classes
Ex = Exception
Exc = Exception
# defns
home=pathlib.Path.home
user_home_dir=pathlib.Path.home
##END

class Svc:
    repo_name: str
    build_cmds: List[str]
    run_cmd: List[str]
    daemon: Any

    def __init__(self, repo_name):
        self.repo_name = repo_name
        self.build_cmds = []
        self.run_cmd = 'false'
        self.daemon = None


def checkout_svc__(
        svc,
        repos_cache_dir,
        github_username,
        git_tag=None,
):
    repo_name = svc.repo_name
    git_url = (
        'https://github.com/' +
        github_username + '/' +
        repo_name)
    res = run([
        'git',
        'clone',
        git_url,
    ],
              cwd=repos_cache_dir,
              stdout=subps.PIPE,
              stderr=subps.PIPE,
              check=True,
    )
    if git_tag is None:
        return
    repo_cache_dir = os.path.join(
        repos_cache_dir,
        repo_name,
    )
    res = run([
        'git',
        'checkout',
        git_tag,
    ],
              cwd=repo_cache_dir,
              stdout=subps.PIPE,
              stderr=subps.PIPE,
              check=True,
    )


def make_shellout(
        cwd
        interactive=False,
):
    async def shellout(cmd):
        proc = aio.create_subprocess_shell(
            cmd,
            cwd=cwd,
        )
        stdout, stderr = await proc.communicate()
        res = await proc.wait()
        if res != 0:
            raise Exception()
        return {
            'res': res,
            'stdout': stdout,
            'stderr': stderr,
        }

    async def interactive_shellout(cmd):
        proc = aio.create_subprocess_shell(
            cmd,
            cwd=cwd,
        )
        return proc

    if interactive:
        return interactive_shellout
    return shellout



async def checkout_repo(
        repo_name,
        repos_cache_dir,
        github_username='ransomw',
        git_tag=None,
):
    git_url = (
        'https://github.com/' +
        github_username + '/' +
        repo_name)
    repo_cache_dir = os.path.join(
        repos_cache_dir,
        repo_name,
    )
    if not os.path.exists(repo_cache_dir):
        shellout = make_shellout(repos_cache_dir)
        await shellout(
            ['git', 'clone', git_url,],
        )
        del shellout
    shellout = make_shellout(repo_cache_dir)
    if git_tag is not None:
        await shellout(
            ['git', 'checkout', git_tag,])
    await shellout(['git', 'pull',])


async def build_and_run__contacts_store(
        repo_cache_dir,
):
    shellout = make_shellout(repo_cache_dir)
    await shellout(
        ['node', 'bin/initdb',])
    shellout = make_shellout(
        repo_cache_dir,
        interactive=True,)
    proc = await shellout(
        ['node', 'bin/run',
         '-p', str(port),])





def build_service(svc, repo_cache_dir):
    build_cmds = svc.build_cmds
    for build_cmd in build_cmds:
        if isinstance(build_cmd, (str,)):
            build_cmd = shlex.split(
                build_cmd)
        if not isinstance(build_cmd,
                          (list,)):
            raise Exception()
        res = run(build_cmd,
            cwd=repo_cache_dir,
            stdout=subps.PIPE,
            stderr=subps.PIPE,
            check=True,
            )


async def _read_stdio(daemon):
    while True:
        with daemon['rw_lock']:
            pass
        aio.sleep(1.5)


def run_service(svc, repo_cache_dir):
    run_cmd = svc.run_cmd
    if isinstance(run_cmd, (str,)):
        run_cmd = shlex.split(run_cmd)
    res = subps.Popen(run_cmd,
                      cwd=repo_cache_dir,
                      stdout=subps.PIPE,
                      stderr=subps.PIPE,
                      )
    daemon = {
        'popen': res,
        'stdout': [],
        'stderr': [],
        'rw_lock': aio.Lock(),
    }
    reader_coproc = _read_stdio(daemon)
    svc.dameon = dict(
        **daemon,
        reader=reader_coproc,
    )
    return svc


class Config:
    svcs: List[Svc]
    repos_cache = (
        os.path.join(
            user_home_dir(),
            '.cache',
            'rpd__github_cache',
        ))


###


