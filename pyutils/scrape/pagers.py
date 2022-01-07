""""""

_FS_DIR_ASYNC_DIRCACHE = {}

async def fs_dir_async(dirname):
    cache = _FS_DIR_ASYNC_DIRCACHE
    if dirname not in cache:
        contents = _
        cache[dirname] = {
            'dirs': [],
            'count': 0,
            'curr_idx': -1,
        }
        for inode in contents:
            if isdir(inode):
                cache[dirname]['dirs'].append(inode)
                cache[dirname]['count'] += 1
                if cache[dirname]['curr_idx'] == -1:
                    cache[dirname]['curr_idx'] = 0
    dirinfo = cache[dirname]
    if dirinfo['curr_idx'] == -1:
        del cache[dirname]
        return

    curr_idx = dirinfo['curr_idx']
    yield dirinfo['dirs'][curr_idx]
