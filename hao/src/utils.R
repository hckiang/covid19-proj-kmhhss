rowexists = function (row, df) nrow(merge(row, df))>0
unique_row = function (df) df[!duplicated(df),]
agegrp_tostr = function (minag,maxag)
    paste(minag, maxag, sep='_')

make_eurostat_age_group_map = function (coarse_map) {
    minag = (0:18)*5
    maxag = minag + 4
    maxag[length(maxag)] = Inf
    usedmask = integer(length(minag))
    result = list()
    for (j in seq_len(nrow(coarse_map))) {
        included = list()
        for (i in seq_along(minag)) {
            if (coarse_map[j,'AgeGrp_min'] <= minag[i] && coarse_map[j,'AgeGrp_max'] >= maxag[i]) {
                included[[length(included)+1]] = agegrp_tostr(minag[i], maxag[i])
                usedmask[i] = 1
            }
        }
        result[[j]] = unlist(included)
    }
    if (prod(usedmask) != 1)
        warning('make_eurostat_age_group_map: Supplied age group does not span all possible ages')
    names(result) = mapply(agegrp_tostr, coarse_map[['AgeGrp_min']], coarse_map[['AgeGrp_max']])
    result
}
nationalise_NUTS = function (code)
    substr(code, start = 1, stop = 2)
agegrp_tonum = function (agegrp_str) {
    ans = sapply(strsplit(agegrp_str, '_')[[1]], as.numeric, simplify=T)
    names(ans) = c('AgeGrp_min','AgeGrp_max')
    ans
}
