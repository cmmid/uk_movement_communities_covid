import uuid
import random
import sys
import pandas as pd


def apply_label_mapping(im):

    im['quadkey'] = ['{0:012d}'.format(n) for n in im['quadkey']]

    im = im.groupby('date')
    im = [im.get_group(x) for x in im.groups]

    # apply method to each timestep
    for i, date in enumerate(im):

        if i > 0:
            t0 = im[i - 1]
            t1 = im[i]

            if i == 1:
                t0 = init_mapping(t0)

            t0_cluster_ref = t0.groupby('cluster')['quadkey'].apply(list).to_dict()
            t1_cluster_ref = t1.groupby('cluster')['quadkey'].apply(list).to_dict()

            # need t0_modules and t1_module variables. Dict with list of nodes
            cluster_maps = compute_shared_nodes(t0, t1, t0_cluster_ref, t1_cluster_ref)

            cluster_maps = arbitrate_duplicate_assignment(cluster_maps, t0_cluster_ref, t1_cluster_ref)

            im[i] = map_clusters(cluster_maps, t1)

        # if i > 5:
        #    break
        print("{}% Done.".format(round((i / len(im)) * 100, 2)))

    im_mapped = pd.concat(im)

    assert sum([len(str(x)) < 10 for x in im_mapped['cluster']]) == 0

    return im_mapped


def init_mapping(t0):
    # initialize - map labels to a unique id
    clusters = list(t0['cluster'].unique())

    clusters = dict(zip(clusters, [str(uuid.uuid4()) for i in range(0, len(clusters))]))

    t0.loc[:, 'cluster'] = [clusters[c] for c in t0['cluster']]

    return(t0)


def compute_shared_nodes(t0, t1, t0_cluster_ref, t1_cluster_ref):
    '''
    For each cluster in time 0, find the cluster in time 1 with the most shared nodes

    Parameters
    ----------
    t0 : TYPE
        DESCRIPTION.
    t1 : TYPE
        DESCRIPTION.

    Returns
    -------
    None.

    '''

    cluster_maps = []

    clust_ids = t0['cluster'].unique()

    label_collision = []
    no_label_collision = []

    for t0_cluster in clust_ids:

        # which nodes are in this t0 module
        t0_nodes = t0.loc[t0['cluster'] == t0_cluster, 'quadkey'].tolist()

        id_intersection = {}
        for t1_cluster in t1['cluster'].unique():

            t1_nodes = t1_cluster_ref[t1_cluster]

            id_intersection[t1_cluster] = len(set(t0_nodes).intersection(set(t1_nodes)))

        # what to do if a module disappears
        if max(id_intersection.values()) == 0:
            pass
        else:
            cluster_map = {'t0': t0_cluster, 't1':list({k: v for k, v in id_intersection.items() if v == max(id_intersection.values())}.keys())}

            # if this is found to error - will need some work
            # assert len(cluster_map['t1']) == 1

            # this is where the larger module should win out
            if len(cluster_map['t1']) > 1:
                label_collision.append(len(t0_cluster_ref[t0_cluster]))
            else:
                no_label_collision.append(len(t0_cluster_ref[t0_cluster]))

            cluster_map['t1'] = cluster_map['t1'][random.randint(0, len(cluster_map['t1']) - 1)]

            cluster_maps.append(cluster_map)

    assert len(cluster_maps) <= len(t0['cluster'].unique())

    return(cluster_maps)


def assign_new_module_id(new_modules, c):

    try:

        return(new_modules[c])

    except Exception:

        return(c)


def arbitrate_duplicate_assignment(cluster_maps, t0_cluster_ref, t1_cluster_ref):
    '''

    Arbitrate the conflict of >1 t0 module wanting to assign label to a given cluster in t1

    Parameters
    ----------
    cluster_maps : TYPE
        DESCRIPTION.
    t0_cluster_ref : TYPE
        DESCRIPTION.
    t1_cluster_ref : TYPE
        DESCRIPTION.

    Returns
    -------
    None.

    '''

    # check here that the keys of cluster_maps are unique
    c_map_df = pd.DataFrame(cluster_maps)

    conflict_df = c_map_df.groupby('t1').count().reset_index()

    # where >1 module in t0 wants to give its label to a module in t1
    conflict_df = conflict_df.loc[conflict_df['t0'] > 1, :]

    if conflict_df.shape[0] == 0:
        return(cluster_maps)
    else:

        conflict_df = c_map_df.loc[[x in conflict_df['t1'].tolist() for x in c_map_df['t1']], :]

        no_conflict_df = c_map_df.loc[[i not in conflict_df.index for i in c_map_df.index], :]

        assert conflict_df.shape[0] + no_conflict_df.shape[0] == c_map_df.shape[0]

        conflict_df = conflict_df.groupby('t1')
        conflict_df = [conflict_df.get_group(x) for x in conflict_df.groups]

        resolved_conflicts = []
        for cdf in conflict_df:

            # t0 sizes
            t0_sizes = [len(t0_cluster_ref[x]) for x in cdf['t0']]

            # t1 size
            t1_size = len(t1_cluster_ref[cdf['t1'].values[0]])

            # if this is >1, assignment should be random
            closest_index = min(range(len(t0_sizes)), key=lambda i: abs(t0_sizes[i]-t1_size))

            cdf = cdf.reset_index(drop = True).loc[closest_index, :].to_dict()

            resolved_conflicts.append(cdf)

        resolved_conflicts = pd.DataFrame(resolved_conflicts)

        assert resolved_conflicts.shape[0] == len(conflict_df)

        resolved_conflicts = pd.concat([resolved_conflicts, no_conflict_df]).reset_index(drop = True)

        resolved_conflicts = resolved_conflicts.to_dict('records')

        assert len(resolved_conflicts) < len(cluster_maps)

        return(resolved_conflicts)


def map_clusters(cluster_maps, t1):
    '''
    Map labels to modules in time t1, resolve conflicts, and assign uuids to new clusters

    Parameters
    ----------
    cluster_maps : TYPE
        DESCRIPTION.
    t1 : TYPE
        DESCRIPTION.

    Raises
    ------
    ValueError
        DESCRIPTION.

    Returns
    -------
    None.

    '''

    assigned = []

    for cluster_map in cluster_maps:

        # check if multiple modules are claiming the same id -
        # now this is working - multiple modules want to give their label to an already assigned cluster
        # need to mediate this dispute. The module closest in size passes its label, else assignment is random
        # could also be done witha weight of which module has existed longer
        assert cluster_map['t1'] not in assigned

        t1.loc[t1['cluster'] == cluster_map['t1'], 'cluster'] = cluster_map['t0']

        assigned.append(cluster_map['t1'])

    new_modules = t1.loc[[len(str(x)) < 10 for x in t1['cluster']], 'cluster'].tolist()

    new_modules = dict(zip(new_modules, [str(uuid.uuid4()) for i in range(0, len(new_modules))]))

    t1.loc[:, 'cluster'] = [assign_new_module_id(new_modules, c) for c in t1['cluster']]

    try:

        assert sum([len(str(x)) < 10 for x in t1['cluster']]) == 0

    except Exception:

        print(t1.loc[[len(str(x)) < 10 for x in t1['cluster']], 'cluster'].tolist())

        raise ValueError()

    return(t1)


def main():

    data = pd.read_csv(sys.argv[1])

    lm = apply_label_mapping(data)

    lm.to_csv(sys.argv[2], index=False)

    print('Finished.')


if __name__ == "__main__":

    main()
