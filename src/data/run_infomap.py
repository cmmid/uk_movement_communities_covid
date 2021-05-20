import sys
import numpy as np
import pandas as pd
from infomap import Infomap


def od_df(df):

    df = df.loc[:, ['start_quadkey', 'end_quadkey', 'n_crisis']]

    df = df.rename(columns={'start_quadkey': 'from',
                            'end_quadkey': 'to',
                            'n_crisis': 'weight'})

    return(df)


def communities_im(mob, silent=True):

    mob_od = od_df(mob).reset_index(drop=True)

    # quadkeys exceed C max values - map nodes to an int value
    unique_qks = np.unique(mob_od['from'].astype('int').tolist() + mob_od['to'].astype('int').tolist())

    qk_ref = dict(zip(unique_qks, range(0, len(unique_qks))))

    qk_ref_i = {v: k for k, v in qk_ref.items()}

    im_str = "--two-level --directed --seed 1000"

    if silent:
        im_str = im_str + " --silent"

    im = Infomap(im_str)

    for i in range(0, len(mob_od['to'])):
        row = mob_od.loc[i, :]

        im.addLink(qk_ref[int(row['from'])], qk_ref[int(row['to'])], row['weight'])

    im.run()

    clusters = []
    for node in im.tree:
        if node.is_leaf:
            clusters.append({'quadkey': qk_ref_i[node.node_id],
                             'cluster': node.module_id,
                             'flow': node.flow})

    res = pd.DataFrame(clusters)

    res["date"] = mob["date"].unique()[0]

    return(res)


def main():

    mob = pd.read_csv(sys.argv[1])

    mob_date = mob.groupby('date')
    mob_date = [mob_date.get_group(x) for x in mob_date.groups]

    im_res = []

    for data in mob_date:
        print("running " + np.unique(data["date"]))
        im_res.append(communities_im(data))

    im_res = pd.concat(im_res)

    im_res.to_csv(sys.argv[-1], index=False)


if __name__ == "__main__":

    main()
