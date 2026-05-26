# Design Validation Summary

- validation_dir: `/project`
- scenarios: `scenario1`, `scenario2`

## Parameter Summary

|scenario|method|dataset|n_patients|fit_ok|KA|CL|V|omega2_cl|omega_cl_v|omega2_v|rho_cl_v|sigma2_prop|objective_value|message|
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
|scenario1|Centralized|ALL|100|TRUE|9.549|8.784|19.44|0.1049|0.0639|0.04044|0.981|0.129|2221|CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH|
|scenario1|Local-Site1|Site1|40|TRUE|8.136|9.062|18.18|0.1154|0.07637|0.05091|0.9965|0.1056|890.9|CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH|
|scenario1|Local-Site2|Site2|30|TRUE|205.3|7.745|18.11|0.03591|0.0003943|0.0008375|0.07191|0.1639|691.5|CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH|
|scenario1|Local-Site3|Site3|30|TRUE|193.8|8.51|18.39|0.05977|0.01827|0.03015|0.4304|0.1147|633.5|CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH|
|scenario1|Federated|ALL||TRUE|9.635|8.763|19.35|0.1055|0.06124|0.03583|0.9961|0.1279|2221|CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH|
|scenario2|Centralized|ALL|100|TRUE|9.549|8.784|19.44|0.1049|0.0639|0.04044|0.981|0.129|2221|CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH|
|scenario2|Local-Site1|Site1|40|TRUE|6.062|8.516|17.27|0.09335|0.06096|0.09499|0.6473|0.09556|878.7|CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH|
|scenario2|Local-Site2|Site2|30|TRUE|12.88|8.877|18.34|0.04971|0.0013|0.003171|0.1036|0.1266|664|CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH|
|scenario2|Local-Site3|Site3|30|TRUE|11.25|9.133|21.53|0.1025|0.04845|0.02372|0.9826|0.1236|675.9|CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH|
|scenario2|Federated|ALL||TRUE|9.693|8.764|19.36|0.1057|0.0611|0.03545|0.998|0.1279|2221|CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH|

## Site And Design Distribution

|scenario|Site|design|n_subjects|
|---|---|---|---|
|scenario1|Site1|D1|40|
|scenario1|Site2|D2|30|
|scenario1|Site3|D3|30|
|scenario2|Site1|D1|17|
|scenario2|Site1|D2| 9|
|scenario2|Site1|D3|14|
|scenario2|Site2|D1|13|
|scenario2|Site2|D2| 8|
|scenario2|Site2|D3| 9|
|scenario2|Site3|D1|10|
|scenario2|Site3|D2|13|
|scenario2|Site3|D3| 7|

