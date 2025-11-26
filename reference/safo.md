# Random generated dataset from the SAFO study

This dataset is a random generated dataset to reproduce the numbers
needed to generate the flowchart of the SAFO study. SAFO is an
open-label, multicenter, phase III–IV superiority randomized clinical
trial to assess whether cloxacillin plus fosfomycin administered for the
initial 7-days of therapy achieves better treatment success than
cloxacillin alone in hospitalized patients with MSSA bacteremia.

## Usage

``` r
data(safo)
```

## Format

A data frame with 925 rows and 21 columns

- id::

  Identifier of each patient. This information does not match the real
  data.

- inclusion_crit::

  The patient not met the inclusion criteria?

- exclusion_crit::

  The patient met the exclusion criteria?

- chronic_heart_failure::

  Exc1: Chronic heart failure?

- expected_death_24h::

  Exc2: Clinical status with expected death in \<24h?

- polymicrobial_bacteremia::

  Exc3: Polymicrobial bacteremia?

- conditions_affect_adhrence::

  Exc4: Conditions expected to affect adhrence to the protocol?

- susp_prosthetic_valve_endocard::

  Exc5: Suspicion of prosthetic valve endocarditis?

- severe_liver_cirrhosis::

  Exc6: Severe liver cirrhosis?

- acute_sars_cov2::

  Exc7: Acute SARS-CoV-2 infection?

- blactam_fosfomycin_hypersens::

  Exc8: Beta-lactam or fosfomycin hypersensitivity?

- other_clinical_trial::

  Exc9: Participation in another clinical trial?

- pregnancy_or_breastfeeding::

  Exc10: Pregnancy or breastfeeding?

- previous_participation::

  Exc11: Previous participation in the SAFO trial?

- myasthenia_gravis::

  Exc12: Myasthenia gravis?

- decline_part::

  The patient declined to participate?

- group::

  Randomized treatment received: cloxacilin alone / cloxacilin plus
  fosfomycin

- itt::

  The patient belongs to the intention to treat (ITT) group?

- reason_itt::

  Reason for exclusion from the ITT group.

- pp::

  The patient belongs to the per protocol (PP) group?

- reason_pp::

  Reason for exclusion from the PP group.

## References

Grillo, S., Pujol, M., Miró, J.M. et al. Cloxacillin plus fosfomycin
versus cloxacillin alone for methicillin-susceptible Staphylococcus
aureus bacteremia: a randomized trial. Nat Med 29, 2518–2525 (2023).
https://doi.org/10.1038/s41591-023-02569-0
