README
================

# Hemoglobin hypothesis

The Hemoglobin hypothesis is **Are POPs associated with hemoglobin A1c
in non-diabetics**. More details could be found in the Grant folder in
BOX drive.

## Dataset

1.  [nhance\_hemoglobin.sas7bdat](./nhance_hemoglobin.sas7bdat) is the
    original subset based Hemoglobin hypothesis.
2.  [nhance\_hemoglobin\_PCB.csv](./nhance_hemoglobin_PCB.csv) is a
    column subset based on the original data. The columns selected are
    PCBs and LBXGH (the
    response).
3.  [nhance\_hemoglobin\_PCB\_LC\_comp.csv](./nhance_hemoglobin_PCB_LC_comp.csv)
    is a cleaned dataset based on the original subset. The specific
    procedure are following:
      - Remove all observations if the response is missing
      - Remove all observations if all the PCBs and PCB\_LC (below limit
        of detection (LOD)) are missing
      - Replace all the
        ![\\sqrt{\\text{LOD}}](https://latex.codecogs.com/png.latex?%5Csqrt%7B%5Ctext%7BLOD%7D%7D
        "\\sqrt{\\text{LOD}}") with LOD (for data imputation)
