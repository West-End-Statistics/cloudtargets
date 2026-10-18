# cloudtargets

Run long-running [targets](https://docs.ropensci.org/targets/) pipelines on
ephemeral, on-demand cloud machines: provision, run detached, monitor, fetch
results, tear down. Time-to-live and idle guardrails protect against forgotten
machines.

Provider support is pluggable. The first backend is
[RunPod](https://www.runpod.io/) (CPU pods); others can be added.

**Status: early design, nothing works yet.**
