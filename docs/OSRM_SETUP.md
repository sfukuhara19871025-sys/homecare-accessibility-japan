# OSRM setup

The study used a local OSRM backend rather than a hosted routing API.

## Recorded study configuration

- Backend: OSRM `v6.0.0`
- Docker image tag: `ghcr.io/project-osrm/osrm-backend:latest`
- Recorded image digest: `sha256:729461bcc9ae9e6aafa92c0f93db9b060a32e85d5e72092c01ae4a4a9f1eb564`
- OSM extract provider: Geofabrik Japan extract
- Recorded OSM snapshot timestamp: `2026-05-11T20:20:52Z`
- Algorithm: MLD
- Profile: car
- Host endpoint used by the R analysis: `http://127.0.0.1:5001/`
- `--max-table-size 5000`

## Example Docker commands

Put the Japan `.osm.pbf` extract in a local directory such as `osrm_data/`, then run:

```bash
IMAGE="ghcr.io/project-osrm/osrm-backend@sha256:729461bcc9ae9e6aafa92c0f93db9b060a32e85d5e72092c01ae4a4a9f1eb564"

docker run --rm -t -v "$PWD/osrm_data:/data" "$IMAGE" \
  osrm-extract -p /opt/car.lua /data/japan-latest.osm.pbf

docker run --rm -t -v "$PWD/osrm_data:/data" "$IMAGE" \
  osrm-partition /data/japan-latest.osrm

docker run --rm -t -v "$PWD/osrm_data:/data" "$IMAGE" \
  osrm-customize /data/japan-latest.osrm

docker run --rm -t -i --name osrm-japan \
  -p 5001:5000 \
  -v "$PWD/osrm_data:/data" \
  "$IMAGE" \
  osrm-routed --algorithm mld --max-table-size 5000 /data/japan-latest.osrm
```

The exact filename of the `.osm.pbf` and resulting `.osrm` dataset may differ. Use the same road-network snapshot/profile for the main routing run and the technical QC/same-facility rerouting.

## Ferry routing

The study used the standard OSRM car profile. Ferry connections represented as routable ways/routes in the OpenStreetMap extract can therefore contribute to a returned route. The analysis did **not** model ferry timetables, waiting time, service frequency, cancellations, or seasonal operation; travel times involving ferry-connected areas should be interpreted as standardized network-routing indicators rather than scheduled journey times.

## Chunking

The national routing script sends OSRM Table requests using:

- up to 1,000 facility sources per source block;
- up to 4,000 centroid destinations per destination block;
- the constraint `sources + destinations <= 5000`.

The all-pairs long table is not stored. For each centroid, the code sequentially updates only the minimum duration and corresponding time-minimising facility.
