# Monitoring assets

Apply `prometheus-rules.yaml` with the Prometheus Operator. The application exposes
`/actuator/prometheus`; scrape it with the annotations already present in
`deployment.yaml`.

Recommended dashboards:

- chat p95 latency and error rate by status
- retrieval empty rate and p95 latency
- citation invalid and quota exceeded counters
- tool calls by tool/status and approval latency
- outbox/queue lag and DLQ size
- SSE disconnect and first-token latency
