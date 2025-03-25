CREATE INDEX idx_benchmark_commit_id ON benchmark(commit_id);
CREATE INDEX idx_test_benchmark_id ON test(benchmark_id);
CREATE INDEX idx_factor_test_id ON factor(test_id);
CREATE INDEX idx_metric_test_id ON metric(test_id);
CREATE INDEX idx_map_branch_commit_branch_id ON map_branch_commit(branch_id);
CREATE INDEX idx_map_branch_commit_commit_id ON map_branch_commit(commit_id);
