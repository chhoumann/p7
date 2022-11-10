namespace Benchmarks;

internal record CodeSubmit(string code, string test);

internal record PullIdResponse(string id);

internal record TestRunResult(string status, bool? success, string? output);