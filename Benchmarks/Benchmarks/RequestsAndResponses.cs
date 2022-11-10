namespace Benchmarks;

public record CodeSubmit(string code, string test);

public record PullIdResponse(string id);

public record TestRunResult(string status, bool? success, string? output);