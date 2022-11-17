namespace Client.DataTransfer;

public record CodeSubmission(string code, string test);

public record PullIdResponse(string id);

public record TestRunResult(string status, bool? success, string? output);