namespace CodeRunnerClients.DataTransfer;

public record CodeSubmission(string code, string test);

public record PollIdResponse(string id);

public record TestRunResult(string status, bool? success, string? output);