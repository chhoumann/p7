using System.Net.Http.Headers;
using System.Net.Http.Json;
using System.Text.Json;
using dotenv.net.Utilities;

namespace Benchmarks;

public class CodeRunnerQueueClient
{
    private readonly HttpClient _client = new();

    private static readonly Uri _postProblemUrl = CreateUri("haskell/submit");
    private static readonly Uri _getResultUrl = CreateUri("haskell/getResult/");

    public CodeRunnerQueueClient()
    {
        _client.DefaultRequestHeaders.Accept.Add(new MediaTypeWithQualityHeaderValue("application/json"));
        _client.DefaultRequestHeaders.Accept.Clear();
    }

    private static Uri CreateUri(string endpointPath)
    {
        UriBuilder v = new()
        {
            Scheme = "http",
            Host = EnvReader.GetStringValue("HOST"),
            Port = EnvReader.GetIntValue("PORT"),
            Path = endpointPath
        };
        
        return v.Uri;
    }

    public Task<HttpResponseMessage> Post(CodeSubmission toSend)
    {
        return _client.PostAsJsonAsync(_postProblemUrl, toSend);
    }

    private Task<HttpResponseMessage> GetByToken(string pollToken)
    {
        return _client.GetAsync($"{_getResultUrl}{pollToken}");
    }

    public async Task<PullIdResponse?> PostCodeRequest(string code, string test)
    {
        CodeSubmission toSend = new(code, test);
        
        return await JsonSerializer.DeserializeAsync<PullIdResponse>(await Post(toSend).Result.Content.ReadAsStreamAsync());
    }

    private async Task<TestRunResult?> GetTestRunResult(string id)
    {
        return await JsonSerializer.DeserializeAsync<TestRunResult>(await GetByToken(id).Result.Content.ReadAsStreamAsync());
    }

    private async Task<TestRunResult> PullUntilResponseReady(TimeSpan timeBetweenPulls, string id)
    {
        PeriodicTimer periodicTimer = new(timeBetweenPulls);
        
        while (await periodicTimer.WaitForNextTickAsync())
        {
            TestRunResult? codeRunResult = await GetTestRunResult(id);

            if (codeRunResult?.success != null)
            {
                return codeRunResult;
            }
        }

        throw new TimeoutException("Did not get a response from server.");
    }

    public Task<HttpResponseMessage>[] PostCodeRequest(string code, string test, int numberOfSubmissions)
    {
        CodeSubmission submission = new(code, test);
        
        return Enumerable.Repeat(Post(submission), numberOfSubmissions).ToArray();
    }

    public Task<TestRunResult> PostAndGetHaskellResultTask(string code, string test, TimeSpan timeBetweenPulls)
    {
        Task<PullIdResponse?> postCodeRequest = PostCodeRequest(code, test);
        
        PullIdResponse? tokenResponse = postCodeRequest.Result;
        string id = tokenResponse?.id is null
            ? throw new InvalidOperationException("Token string was null!")
            : tokenResponse.id;

        return PullUntilResponseReady(timeBetweenPulls, id);
    }
}