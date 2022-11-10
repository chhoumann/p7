using System.Net.Http.Headers;
using System.Net.Http.Json;
using System.Text.Json;
using dotenv.net.Utilities;

namespace Benchmarks;

internal class CodeRunnerQueueClient
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

    private Task<HttpResponseMessage> Post(CodeSubmit toSend) => _client.PostAsJsonAsync(_postProblemUrl, toSend);
    private Task<HttpResponseMessage> GetByToken(string pollToken) => _client.GetAsync(_getResultUrl + pollToken);

    public async Task<PullIdResponse?> PostCodeRequest(string code, string test)
    {
        return await JsonSerializer.DeserializeAsync<PullIdResponse>(
            await Post(new CodeSubmit(code, test)).Result.Content.ReadAsStreamAsync());
    }

    private async Task<TestRunResult?> GetTestRunResult(string id)
    {
        return await JsonSerializer.DeserializeAsync<TestRunResult>
            (await GetByToken(id).Result.Content.ReadAsStreamAsync());
    }

    private async Task<TestRunResult> PullUntilResponseReady(TimeSpan timeBetweenPulls, string id)
    {
        PeriodicTimer periodicTimer = new PeriodicTimer(timeBetweenPulls);
        while (await periodicTimer.WaitForNextTickAsync())
        {
            TestRunResult? codeRunResult = await this.GetTestRunResult(id);
            if (codeRunResult is not null && codeRunResult.success.HasValue)
                return codeRunResult;
        }

        throw new TimeoutException("Did not get an answer from server.");
    }

    public Task[] PostCodeRequest(string code, string test, int numberOfSubmissions)
    {
        Task[] taskList = new Task[numberOfSubmissions];
        for (int i = 0; i < numberOfSubmissions; i++)
            taskList[i] = Post(new CodeSubmit(code, test));
        return taskList;
    }


    public Func<Task<Task<TestRunResult>>> CreatePostAndGetHaskellResult(string code, string test, TimeSpan timeBetweenPulls)
    {
        return () => PostAndGetHaskellResultTask(code, test, timeBetweenPulls);
    }

    public Task<Task<TestRunResult>> PostAndGetHaskellResultTask(string code, string test, TimeSpan timeBetweenPulls)
    {
        return PostCodeRequest(code, test)
            .ContinueWith<Task<TestRunResult>>(async (tokenResponseTask) =>
            {
                PullIdResponse? tokenResponse = await tokenResponseTask;
                string id = tokenResponse?.id is null
                    ? throw new InvalidOperationException("Token string was null!")
                    : tokenResponse.id;

                return await PullUntilResponseReady(timeBetweenPulls, id);
            });
    }
}